;;; fzf-native-differential-generator.el --- Structured differential cases -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Duc Dang
;; Author: Duc Dang <me@dangduc.com>
;; Assisted-by: Codex:gpt-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of fzf-native.

;;; Commentary:

;; Generate deterministic, parsed-query cases for comparison with upstream fzf.
;; The generator constructs at least one matching candidate before it adds near
;; misses, ranking variants, and duplicate occurrences.  Stable numeric
;; identities let the comparison preserve multiplicity.

;;; Code:

(require 'cl-lib)

(cl-defstruct fzf-native-differential-rng state)

(cl-defstruct fzf-native-differential-term
  kind inverse literal)

(cl-defstruct fzf-native-differential-query
  sets case-mode fuzzy normalize forward)

(cl-defstruct fzf-native-differential-candidate
  id text role)

(cl-defstruct fzf-native-differential-case
  seed serial profile query rendered-query candidates dimensions comparison)

(defconst fzf-native-differential--ascii-atoms
  ["a" "ab" "alpha" "Beta" "fzf" "main" "src" "test42" "xYz"])

(defconst fzf-native-differential--unicode-atoms
  ["café" "σ" "中文" "😀" "é" "Ωmega" "𐐷"])

(defconst fzf-native-differential--malformed-atoms
  (vector (unibyte-string #xff)
          (unibyte-string #xc3)
          (unibyte-string #xe2 #x82)
          (unibyte-string #xed #xa0 #x80)
          (unibyte-string #xf5 #x80 #x80 #x80))
  "Byte strings that are not valid UTF-8.")

(defconst fzf-native-differential--case-pairs
  [["café" "CAFÉ"]
   ["σ" "Σ"]
   ["k" "K"]
   ["ⱥ" "Ⱥ"]
   ["i" "İ"]
   ["ß" "ẞ"]])

(defconst fzf-native-differential--separators
  ["/" "-" "_" "." ":" " "])

(defconst fzf-native-differential--absent-atoms
  ["@" "#" "%" "&" ";" "?" "+" "=" "🛸"])

(defconst fzf-native-differential--common-kinds
  [fuzzy exact prefix suffix equal])

(defun fzf-native-differential-rng-create (seed)
  "Return a deterministic generator initialized with SEED."
  (make-fzf-native-differential-rng
   :state (logand (max 1 seed) #xffffffff)))

(defun fzf-native-differential-random (rng limit)
  "Use RNG to return an integer in [0, LIMIT)."
  (let ((x (fzf-native-differential-rng-state rng)))
    (setq x (logxor x (ash x 13)))
    (setq x (logxor x (ash x -17)))
    (setq x (logxor x (ash x 5)))
    (setq x (logand x #xffffffff))
    (setf (fzf-native-differential-rng-state rng) x)
    (if (<= limit 0) 0 (% x limit))))

(defun fzf-native-differential--pick (rng vector)
  "Return one member of VECTOR using RNG."
  (aref vector (fzf-native-differential-random rng (length vector))))

(defun fzf-native-differential--identity (id)
  "Return the stable textual identity for ID."
  (format "~%04x~" id))

(defun fzf-native-differential--escape-literal (literal)
  "Escape spaces in LITERAL for the extended-search parser."
  (replace-regexp-in-string " " "\\ " literal t t))

(defun fzf-native-differential-render-term (term fuzzy)
  "Render TERM for an extended query with global FUZZY mode."
  (let* ((literal
          (fzf-native-differential--escape-literal
           (fzf-native-differential-term-literal term)))
         (kind (fzf-native-differential-term-kind term))
         (inverse (fzf-native-differential-term-inverse term)))
    (if inverse
        (pcase kind
          ('fuzzy (concat "!'" literal))
          ('exact (concat "!" literal))
          ('prefix (concat "!^" literal))
          ('suffix (concat "!" literal "$"))
          ('equal (concat "!^" literal "$"))
          (_ (error "Unsupported inverse term kind: %S" kind)))
      (pcase kind
        ('fuzzy (if fuzzy literal (concat "'" literal)))
        ('exact (if fuzzy (concat "'" literal) literal))
        ('prefix (concat "^" literal))
        ('suffix (concat literal "$"))
        ('equal (concat "^" literal "$"))
        ('boundary-exact (concat "'" literal "'"))
        (_ (error "Unsupported term kind: %S" kind))))))

(defun fzf-native-differential-render-query (query)
  "Render structured QUERY for fzf and fzf-native."
  (mapconcat
   (lambda (set)
     (mapconcat
      (lambda (term)
        (fzf-native-differential-render-term
         term (fzf-native-differential-query-fuzzy query)))
      set " | "))
   (fzf-native-differential-query-sets query) " "))

(defun fzf-native-differential--subsequence (string)
  "Return a nonempty, ordered subsequence of STRING."
  (let ((characters (string-to-list string)) result)
    (cl-loop for character in characters
             for index from 0
             when (zerop (% index 2))
             do (push character result))
    (if result
        (apply (if (multibyte-string-p string)
                   #'string
                 #'unibyte-string)
               (nreverse result))
      "a")))

(defun fzf-native-differential--prefix (string)
  "Return a nonempty prefix of STRING."
  (substring string 0 (max 1 (min (length string) 5))))

(defun fzf-native-differential--suffix (string)
  "Return a nonempty suffix of STRING."
  (substring string (max 0 (- (length string) 6))))

(defun fzf-native-differential--absent (rng candidate)
  "Return a one-character atom that is absent from CANDIDATE.

Use RNG only for the defensive fallback.  A missing single character cannot
match CANDIDATE under fuzzy, exact, prefix, suffix, or equal matching."
  (or (cl-find-if
       (lambda (atom) (not (string-search atom candidate)))
       (append fzf-native-differential--absent-atoms nil))
      (concat "qzx" (number-to-string
                     (fzf-native-differential-random rng 100000)))))

(defun fzf-native-differential--term-for-candidate
    (kind candidate needle &optional query-needle)
  "Return a KIND term that matches CANDIDATE.

For fuzzy and exact terms, derive text from QUERY-NEEDLE or NEEDLE."
  (let ((source (or query-needle needle)))
    (make-fzf-native-differential-term
     :kind kind
     :inverse nil
     :literal
     (pcase kind
       ('fuzzy (fzf-native-differential--subsequence source))
       ('exact source)
       ('prefix (fzf-native-differential--prefix candidate))
       ('suffix (fzf-native-differential--suffix candidate))
       ('equal candidate)
       ('boundary-exact source)
       (_ (error "Unsupported generated term kind: %S" kind))))))

(defun fzf-native-differential--inverse-for-candidate (rng candidate)
  "Use RNG to return an inverse shared-syntax term that accepts CANDIDATE."
  (make-fzf-native-differential-term
   :kind (fzf-native-differential--pick
          rng fzf-native-differential--common-kinds)
   :inverse t
   :literal (fzf-native-differential--absent rng candidate)))

(defun fzf-native-differential--miss-for-candidate (rng candidate)
  "Use RNG to return a positive exact term that rejects CANDIDATE."
  (make-fzf-native-differential-term
   :kind 'exact :inverse nil
   :literal (fzf-native-differential--absent rng candidate)))

(defun fzf-native-differential--repeat-unit (unit count)
  "Return UNIT repeated COUNT times."
  (apply #'concat (make-list (max 0 count) unit)))

(defun fzf-native-differential--fit-length (unit target)
  "Repeat and truncate UNIT to exactly TARGET characters."
  (let* ((unit-length (max 1 (length unit)))
         (copies (/ (+ target unit-length -1) unit-length)))
    (substring (fzf-native-differential--repeat-unit unit copies) 0 target)))

(defun fzf-native-differential--length-target (profile serial)
  "Return the query-length target for PROFILE and SERIAL."
  (if (eq profile 'long)
      (aref [999 1000 1001] (% serial 3))
    (aref [1 2 7 31 32 63 64] (% serial 7))))

(defun fzf-native-differential--text-plan (rng profile serial case-mode)
  "Return a text-generation plan for RNG, PROFILE, SERIAL, and CASE-MODE."
  (let* ((variant
          (fzf-native-differential--pick
           rng [ascii unicode unicode casefold malformed]))
         ;; A case-fold pair cannot be the guaranteed matching candidate in
         ;; respect-case mode.  Keep that draw as ordinary Unicode instead.
         (casefold (and (eq variant 'casefold)
                        (not (eq case-mode 'respect))))
         (text-class
          (cond (casefold 'unicode)
                ((eq variant 'casefold) 'unicode)
                (t variant)))
         (target (fzf-native-differential--length-target profile serial))
         (pair (and casefold
                    (fzf-native-differential--pick
                     rng fzf-native-differential--case-pairs)))
         (query-unit
          (cond
           (pair (aref pair 0))
           ((eq text-class 'unicode)
            (fzf-native-differential--pick
             rng fzf-native-differential--unicode-atoms))
           ((eq text-class 'malformed)
            (fzf-native-differential--pick
             rng fzf-native-differential--malformed-atoms))
           (t
            (fzf-native-differential--pick
             rng fzf-native-differential--ascii-atoms))))
         (candidate-unit (if pair (aref pair 1) query-unit))
         (query-needle
          (fzf-native-differential--fit-length query-unit target))
         (candidate-needle
          (fzf-native-differential--fit-length candidate-unit target)))
    (list :class text-class
          :casefold casefold
          :target target
          :query-needle query-needle
          :candidate-needle candidate-needle)))

(defun fzf-native-differential--anchor-text
    (rng id needle primary-kind rank-shape)
  "Use RNG to give ID to a NEEDLE candidate.

PRIMARY-KIND and RANK-SHAPE control its layout."
  (let ((identity (fzf-native-differential--identity id))
        (separator (fzf-native-differential--pick
                    rng fzf-native-differential--separators)))
    (pcase primary-kind
      ('prefix (concat needle separator "tail" separator identity))
      ('suffix (concat identity separator "head" separator needle))
      ('equal (concat needle separator identity))
      ('boundary-exact (concat identity "/" needle "/tail"))
      (_
       (pcase rank-shape
         ('front (concat needle separator "tail" separator identity))
         ('middle (concat "head" separator needle separator identity))
         ('tail (concat identity separator "head" separator needle))
         ('ambiguous
          (concat needle separator "head" separator needle separator identity))
         (_ (concat "head" separator needle separator identity)))))))

(defun fzf-native-differential--query-shape (rng profile)
  "Use RNG to return the structured-query shape for PROFILE."
  (if (eq profile 'long)
      'single
    (fzf-native-differential--pick
     rng [empty single and or inverse mixed])))

(defun fzf-native-differential--primary-kind (rng profile)
  "Use RNG to return the primary term kind for PROFILE."
  (cond
   ((eq profile 'long) 'fuzzy)
   ((and (eq profile 'parity)
         (zerop (fzf-native-differential-random rng 11)))
    'boundary-exact)
   (t (fzf-native-differential--pick
       rng fzf-native-differential--common-kinds))))

(defun fzf-native-differential--secondary-kind (rng)
  "Return one shared term kind using RNG."
  (fzf-native-differential--pick rng fzf-native-differential--common-kinds))

(defun fzf-native-differential--build-sets
    (rng shape primary anchor needle)
  "Use RNG to build a term-set list that accepts ANCHOR.

SHAPE selects the grammar.  PRIMARY and NEEDLE supply matching text."
  (let ((miss (fzf-native-differential--miss-for-candidate rng anchor))
        (second
         (fzf-native-differential--term-for-candidate
          (fzf-native-differential--secondary-kind rng)
          anchor needle)))
    (pcase shape
      ('empty nil)
      ('single (list (list primary)))
      ('and (list (list primary) (list second)))
      ('or (list (list primary miss)))
      ('inverse
       (list (list primary)
             (list (fzf-native-differential--inverse-for-candidate
                    rng anchor))))
      ('mixed
       (list (list primary miss)
             (list second)
             (list (fzf-native-differential--inverse-for-candidate
                    rng anchor))))
      (_ (error "Unsupported query shape: %S" shape)))))

(defun fzf-native-differential--identified-text (id body)
  "Add stable identity ID after BODY."
  (concat body "/" (fzf-native-differential--identity id)))

(defun fzf-native-differential--rank-candidates
    (primary-kind candidate-needle query-needle)
  "Return ranking variants for PRIMARY-KIND.

CANDIDATE-NEEDLE and QUERY-NEEDLE supply equivalent match text."
  (let ((needle candidate-needle)
        (query query-needle))
    (pcase primary-kind
      ((or 'fuzzy 'exact)
       (list
        (list 'short (concat needle "/" (fzf-native-differential--identity 1)))
        (list 'middle (fzf-native-differential--identified-text
                       2 (concat "head/" needle)))
        (list 'long (fzf-native-differential--identified-text
                     3 (concat "long/prefix/" needle "/long/tail")))
        (list 'ambiguous (fzf-native-differential--identified-text
                          4 (concat needle "/x/" needle)))))
      ('prefix
       (list
        (list 'short (fzf-native-differential--identified-text 1 query))
        (list 'long
              (fzf-native-differential--identified-text
               2 (concat query "/long/tail")))))
      ('suffix
       (list
        (list 'short
              (concat (fzf-native-differential--identity 1) "/" query))
        (list 'long
              (concat (fzf-native-differential--identity 2)
                      "/long/prefix/" query))))
      (_ nil))))

(defun fzf-native-differential--negative-text (id needle)
  "Return a near miss for NEEDLE with identity ID."
  (let* ((characters (string-to-list needle))
         (replacement (if characters
                          (apply (if (multibyte-string-p needle)
                                     #'string
                                   #'unibyte-string)
                                 (cdr characters))
                        "qzx")))
    (fzf-native-differential--identified-text
     id (concat "miss/" (if (string-empty-p replacement) "qzx" replacement)))))

(defun fzf-native-differential--random-candidate (rng id text-class)
  "Use RNG to return candidate ID from TEXT-CLASS."
  (let ((count (1+ (fzf-native-differential-random rng 5))) pieces)
    (dotimes (_ count)
      (push
       (cond
        ((and (eq text-class 'unicode)
              (zerop (fzf-native-differential-random rng 3)))
         (fzf-native-differential--pick
          rng fzf-native-differential--unicode-atoms))
        ((and (eq text-class 'malformed)
              (zerop (fzf-native-differential-random rng 2)))
         (fzf-native-differential--pick
          rng fzf-native-differential--malformed-atoms))
        (t
         (fzf-native-differential--pick
          rng fzf-native-differential--ascii-atoms)))
       pieces)
      (push (fzf-native-differential--pick
             rng fzf-native-differential--separators)
            pieces))
    (fzf-native-differential--identified-text
     id (apply #'concat (nreverse pieces)))))

(defun fzf-native-differential--candidates
    (rng anchor primary-kind candidate-needle query-needle text-class)
  "Use RNG to return identified candidates around ANCHOR.

PRIMARY-KIND, CANDIDATE-NEEDLE, QUERY-NEEDLE, and TEXT-CLASS select variants."
  (let ((candidates
         (list (make-fzf-native-differential-candidate
                :id 0 :text anchor :role 'anchor)))
        (next-id 1))
    (dolist (variant
             (fzf-native-differential--rank-candidates
              primary-kind candidate-needle query-needle))
      (push (make-fzf-native-differential-candidate
             :id next-id :text (cadr variant) :role (car variant))
            candidates)
      (setq next-id (1+ next-id)))
    ;; Preserve duplicate occurrences as distinct candidates.  The oracle
    ;; harness assigns output occurrences to these IDs in producer order.
    (push (make-fzf-native-differential-candidate
           :id next-id :text (copy-sequence anchor) :role 'duplicate)
          candidates)
    (setq next-id (1+ next-id))
    (push (make-fzf-native-differential-candidate
           :id next-id
           :text (fzf-native-differential--negative-text
                  next-id candidate-needle)
           :role 'near-miss)
          candidates)
    (setq next-id (1+ next-id))
    (dotimes (_ (+ 3 (fzf-native-differential-random rng 6)))
      (push (make-fzf-native-differential-candidate
             :id next-id
             :text (fzf-native-differential--random-candidate
                    rng next-id text-class)
             :role 'random)
            candidates)
      (setq next-id (1+ next-id)))
    (nreverse candidates)))

(defun fzf-native-differential-generate-case (rng seed serial profile)
  "Generate one deterministic differential case.

RNG supplies choices, SEED and SERIAL identify the replay, and PROFILE is one
of `common', `parity', or `long'."
  (unless (memq profile '(common parity long))
    (error "Unknown differential profile: %S" profile))
  (let* ((case-mode
          (if (eq profile 'long)
              'smart
            (fzf-native-differential--pick rng [smart ignore respect])))
         (fuzzy
          (if (eq profile 'long)
              t
            (not (zerop (fzf-native-differential-random rng 2)))))
         (shape (fzf-native-differential--query-shape rng profile))
         (primary-kind
          (fzf-native-differential--primary-kind rng profile))
         (rank-shape
          (fzf-native-differential--pick rng [front middle tail ambiguous]))
         (text-plan
          (fzf-native-differential--text-plan
           rng profile serial case-mode))
         (text-class (plist-get text-plan :class))
         (casefold (plist-get text-plan :casefold))
         (query-needle (plist-get text-plan :query-needle))
         (candidate-needle (plist-get text-plan :candidate-needle))
         (anchor
          (fzf-native-differential--anchor-text
           rng 0 candidate-needle primary-kind rank-shape))
         (primary
          (if (eq profile 'long)
              (make-fzf-native-differential-term
               :kind 'fuzzy :inverse nil :literal query-needle)
            (fzf-native-differential--term-for-candidate
             primary-kind anchor candidate-needle query-needle)))
         (sets
          (fzf-native-differential--build-sets
           rng shape primary anchor candidate-needle))
         (query
          (make-fzf-native-differential-query
           :sets sets :case-mode case-mode :fuzzy fuzzy
           :normalize nil :forward t))
         (rendered-query (fzf-native-differential-render-query query))
         (comparison
          (if (and (eq profile 'parity)
                   (not (eq primary-kind 'boundary-exact))
                   (zerop (% serial 3)))
              'ranking
            'membership))
         (candidates
          (fzf-native-differential--candidates
           rng anchor primary-kind candidate-needle query-needle text-class)))
    (make-fzf-native-differential-case
     :seed seed
     :serial serial
     :profile profile
     :query query
     :rendered-query rendered-query
     :candidates candidates
     :dimensions
     (list :text-class text-class
           :unicode-casefold casefold
           :needle-length (plist-get text-plan :target)
           :query-length (length rendered-query)
           :anchor-length (length anchor)
           :query-shape shape
           :primary-kind primary-kind
           :rank-shape rank-shape
           :valid-utf8 (not (eq text-class 'malformed)))
     :comparison comparison)))

(defun fzf-native-differential-case-description (case)
  "Return a compact replay description for CASE."
  (format
   "seed=%d serial=%d profile=%S comparison=%S dimensions=%S query=%S candidates=%S"
   (fzf-native-differential-case-seed case)
   (fzf-native-differential-case-serial case)
   (fzf-native-differential-case-profile case)
   (fzf-native-differential-case-comparison case)
   (fzf-native-differential-case-dimensions case)
   (fzf-native-differential-case-rendered-query case)
   (mapcar
    (lambda (candidate)
      (list (fzf-native-differential-candidate-id candidate)
            (fzf-native-differential-candidate-role candidate)
            (fzf-native-differential-candidate-text candidate)))
    (fzf-native-differential-case-candidates case))))

(provide 'fzf-native-differential-generator)
;;; fzf-native-differential-generator.el ends here
