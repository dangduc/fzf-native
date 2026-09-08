;;; fzf-native-differential-exceptions.el --- Differential exception policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Duc Dang
;; Author: Duc Dang <me@dangduc.com>
;; Assisted-by: Codex:gpt-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of fzf-native.

;;; Commentary:

;; Classify only explicit compatibility differences.  A classifier receives
;; the compared result facet and a structured case.  No rule permits a crash,
;; hang, unknown candidate, or ordinary common-profile membership difference.

;;; Code:

(require 'cl-lib)
(require 'fzf-native-differential-generator)

(defconst fzf-native-differential-upstream-revision
  "1372d04f79bde0daa3bab4b96a068baafa808e67"
  "Pinned junegunn/fzf revision for differential exception metadata.")

(defconst fzf-native-differential-unicode-version-context
  `(:upstream-revision ,fzf-native-differential-upstream-revision
    :go-toolchain "go1.27.1"
    :go-unicode-version "17.0.0"
    :native-unicode-library "utf8proc"
    :native-library-version "2.10.0"
    :native-unicode-version "16.0.0")
  "Exact runtime identity required by the Unicode table exception.")

(defun fzf-native-differential--case-has-kind-p (case kind)
  "Return non-nil if CASE has a term of KIND."
  (cl-some
   (lambda (set)
     (cl-some
      (lambda (term)
        (eq (fzf-native-differential-term-kind term) kind))
      set))
   (fzf-native-differential-query-sets
    (fzf-native-differential-case-query case))))

(defun fzf-native-differential--utf8-continuation-p (byte)
  "Return non-nil when BYTE is a UTF-8 continuation byte."
  (and (<= #x80 byte) (<= byte #xbf)))

(defun fzf-native-differential--valid-utf8-bytes-p (string)
  "Return non-nil when unibyte STRING is structurally valid UTF-8."
  (let ((index 0)
        (length (length string))
        valid)
    (setq valid t)
    (while (and valid (< index length))
      (let ((first (aref string index)))
        (cond
         ((<= first #x7f)
          (setq index (1+ index)))
         ((and (<= #xc2 first) (<= first #xdf)
               (< (1+ index) length)
               (fzf-native-differential--utf8-continuation-p
                (aref string (1+ index))))
          (setq index (+ index 2)))
         ((and (<= #xe0 first) (<= first #xef)
               (< (+ index 2) length)
               (let ((second (aref string (1+ index))))
                 (and
                  (cond
                   ((= first #xe0) (and (<= #xa0 second) (<= second #xbf)))
                   ((= first #xed) (and (<= #x80 second) (<= second #x9f)))
                   (t (fzf-native-differential--utf8-continuation-p second)))
                  (fzf-native-differential--utf8-continuation-p
                   (aref string (+ index 2))))))
          (setq index (+ index 3)))
         ((and (<= #xf0 first) (<= first #xf4)
               (< (+ index 3) length)
               (let ((second (aref string (1+ index))))
                 (and
                  (cond
                   ((= first #xf0) (and (<= #x90 second) (<= second #xbf)))
                   ((= first #xf4) (and (<= #x80 second) (<= second #x8f)))
                   (t (fzf-native-differential--utf8-continuation-p second)))
                  (fzf-native-differential--utf8-continuation-p
                   (aref string (+ index 2)))
                  (fzf-native-differential--utf8-continuation-p
                   (aref string (+ index 3))))))
          (setq index (+ index 4)))
         (t (setq valid nil)))))
    valid))

(defun fzf-native-differential--malformed-utf8-string-p (string)
  "Return non-nil if STRING has bytes that are not valid UTF-8."
  (and (stringp string)
       (not (multibyte-string-p string))
       (not (fzf-native-differential--valid-utf8-bytes-p string))))

(defun fzf-native-differential--case-has-malformed-utf8-p (case)
  "Return non-nil if CASE has an actually malformed byte string."
  (or
   (fzf-native-differential--malformed-utf8-string-p
    (fzf-native-differential-case-rendered-query case))
   (cl-some
    (lambda (candidate)
      (fzf-native-differential--malformed-utf8-string-p
       (fzf-native-differential-candidate-text candidate)))
    (fzf-native-differential-case-candidates case))))

(defun fzf-native-differential--identity-set-equal-p (left right)
  "Return non-nil when LEFT and RIGHT contain the same identities."
  (and (listp left)
       (listp right)
       (= (length left) (length right))
       (= (length left) (length (delete-dups (copy-sequence left))))
       (= (length right) (length (delete-dups (copy-sequence right))))
       (null (cl-set-difference left right :test #'equal))
       (null (cl-set-difference right left :test #'equal))))

(defun fzf-native-differential--identity-set-difference (left right)
  "Return identities present in exactly one of LEFT and RIGHT."
  (append (cl-set-difference left right :test #'equal)
          (cl-set-difference right left :test #'equal)))

(defun fzf-native-differential--unicode-version-context-p (context)
  "Return non-nil when CONTEXT has every pinned Unicode runtime identity."
  (and
   (listp context)
   (cl-loop
    for (key value) on fzf-native-differential-unicode-version-context
    by #'cddr
    always (and (plist-member context key)
                (equal (plist-get context key) value)))))

(defun fzf-native-differential--unicode-version-orientation
    (pattern candidate)
  "Return the pinned lowercase orientation for PATTERN and CANDIDATE."
  (when (and (stringp pattern) (= (length pattern) 1)
             (stringp candidate) (= (length candidate) 1))
    (let ((pattern-codepoint (aref pattern 0))
          (candidate-codepoint (aref candidate 0)))
      (cl-loop
       for pair in fzf-native-differential--unicode-version-lowercase-pairs
       if (and (= pattern-codepoint (car pair))
               (= candidate-codepoint (cdr pair)))
       return (list pair 'upper-to-lower)
       if (and (= pattern-codepoint (cdr pair))
               (= candidate-codepoint (car pair)))
       return (list pair 'lower-to-upper)))))

(defun fzf-native-differential--unicode-version-membership-p
    (case context)
  "Recognize the exact upstream-only table miss described by CASE and CONTEXT."
  (let* ((identities (and (listp context)
                          (plist-get context :differing-identities)))
         (native (and (listp context)
                      (plist-get context :native-membership)))
         (upstream (and (listp context)
                        (plist-get context :upstream-membership)))
         (query (fzf-native-differential-case-query case))
         (sets (and query (fzf-native-differential-query-sets query)))
         (term (and (listp sets)
                    (= (length sets) 1)
                    (listp (car sets))
                    (= (length (car sets)) 1)
                    (caar sets)))
         (literal (and term (fzf-native-differential-term-literal term)))
         (dimensions (fzf-native-differential-case-dimensions case))
         (candidates (fzf-native-differential-case-candidates case))
         (candidate-ids
          (mapcar #'fzf-native-differential-candidate-id candidates)))
    (and
     (consp identities)
     (listp native)
     (listp upstream)
     (fzf-native-differential--identity-set-equal-p identities identities)
     (fzf-native-differential--identity-set-equal-p native native)
     (fzf-native-differential--identity-set-equal-p upstream upstream)
     (fzf-native-differential--identity-set-equal-p
      identities
      (fzf-native-differential--identity-set-difference native upstream))
     (cl-every (lambda (identity)
                 (and (member identity upstream)
                      (not (member identity native))))
               identities)
     (= (length candidate-ids)
        (length (delete-dups (copy-sequence candidate-ids))))
     (eq (fzf-native-differential-case-profile case) 'parity)
     (eq (fzf-native-differential-case-comparison case) 'membership)
     (plist-get dimensions :valid-utf8)
     query
     (eq (fzf-native-differential-query-case-mode query) 'ignore)
     (not (fzf-native-differential-query-normalize query))
     term
     (not (fzf-native-differential-term-inverse term))
     (memq (fzf-native-differential-term-kind term)
           (append fzf-native-differential--unicode-version-public-kinds nil))
     (equal (fzf-native-differential-case-rendered-query case)
            (fzf-native-differential-render-query query))
     (cl-every
      (lambda (identity)
        (let* ((candidate
                (cl-find identity candidates
                         :key #'fzf-native-differential-candidate-id
                         :test #'equal))
               (orientation
                (and candidate
                     (fzf-native-differential--unicode-version-orientation
                      literal
                      (fzf-native-differential-candidate-text candidate)))))
          (and
           candidate
           (eq (fzf-native-differential-candidate-role candidate)
               'unicode-version-peer)
           orientation
           (equal (car orientation)
                  (plist-get dimensions :unicode-version-pair))
           (eq (cadr orientation)
               (plist-get dimensions :unicode-version-orientation)))))
      identities))))

(defun fzf-native-differential--exception-unicode-version-p
    (case facet context)
  "Recognize pinned Unicode table membership differences for CASE and FACET."
  (and (eq facet 'membership)
       (fzf-native-differential--unicode-version-context-p context)
       (fzf-native-differential--unicode-version-membership-p case context)))

(defun fzf-native-differential--malformed-query-p (case)
  "Return non-nil when CASE has a malformed rendered query."
  (fzf-native-differential--malformed-utf8-string-p
   (fzf-native-differential-case-rendered-query case)))

(defun fzf-native-differential--nonempty-query-p (case)
  "Return non-nil when CASE has at least one parsed query term."
  (cl-some #'identity
           (fzf-native-differential-query-sets
            (fzf-native-differential-case-query case))))

(defun fzf-native-differential--malformed-difference-attributed-p
    (case identities)
  "Return non-nil when malformed input in CASE can affect IDENTITIES."
  (let* ((candidates (fzf-native-differential-case-candidates case))
         (differing
          (mapcar
           (lambda (identity)
             (cl-find identity candidates
                      :key #'fzf-native-differential-candidate-id
                      :test #'equal))
           identities)))
    (and (cl-every #'identity differing)
         (or
          (fzf-native-differential--malformed-query-p case)
          (cl-every
           (lambda (candidate)
             (fzf-native-differential--malformed-utf8-string-p
              (fzf-native-differential-candidate-text candidate)))
           differing)))))

(defun fzf-native-differential--malformed-membership-predicted-p
    (case context)
  "Return non-nil when CONTEXT predicts CASE's decoder-only difference.

CONTEXT supplies actual native and upstream memberships plus the membership
from running the native matcher on Go-decoded input.  The predicted membership
must equal upstream, and its exact difference from native must equal the
reported identities.  Empty queries fail closed because decoding match text
cannot change their membership."
  (let ((identities (and (listp context)
                         (plist-get context :differing-identities)))
        (native (and (listp context)
                     (plist-get context :native-membership)))
        (upstream (and (listp context)
                       (plist-get context :upstream-membership)))
        (decoded (and (listp context)
                      (plist-get context :go-decoded-native-membership))))
    (and (consp identities)
         (plist-member context :native-membership)
         (plist-member context :upstream-membership)
         (plist-member context :go-decoded-native-membership)
         (listp native)
         (listp upstream)
         (listp decoded)
         (fzf-native-differential--nonempty-query-p case)
         (fzf-native-differential--identity-set-equal-p decoded upstream)
         (fzf-native-differential--identity-set-equal-p
          identities
          (fzf-native-differential--identity-set-difference native upstream))
         (fzf-native-differential--malformed-difference-attributed-p
          case identities))))

(defun fzf-native-differential--exception-malformed-utf8-p
    (case facet context)
  "Recognize malformed decoder differences for CASE, FACET, and CONTEXT."
  (and (eq facet 'membership)
       (not (plist-get
             (fzf-native-differential-case-dimensions case)
             :valid-utf8))
       (fzf-native-differential--case-has-malformed-utf8-p case)
       (fzf-native-differential--malformed-membership-predicted-p
        case context)))

(defconst fzf-native-differential-exceptions
  `((:name malformed-utf8-decoder
     :reason "Go and utf8proc decode malformed match text differently."
     :disposition accepted
     :upstream-revision ,fzf-native-differential-upstream-revision
     :owner "fzf-native UTF-8 compatibility policy"
     :scope "Only predicted membership changes for nonempty malformed input."
     :remove-when "Both oracles apply one documented malformed-byte policy."
     :predicate ,#'fzf-native-differential--exception-malformed-utf8-p)
    (:name unicode-lowercase-table-version
     :reason "Go Unicode 17 has 28 lowercase pairs absent from utf8proc Unicode 16."
     :disposition accepted
     :upstream-revision ,fzf-native-differential-upstream-revision
     :go-toolchain "go1.27.1"
     :go-unicode-version "17.0.0"
     :native-unicode-library "utf8proc"
     :native-library-version "2.10.0"
     :native-unicode-version "16.0.0"
     :owner "fzf-native Unicode table compatibility policy"
     :scope "Only upstream-only membership for the 28 enumerated one-rune lowercase pairs."
     :remove-when "Both peers use Unicode tables with the same lowercase mappings."
     :predicate ,#'fzf-native-differential--exception-unicode-version-p))
  "Ordered, narrow predicates for known differential behavior.")

(defun fzf-native-differential-classify (case facet &optional context)
  "Return the first exception entry for CASE, FACET, and CONTEXT.

Return nil for an unclassified difference."
  (cl-find-if
   (lambda (entry)
     (funcall (plist-get entry :predicate) case facet context))
   fzf-native-differential-exceptions))

(provide 'fzf-native-differential-exceptions)
;;; fzf-native-differential-exceptions.el ends here
