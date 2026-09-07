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

(defun fzf-native-differential--exception-exact-boundary-p
    (case facet _context)
  "Return non-nil for current fzf boundary syntax in CASE and FACET.

CONTEXT is not applicable to this classifier."
  (and (eq facet 'membership)
       (fzf-native-differential--case-has-kind-p case 'boundary-exact)))

(defun fzf-native-differential--exception-ranking-p
    (case facet context)
  "Return non-nil for CASE and FACET after CONTEXT confirms membership."
  (and (eq facet 'ranking)
       (eq (fzf-native-differential-case-comparison case) 'ranking)
       (plist-get context :membership-equal)))

(defun fzf-native-differential--exception-normalization-p
    (case facet _context)
  "Recognize CASE normalization for FACET only when the query enables it.

CONTEXT is not applicable to this classifier."
  (and (eq facet 'membership)
       (fzf-native-differential-query-normalize
        (fzf-native-differential-case-query case))))

(defun fzf-native-differential--exception-backward-p
    (case facet _context)
  "Recognize CASE backward search for FACET only when the query requests it.

CONTEXT is not applicable to this classifier."
  (and (memq facet '(membership positions ranking))
       (not (fzf-native-differential-query-forward
             (fzf-native-differential-case-query case)))))

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

(defun fzf-native-differential--malformed-difference-p (case context)
  "Return non-nil if CONTEXT identifies malformed differences in CASE.

CONTEXT must contain a nonempty `:differing-identities' list.  Every identity
must name a candidate in CASE, and every differing candidate must itself
contain malformed UTF-8.  A malformed query alone does not attribute an
arbitrary valid-candidate difference to the decoder policy."
  (let ((identities (and (listp context)
                         (plist-get context :differing-identities)))
        (candidates (fzf-native-differential-case-candidates case)))
    (and (consp identities)
         (cl-every
          (lambda (identity)
            (let ((candidate
                   (cl-find identity candidates
                            :key #'fzf-native-differential-candidate-id
                            :test #'equal)))
              (and candidate
                   (fzf-native-differential--malformed-utf8-string-p
                    (fzf-native-differential-candidate-text candidate)))))
          identities))))

(defun fzf-native-differential--exception-malformed-utf8-p
    (case facet context)
  "Recognize malformed decoder differences for CASE, FACET, and CONTEXT."
  (and (memq facet '(membership positions))
       (not (plist-get
             (fzf-native-differential-case-dimensions case)
             :valid-utf8))
       (fzf-native-differential--case-has-malformed-utf8-p case)
       (fzf-native-differential--malformed-difference-p case context)))

(defconst fzf-native-differential-exceptions
  `((:name exact-boundary-syntax
     :reason "Current fzf implements the trailing-quote boundary term."
     :disposition parity-debt
     :upstream-revision ,fzf-native-differential-upstream-revision
     :owner "fzf-native query parser"
     :remove-when "Boundary-term membership matches the pinned fzf revision."
     :predicate ,#'fzf-native-differential--exception-exact-boundary-p)
    (:name score-ranking-revision
     :reason "Current fzf and fzf-native use different score and rank rules."
     :disposition parity-debt
     :upstream-revision ,fzf-native-differential-upstream-revision
     :owner "fzf-native scoring implementation"
     :remove-when "Ordered identities match pinned fzf when membership agrees."
     :predicate ,#'fzf-native-differential--exception-ranking-p)
    (:name normalization-policy
     :reason "The public fzf-native parser does not enable fzf normalization."
     :disposition parity-debt
     :upstream-revision ,fzf-native-differential-upstream-revision
     :owner "fzf-native public query API"
     :remove-when "Normalized-query membership matches the pinned fzf revision."
     :predicate ,#'fzf-native-differential--exception-normalization-p)
    (:name backward-search-capability
     :reason "fzf-native does not expose fzf's backward matcher direction."
     :disposition parity-debt
     :upstream-revision ,fzf-native-differential-upstream-revision
     :owner "fzf-native matcher API"
     :remove-when "Backward membership, positions, and ranking match pinned fzf."
     :predicate ,#'fzf-native-differential--exception-backward-p)
    (:name malformed-utf8-decoder
     :reason "Go and utf8proc preserve malformed input differently."
     :disposition accepted
     :upstream-revision ,fzf-native-differential-upstream-revision
     :owner "fzf-native UTF-8 compatibility policy"
     :scope "Only differing malformed inputs for membership or positions."
     :remove-when "Both oracles apply one documented malformed-byte policy."
     :predicate ,#'fzf-native-differential--exception-malformed-utf8-p))
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
