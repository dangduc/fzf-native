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
