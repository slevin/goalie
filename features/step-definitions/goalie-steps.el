;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.


(require 'cl-lib)
(require 'espuds)

(Then "^\"\\(.+\\)\" should be active$"
      (lambda (mode)
        (cl-assert (string= mode major-mode))))

(Given "I start Goalie"
       (lambda ()
         (When "I start an action chain")
         (When "I press \"M-x\"")
         (When "I type \"goalie\"")
         (When "I execute the action chain")))

(Then "\"\\([^\"]+\\)\" should be a commitment"
      (lambda (new-commitment)
        (Then (format "I should see %S" new-commitment))))

(Then "\"\\([^\"]+\\)\" should not be a commitment"
      (lambda (new-commitment)
        (Then (format "I should not see %S" new-commitment))))

(When "I add commitment \"\\([^\"]+\\)\""
      (lambda (new-commitment)
        (When "I start an action chain")
        (When "I press \"RET\"")
        (When (format "I type %S" new-commitment))
        (When "I execute the action chain")))

(defmacro with-hilight-check (check)
  `(lambda (text)
     (goto-char (point-max))
     (re-search-backward text)
     (let* ((props (get-char-property (point) 'face))
            (hilighted (member (list :foreground "red") props)))
       ,check)))

(Then "\"\\([^\"]+\\)\" should be hilighted"
      (with-hilight-check
       (cl-assert hilighted nil
                  "Expected current point to be hilighted")))

(Then "\"\\([^\"]+\\)\" should not be hilighted"
      (with-hilight-check
       (cl-assert (not hilighted) nil
                  "Expected current point to not be hilighted")))

(When "I quit Goalie"
      (lambda ()
        (kill-buffer)))
