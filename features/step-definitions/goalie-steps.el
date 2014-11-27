;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.


(Then "^I should be in \"\\([^\"]+\\)\" mode$"
      (lambda (mode)
        (equal 'mode major-mode)))

;; (Given "^I have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))

;; (When "^I have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))

;; (Then "^I should have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))

;; (And "^I have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))

;; (But "^I should not have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))
