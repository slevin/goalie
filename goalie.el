;;; goalie.el --- Emacs based goal tracking application

;; Copyright (C) 2014 Sean Levin

;; Author: Sean Levin
;; Created: 23 Nov 2014
;; Version: ???????
;; URL: https://github.com/slevin/goalie

;;; Commentary:
;; Some description here

;; more description

;;; Change Log:
;; none

;;; Code:

(defvar goalie-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'goalie-goto-next)
    (define-key map (kbd "p") 'goalie-goto-previous)
    (define-key map (kbd "RET") 'goalie-execute)
    map)
  "Goalie key map.")


(define-derived-mode goalie-mode special-mode "Goalie"
  "Major mode for Goalie."
  )

(defun goalie-goto-next ()
  (interactive)
  (message "goto next"))

(defun goalie-goto-previous ()
  (interactive)
  (message "goto previous"))

(defun goalie-execute ()
  (interactive)
  (message "execute"))


(defun goalie-initialize-ui ()
  (switch-to-buffer "*goalie*")
  (goalie-mode))

(defun goalie-render-ui ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Yesterday's Commitments (Date goes here)\n")
    (insert "some lines about commitments\n\n")
    (insert "Today's Commitments (Date goes here)\n")
    (insert "some lines about todays commitments\n")))

;; could have rerender whole thing based on updates
;; first is given a buffer var draw in
(defun goalie ()
  "Start goalie."
  (interactive)
  (goalie-initialize-ui)
  (goalie-render-ui)
  ;; start up goalie "object"
  ;; creates/opens buffer

  ;; reads in goalie data file

  ;; calls render method on parts
  )
;;; goalie.el ends here
