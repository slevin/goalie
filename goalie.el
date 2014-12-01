;;; goalie.el --- Emacs based goal tracking application

;; Copyright (C) 2014 Sean Levin

;; Author: Sean Levin
;; Created: 23 Nov 2014
;; Version: 20141123
;; URL: https://github.com/slevin/goalie

;;; Commentary:
;; Some description here

;; more description

;;; Change Log:
;; none

;;; Code:

(provide 'goalie)
(require 'dash)

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
  (goalie--move-previous))

(defun goalie-execute ()
  (interactive)
  (goalie--handle-execute))


(defun goalie--initialize-ui ()
  (switch-to-buffer "*goalie*")
  (goalie-mode))

(defun goalie--insert-line (hilight text)
  (let ((line (if hilight
                  (goalie--hilight-line text)
                text)))
    (insert (concat line "\n"))))

(defun goalie--render-ui (commit)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Open Commitments\n")
    (mapc (lambda (each) (goalie--insert-line (car each) (cadr each))) commit)
    (insert "\n")
    (insert "Today's Commitments (Date goes here)\n")
    (goalie--insert-line t "-- Add Commitment --")
    ))

(defun goalie--handle-execute ()
  (let ((new-commit (funcall goalie--prompt-for-new-commitment-fun)))
    (setq goalie--existing-commitments (append goalie--existing-commitments
                                               (list (list nil new-commit))))
    (funcall goalie--render-fun goalie--existing-commitments)))

(defun goalie--prompt-for-new-commitment ()
  (read-string "What is your commitment? ")
  )

(defun goalie--move-previous ()
  (setq goalie--existing-commitments
        (-map-indexed
         (lambda (idx item)
           ;; mark last item as highlighted
           (list (if (= (1+ idx) (length goalie--existing-commitments))
                     t
                   nil)
                 (cadr item)))
         goalie--existing-commitments))
  (funcall goalie--render-fun goalie--existing-commitments))

(defun goalie--hilight-line (line)
  (propertize line 'face '((:foreground "red"))))

(defvar goalie--existing-commitments '())
(defvar goalie--prompt-for-new-commitment-fun #'ignore)
(defvar goalie--render-fun #'ignore)

(defun goalie-start (init-fun
                     render-fun
                     prompt-fun)
  (setq goalie--prompt-for-new-commitment-fun prompt-fun)
  (setq goalie--render-fun render-fun)
  (setq goalie--existing-commitments '())
  (funcall init-fun)
  (funcall render-fun nil))

;; could have rerender whole thing based on updates
;; first is given a buffer var draw in
(defun goalie ()
  "Start goalie."
  (interactive)
  (goalie-start
   #'goalie--initialize-ui
   #'goalie--render-ui
   #'goalie--prompt-for-new-commitment)

  ;; start up goalie "object"
  ;; creates/opens buffer

  ;; reads in goalie data file

  ;; calls render method on parts
  )

;;; goalie.el ends here