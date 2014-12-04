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
  (goalie--move-next))

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

(defun goalie--render-ui (commit hl)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Open Commitments\n")
    (mapc (lambda (each) (goalie--insert-line (car each) (cadr each))) commit)
    (insert "\n")
    (insert "Today's Commitments (Date goes here)\n")
    (goalie--insert-line hl "-- Add Commitment --")
    ))

(defun goalie--handle-execute ()
  (let ((new-commit (funcall goalie--prompt-for-new-commitment-fun)))
    (setq goalie--existing-commitments (append goalie--existing-commitments
                                               (list (list nil new-commit))))
    (goalie--call-render)))

(defun goalie--prompt-for-new-commitment ()
  (read-string "What is your commitment? ")
  )


(defun goalie--hilight-index (existing-commitments)
  (-find-index
   (lambda (item) (not (null (car item))))
   existing-commitments))

(defun goalie--update-hilight-index (newindex existing-commitments)
  (-map-indexed
   (lambda (idx item)
     (list (cond ((null newindex) nil)
                 ((= idx newindex) t)
                 (t nil))
           (cadr item)))
   existing-commitments))

(defun goalie--prev-index (currentindex existing-commitments)
  (if (> (length existing-commitments) 0)
      (cond ((null currentindex) (1- (length existing-commitments)))
            ((equal currentindex 0) 0)
            (t (1- currentindex)))
    nil))

(defun goalie--next-index (currentindex existing-commitments)
  (cond ((null currentindex) nil)
        ((= currentindex (1- (length existing-commitments))) nil)
        (t (1+ currentindex))))

(defun goalie--move-previous ()
  (setq goalie--existing-commitments
        (goalie--update-hilight-index
         (goalie--prev-index (goalie--hilight-index
                              goalie--existing-commitments)
                             goalie--existing-commitments)
         goalie--existing-commitments))
  (goalie--call-render))

(defun goalie--move-next ()
  (setq goalie--existing-commitments
        (goalie--update-hilight-index
         (goalie--next-index (goalie--hilight-index
                              goalie--existing-commitments)
                             goalie--existing-commitments)
         goalie--existing-commitments))
  (goalie--call-render))

(defun goalie--call-render ()
  (let ((hilight-add (-none?
                      (lambda (item) (car item))
                      goalie--existing-commitments)))
    (funcall goalie--render-fun goalie--existing-commitments hilight-add)))

(defun goalie--hilight-line (line)
  (propertize line 'face '((:foreground "red"))))

(defun goalie--read-saved-content ()
  '())

(defvar goalie--existing-commitments '())
(defvar goalie--prompt-for-new-commitment-fun #'ignore)
(defvar goalie--render-fun #'ignore)

(defun goalie-start (read-saved-fun
                     init-fun
                     render-fun
                     prompt-fun)
  (setq goalie--prompt-for-new-commitment-fun prompt-fun)
  (setq goalie--render-fun render-fun)
  (setq goalie--existing-commitments (funcall read-saved-fun))
  (funcall init-fun)
  (goalie--call-render))

;; could have rerender whole thing based on updates
;; first is given a buffer var draw in
(defun goalie ()
  "Start goalie."
  (interactive)
  (goalie-start
   #'goalie--read-saved-content
   #'goalie--initialize-ui
   #'goalie--render-ui
   #'goalie--prompt-for-new-commitment))

;;; goalie.el ends here
