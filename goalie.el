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
;;; -*- lexical-binding: t -*-

(provide 'goalie)
(require 'dash)

;; ---------------------------------------------------------
;; User Interface Code (The Edges)
;; ---------------------------------------------------------

(defvar goalie-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'goalie-goto-next)
    (define-key map (kbd "p") 'goalie-goto-previous)
    (define-key map (kbd "d") 'goalie-delete-commitment)
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

(defun goalie-delete-commitment ()
  (interactive)
  (goalie--request-delete))

(defun goalie--initialize-ui ()
  (switch-to-buffer "*goalie*")
  (goalie-mode))

(defun goalie--insert-line (hilight-fun text)
  (insert (concat (funcall hilight-fun text) "\n")))

(defun goalie--hilight-fun (text)
  (propertize text 'face '((:foreground "red"))))

(defun goalie--insert-header-line (line)
  (let ((fline (propertize line 'face '((:foreground "medium sea green")))))
    (insert (concat fline "\n"))))

(defun goalie--render-ui (commit hl)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (goalie--insert-header-line "Open Commitments")
    (mapc (lambda (each) (goalie--insert-line
                          (car each)
                          (cadr each)))
          commit)
    (insert "\n")
    (goalie--insert-header-line "Today's Commitments (Date goes here)")
    (goalie--insert-line hl "-- Add Commitment --")))

(defun goalie--prompt-for-new-commitment ()
  (read-string "What is your commitment? "))

(defun goalie--prompt-for-delete ()
  (y-or-n-p "Do you really want to delete? "))

(defun goalie--read-saved-content ()
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents goalie--save-file-path)
      (error '()))
    (buffer-string)))

(defun goalie--save-content (content-string)
  (with-temp-file goalie--save-file-path
    (insert content-string)))

;; ---------------------------------------------------------
;; Logic Code (The Core)
;; ---------------------------------------------------------

(defun goalie--get-hilight-fun (hilight)
  (if hilight
      goalie--hilight-fun-private
    #'identity))

(defun goalie--handle-execute ()
  (let ((new-commit (funcall goalie--prompt-for-new-commitment-fun)))
    (setq goalie--existing-commitments (append goalie--existing-commitments
                                               (list (list nil new-commit))))
    (goalie--prepare-and-save-content goalie--existing-commitments)
    (goalie--call-render)))

(defun goalie--request-delete ()
  (let ((hilighted (goalie--hilight-index goalie--existing-commitments)))
    (if (not (null hilighted))
        (if (funcall goalie--confirmation-fun)
            (progn
              (setq goalie--existing-commitments
                    (-remove-at hilighted goalie--existing-commitments))
              (goalie--call-render))))))


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
  (goalie--move #'goalie--prev-index))

(defun goalie--move-next ()
  (goalie--move #'goalie--next-index))

(defun goalie--move (movefun)
  (setq goalie--existing-commitments
        (goalie--update-hilight-index
         (funcall movefun
                  (goalie--hilight-index goalie--existing-commitments)
                  goalie--existing-commitments)
         goalie--existing-commitments))
  (goalie--call-render))

(defun goalie--call-render ()
  (let ((hilight-add (goalie--get-hilight-fun
                      (-none?
                       (lambda (item) (car item))
                       goalie--existing-commitments)))
        (render-commitments (mapcar (lambda (each)
                                      (list (goalie--get-hilight-fun
                                             (car each))
                                            (cadr each)))
                                    goalie--existing-commitments)))
    (funcall goalie--render-fun render-commitments hilight-add)))

(defun goalie--parse-saved-content (content-string)
  (let ((parsed (condition-case nil
                    (let ((parsed (car (read-from-string content-string))))
                      (if (listp parsed) parsed '()))
                  (error '()))))
    (mapcar (lambda (item) (list nil item)) parsed)))

(defun goalie--prepare-content (content)
  (prin1-to-string (mapcar (lambda (item) (cadr item)) content)))


(defun goalie--prepare-and-save-content (content)
  (let ((prepared (goalie--prepare-content content)))
    (funcall goalie--save-fun prepared)))

(defvar goalie--save-file-path "/Users/slevin/goalie-file.txt")
(defvar goalie--existing-commitments '())
(defvar goalie--prompt-for-new-commitment-fun #'ignore)
(defvar goalie--render-fun #'ignore)
(defvar goalie--save-fun #'ignore)
(defvar goalie--confirmation-fun #'ignore)
(defvar goalie--hilight-fun-private #'identity)

;; read-saved-fun, save-fun
(defun goalie-start (interface
                     init-fun
                     render-fun
                     prompt-fun
                     confirm-fun
                     hilight-fun)
  (setq goalie--prompt-for-new-commitment-fun prompt-fun)
  (setq goalie--render-fun render-fun)
  (setq goalie--save-fun save-fun)
  (setq goalie--confirmation-fun confirm-fun)
  (setq goalie--hilight-fun-private hilight-fun)
  (setq goalie--existing-commitments (goalie--parse-saved-content
                                      (funcall read-saved-fun)))
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
   #'goalie--prompt-for-new-commitment
   #'goalie--save-content
   #'goalie--prompt-for-delete
   #'goalie--hilight-fun))

;; eieio stuff

(defclass goalie--external-emacs () ()
  "external ui adaptor interface")

(defgeneric goalie--read-saved-content ())
(defmethod goalie--read-saved-content ((obj goalie--external-emacs))
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents goalie--save-file-path)
      (error '()))
    (buffer-string)))

(defgeneric goalie--save-content (content-string))
(defmethod goalie--save-content ((obj goalie--external-emacs) content-string)
  (with-temp-file goalie--save-file-path
    (insert content-string)))

;;; goalie.el ends here
