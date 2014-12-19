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
(require 'eieio)

;; ---------------------------------------------------------
;; Interface Code (The Crust)
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


(defclass goalie--external-emacs () ()
  "external ui adaptor interface")

(defvar goalie--save-file-path "/Users/slevin/goalie-file.txt")

(defmethod goalie--initialize-ui ((obj goalie--external-emacs))
  (switch-to-buffer "*goalie*")
  (goalie-mode))

(defun goalie--insert-line (external-interface hilight-fun text commit-line)
  (let ((boxes (if commit-line " [ ] " "")))
    (insert (concat boxes (funcall hilight-fun external-interface text) "\n"))))

(defmethod goalie--hilight-fun ((obj goalie--external-emacs) text)
  (propertize text 'face '((:foreground "red"))))

(defun goalie--insert-header-line (line)
  (let ((fline (propertize line 'face '((:foreground "medium sea green")))))
    (insert (concat fline "\n"))))

(defmethod goalie--render-ui ((obj goalie--external-emacs) commit hl)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (goalie--insert-header-line "Open Commitments")
    (mapc (lambda (each) (goalie--insert-line
                          obj
                          (car each)
                          (cadr each)
                          t))
          commit)
    (insert "\n")
    (goalie--insert-header-line "Today's Commitments (Date goes here)")
    (goalie--insert-line obj hl "-- Add Commitment --" nil)))

(defmethod goalie--prompt-for-new-commitment ((obj goalie--external-emacs))
  (read-string "What is your commitment? "))

(defmethod goalie--prompt-for-delete ((obj goalie--external-emacs))
  (y-or-n-p "Do you really want to delete? "))

(defmethod goalie--read-saved-content ((obj goalie--external-emacs))
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents goalie--save-file-path)
      (error '()))
    (buffer-string)))

(defmethod goalie--save-content ((obj goalie--external-emacs) content-string)
  (with-temp-file goalie--save-file-path
    (insert content-string)))

;; ---------------------------------------------------------
;; Interaction Code (The Sauce)
;; ---------------------------------------------------------

(defgeneric goalie--read-saved-content ())
(defgeneric goalie--save-content (content-string))
(defgeneric goalie--initialize-ui ())
(defgeneric goalie--render-ui (commit hl))
(defgeneric goalie--prompt-for-new-commitment ())
(defgeneric goalie--prompt-for-delete ())
(defgeneric goalie--hilight-fun (text))


(defmacro with-goalie-state-update (new-state-code)
  `(progn
     (setq goalie--existing-commitments ,new-state-code)
     (goalie--prepare-and-save-content goalie--existing-commitments)
     (goalie--call-render)))

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
    (goalie--render-ui goalie--interface render-commitments hilight-add)))


(defun goalie--handle-execute ()
  (let* ((new-commit (goalie--prompt-for-new-commitment goalie--interface)))
    (with-goalie-state-update
     (goalie--add-commitment goalie--existing-commitments
                             new-commit))))

(defun goalie--request-delete ()
  (let ((hilighted (goalie--hilight-index goalie--existing-commitments)))
    (if (not (null hilighted))
        (if (goalie--prompt-for-delete goalie--interface)
            (with-goalie-state-update
             (-remove-at hilighted goalie--existing-commitments))))))

(defun goalie--move-previous ()
  (goalie--move #'goalie--prev-index))

(defun goalie--move-next ()
  (goalie--move #'goalie--next-index))

(defun goalie--move (movefun)
  (with-goalie-state-update
   (goalie--move-hilight
    goalie--existing-commitments
    movefun)))

(defvar goalie--existing-commitments '())
(defvar goalie--interface '())

(defun goalie-start (interface)
  (setq goalie--interface interface)
  (setq goalie--existing-commitments (goalie--parse-saved-content
                                      (goalie--read-saved-content interface)))
  (goalie--initialize-ui interface)
  (goalie--call-render))

(defun goalie ()
  "Start goalie."
  (interactive)
  (goalie-start
   (make-instance 'goalie--external-emacs)))


;; ---------------------------------------------------------
;; Logic Code (The Cheese)
;; ---------------------------------------------------------


(defun goalie--get-hilight-fun (hilight)
  (if hilight
      #'goalie--hilight-fun
    #'goalie--non-hilight))

(defun goalie--non-hilight (interface text)
  text)

(defun goalie--add-commitment (existing new)
  (append existing (list (list nil new-commit))))

(defun goalie--move-hilight (existing movefun)
  (goalie--update-hilight-index
   (funcall movefun
            (goalie--hilight-index existing)
            existing)
   existing))

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
    (goalie--save-content goalie--interface prepared)))

;;; goalie.el ends here
