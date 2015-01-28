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
    (define-key map (kbd "a") 'goalie-add-commitment)
    (define-key map (kbd "RET") 'goalie-complete)
    (define-key map (kbd "s") 'goalie-skip)
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

(defun goalie-complete ()
  (interactive)
  (goalie--handle-complete))

(defun goalie-add-commitment ()
  (interactive)
  (goalie--prompt-for-commitment (current-time)))

(defun goalie-delete-commitment ()
  (interactive)
  (goalie--request-delete))

(defun goalie-skip ()
  (interactive)
  (goalie--skip))


(defclass goalie--external-emacs () ()
  "external ui adaptor interface")

(defvar goalie--save-file-path "/Users/slevin/goalie-file.txt")

(defmethod goalie--initialize-ui ((obj goalie--external-emacs))
  (switch-to-buffer "*goalie*")
  (goalie-mode))

(defun goalie--insert-line (external-interface hilight-fun text commit-marker)
  (insert (concat (funcall commit-marker external-interface)
                  (funcall hilight-fun external-interface text)
                  "\n")))

(defun goalie--format-commit-date (date)
  (format-time-string "(%A)" date)
  )

(defun goalie--insert-line (external-interface line)
  (insert (concat (funcall (oref line commit-marker-fun)
                           external-interface)
                  (funcall (oref line hilight-fun)
                           external-interface
                           (concat (oref line text)
                                   " "
                                   (goalie--format-commit-date (oref line commit-time))))
                  "\n")))

(defun goalie--insert-lines (external-interface lines)
  (mapc (lambda (each) (goalie--insert-line external-interface each)) lines))

(defmethod goalie--hilight-fun ((obj goalie--external-emacs) text)
  (propertize text 'face '((:foreground "red"))))

(defun goalie--insert-header-line (line)
  (let ((fline (propertize line 'face '((:foreground "medium sea green")))))
    (insert (concat fline "\n"))))

(defmethod goalie--render-ui ((obj goalie--external-emacs) commit-ls past-ls)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (goalie--insert-header-line "Open Commitments")
    (goalie--insert-lines obj commit-ls)
    (insert "\n")
    (goalie--insert-header-line "Past Commitments")
    (goalie--insert-lines obj past-ls)
    ))

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

(defmethod goalie--non-commit-marker ((obj goalie--external-emacs))
  "")

(defmethod goalie--commit-marker ((obj goalie--external-emacs))
  " [ ] ")

(defmethod goalie--commit-marker-complete ((obj goalie--external-emacs))
  " [*] ")

(defmethod goalie--commit-marker-skip ((obj goalie--external-emacs))
  " [-] ")

;; ---------------------------------------------------------
;; Interaction Code (The Sauce)
;; ---------------------------------------------------------

(defmacro with-goalie-state-update (new-state-code)
  `(progn
     (setq goalie--existing-commitments ,new-state-code)
     (goalie--build-lines)
     (goalie--prepare-and-save-content goalie--existing-commitments)
     (goalie--render-ui goalie--interface goalie--current-lines goalie--past-lines)))

(defun goalie--build-lines ()
  (let* ((res (goalie--partition-commitments goalie--existing-commitments))
         (current (goalie--build-commit-lines (car res)))
         (past (goalie--build-commit-lines (cadr res))))
    (setq goalie--current-lines current)
    (setq goalie--past-lines (goalie--exclude-old-lines past (goalie--days-before (current-time) goalie--too-old-days)))
    (goalie--update-line-hilight (goalie--all-lines) goalie--current-hilight-index)))

(defun goalie--days-before (time days)
  (time-subtract time (seconds-to-time (* 60 60 24 days))))

(defun goalie--exclude-old-lines (lines before-time)
  (--filter (time-less-p before-time (oref it commit-time)) lines))


(defun goalie--all-lines ()
  (append goalie--current-lines goalie--past-lines))

(defun goalie--handle-complete ()
  (with-goalie-state-update
   (progn
     (goalie--toggle-complete (goalie--current-commitment))
     goalie--existing-commitments)))

(defun goalie--update-line-hilight (lines index)
  (-map-indexed (lambda (idx ln)
                  (oset ln hilight-fun (goalie--get-hilight-fun
                                        (equal idx index)))
                  ln)
                lines))

(defun goalie--skip ()
  (with-goalie-state-update
   (progn
     (goalie--toggle-skip (goalie--current-commitment))
     goalie--existing-commitments)))

(defun goalie--partition-commitments (commits)
  (--separate (null (oref it completed)) commits))

(defun goalie--index-to-commitment (index lines)
  (oref (nth index lines) commitment))

(defun goalie--prompt-for-commitment (time)
  (let* ((new-commit (goalie--prompt-for-new-commitment goalie--interface)))
    (with-goalie-state-update
     (goalie--add-commitment goalie--existing-commitments
                             new-commit
                             time))))

(defun goalie--current-commitment ()
  (goalie--index-to-commitment goalie--current-hilight-index
                               (goalie--all-lines)))

(defun goalie--request-delete ()
  (if (> (length goalie--existing-commitments) 0)
      (if (goalie--prompt-for-delete goalie--interface)
          (with-goalie-state-update
           (let ((commitment (goalie--current-commitment)))
             (-remove (lambda (item) (equal item commitment))
                      goalie--existing-commitments))))))

(defun goalie--move-previous ()
  (goalie--move #'goalie--prev-index))

(defun goalie--move-next ()
  (goalie--move #'goalie--next-index))

(defun goalie--move (movefun)
  (setq goalie--current-hilight-index (funcall movefun
                                               goalie--current-hilight-index
                                               (goalie--all-lines)))
  (with-goalie-state-update
   goalie--existing-commitments)) ;; no change but update ui

(defvar goalie--existing-commitments '())
(defvar goalie--interface '())
(defvar goalie--current-hilight-index 0)
(defvar goalie--current-lines '())
(defvar goalie--past-lines '())
(defvar goalie--too-old-days 3)

(defun goalie-start (interface)
  (setq goalie--interface interface)
  (goalie--initialize-ui interface)
  (with-goalie-state-update
   (goalie--parse-saved-content
    (goalie--read-saved-content goalie--interface))))

(defun goalie ()
  "Start goalie."
  (interactive)
  (goalie-start
   (make-instance 'goalie--external-emacs)))


;; ---------------------------------------------------------
;; Logic Code (The Cheese)
;; ---------------------------------------------------------


(defclass goalie--line-c ()
  ((text :initarg :text
         :type string)
   (hilight-fun :initarg :hilight-fun)
   (commit-marker-fun :initarg :commit-marker-fun)
   (commit-time :initarg :commit-time :initform nil)
   (commitment :initarg :commitment :initform nil)))

(defclass goalie--commitment-c ()
  ((text :initarg :text
         :type string)
   (completed :initarg :completed
              :initform nil)
   (commit-time :initarg :commit-time
                :initform nil)))


(defun goalie--build-commit-lines (commitments)
  (-map-indexed (lambda (idx commit)
                  (goalie--line-c "commit"
                                  :text (oref commit text)
                                  :commit-marker-fun (goalie--get-commit-marker-fun (oref commit completed))
                                  :commit-time (oref commit commit-time)
                                  :commitment commit))
                commitments))


(defun goalie--get-hilight-fun (hilight)
  (if hilight
      #'goalie--hilight-fun
    #'goalie--non-hilight))

(defun goalie--non-hilight (interface text)
  text)

(defun goalie--get-commit-marker-fun (commit)
  (if (equal commit nil)
      #'goalie--commit-marker
    (if (equal commit 'complete)
        #'goalie--commit-marker-complete
      (if (equal commit 'skip)
          #'goalie--commit-marker-skip
        #'goalie--non-commit-marker))))

(defun goalie--toggle-complete (commit)
  (if (equal (oref commit completed) 'complete)
      (oset commit completed nil)
    (oset commit completed 'complete)))

(defun goalie--toggle-skip (commit)
  (if (equal (oref commit completed) 'skip)
      (oset commit completed nil)
    (oset commit completed 'skip)))

(defun goalie--add-commitment (existing new time)
  (append existing (list (goalie--commitment-c new
                                               :text new
                                               :commit-time time))))

(defun goalie--prev-index (currentindex lines)
  (cond ((= 0 (length lines)) 0)
        ((or (null currentindex) (equal 0 currentindex)) 0)
        ((>= currentindex (length lines)) (1- (length lines)))
        (t (1- currentindex))))

(defun goalie--next-index (currentindex lines)
  (cond ((= 0 (length lines)) 0)
        ((= currentindex (1- (length lines))) currentindex)
        (t (1+ currentindex))))

(defun goalie--parse-saved-content (content-string)
  (condition-case nil
      (let ((parsed (car (read-from-string content-string))))
        (if (listp parsed) parsed '()))
    (error '())))

(defun goalie--prepare-content (content)
  (prin1-to-string content))


(defun goalie--prepare-and-save-content (content)
  (let ((prepared (goalie--prepare-content content)))
    (goalie--save-content goalie--interface prepared)))

;;; goalie.el ends here
