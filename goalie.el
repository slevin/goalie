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

;; code goes here

(defun goalie-initialize-ui ()
  (switch-to-buffer "*goalie*")
  (setq buffer-read-only t))

(defun goalie-render-ui ()
  (let ((inhibit-read-only t))
    (progn  (insert " a buffer\nsomething")
            (insert "\n")
            (insert "another line"))))

;; could have rerender whole thing based on updates
;; first is given a buffer var draw in
(defun goalie ()
  "Start goalie."
  (interactive)
  (goalie-initialize-ui)
  (render-ui)
  ;; start up goalie "object"
  ;; creates/opens buffer

  ;; reads in goalie data file

  ;; calls render method on parts
  )
;;; goalie.el ends here
