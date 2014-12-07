(require 'f)

(defvar goalie-support-path
  (f-dirname load-file-name))

(defvar goalie-features-path
  (f-parent goalie-support-path))

(defvar goalie-root-path
  (f-parent goalie-features-path))

(add-to-list 'load-path goalie-root-path)

(require 'goalie)
(require 'espuds)
(require 'ert)

(defun savefile ()
  (f-join goalie-features-path "temp-save.txt"))

(defun clearsave ()
  (condition-case nil
      (f-delete (savefile))
    (error '())))

(Setup
 ;; Before anything has run
 )

(Before
 (clearsave)
 (setq goalie--save-file-path (savefile)))


(After
 ;; After each scenario is run
 )

(Teardown
 (clearsave))
