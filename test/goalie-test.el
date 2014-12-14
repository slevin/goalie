;;; -*- lexical-binding: t -*-

(defvar saved-content-string "()")
(defvar delete-prompt-return '())
(defvar *goalie-saved-content* nil)
(defvar *goalie-initialized* nil)
(defvar *goalie-render-commitments* nil)
(defvar *goalie-render-hilight* nil)
(defvar *new-commitments* nil)
(defvar *delete-prompted* nil)

(defun testhi (text) text)

(defclass goalie--external-test () ()
  "test interface")

(defmethod goalie--read-saved-content ((obj goalie--external-test))
  saved-content-string)

(defmethod goalie--save-content ((obj goalie--external-test) content-string)
  (setq *goalie-saved-content* content-string))

(defmethod goalie--initialize-ui ((obj goalie--external-test))
  (setq *goalie-initialized* t))

(defmethod goalie--render-ui ((obj goalie--external-test) coms hl)
  (setq *goalie-render-commitments* coms)
  (setq *goalie-render-hilight* hl))

(defmethod goalie--prompt-for-new-commitment ((obj goalie--external-test))
  (let ((return-commit (car *new-commitments*)))
    (setq *new-commitments* (cdr *new-commitments*))
    return-commit))

(defmethod goalie--prompt-for-delete ((obj goalie--external-test))
  (setq *delete-prompted* t)
  delete-prompt-return)

;; render-commitments render-hilight
(defmacro with-my-fixture (&rest body)
  `(let* ((hilight-fun2 #'testhi))
     (setq *new-commitments* (list "commit1" "commit2"))
     (goalie-start
      (make-instance 'goalie--external-test)
      hilight-fun2)
     ,@body))

(ert-deftest start-initializes ()
    "Start goalie calls initialize and render functions"
    (with-my-fixture
     (should (equal *goalie-initialized* t))
     (should (equal *goalie-render-commitments* '()))))

(ert-deftest start-reads-in-saved-content ()
  "read in content"
  (setq saved-content-string "(\"commit one\" \"commit two\")")
  (with-my-fixture
   (should (equal *goalie-render-commitments*
                  (list (list #'identity "commit one")
                        (list #'identity "commit two"))))))

(ert-deftest add-renders-new ()
  "After adding commitment it should show up in today list"
  (with-my-fixture
   (goalie--handle-execute)
   (should (equal *goalie-render-commitments*
                  (list (list #'identity "commit1"))))
   (should (equal *goalie-render-hilight* #'testhi))))

(ert-deftest add-multiple-renders-multiple ()
  "adding multiple times should return multiple"
  (with-my-fixture
   (goalie--handle-execute)
   (goalie--handle-execute)
   (should (equal *goalie-render-commitments*
                  (list (list #'identity "commit1")
                        (list #'identity  "commit2"))))))

(ert-deftest add-commits-saves-state ()
  "After adding content is saved through save function"
  (with-my-fixture
   (goalie--handle-execute)
   (goalie--handle-execute)
   (should (equal *goalie-saved-content* "(\"commit1\" \"commit2\")"))))

(ert-deftest add-move-previous ()
  "adding one and move previous should highlight have that one highlighted"
  (with-my-fixture
   (goalie--handle-execute)
   (goalie--move-previous)
   (should (equal *goalie-render-commitments*
                  (list (list #'testhi "commit1"))))
   (should (equal *goalie-render-hilight* #'identity))))

(ert-deftest add-move-previous-2x ()
  "moving previous twice hilights top one"
  (with-my-fixture
   (goalie--handle-execute)
   (goalie--handle-execute)
   (goalie--move-previous)
   (goalie--move-previous)
   (should (equal *goalie-render-commitments*
                  (list (list #'testhi "commit1")
                        (list #'identity "commit2"))))))


(ert-deftest delete-nothing ()
  "delete in initial nothing state should do nothing"
  (with-my-fixture
   (goalie--request-delete)
   (should (null *delete-prompted*))
   ))


(ert-deftest delete-something ()
  "if something is hilighted then it should prompt for it and delete it"
  (with-my-fixture
   (goalie--handle-execute)
   (goalie--handle-execute)
   (goalie--move-previous)
   (goalie--move-previous)
   (setq delete-prompt-return t)
   (goalie--request-delete)
   (should (equal t *delete-prompted*))
   (should (equal *goalie-render-commitments* (list (list #'identity "commit2"))))))

;;; Simpler function tests

(ert-deftest hilight-index ()
  "goalie--hilight-index"
  (should (null (goalie--hilight-index
                 (list (list nil 'commit1)
                       (list nil 'commit2)))))

  (should (equal 0 (goalie--hilight-index
                    (list (list t 'commit1)
                          (list nil 'commit2)))))

  (should (equal 1 (goalie--hilight-index
                    (list (list nil 'commit1)
                          (list t 'commit2))))))

(ert-deftest update-hilight-index ()
  "goalie--update-hilight-index"
  (should (equal (list (list nil 'commit1)
                       (list nil 'commit2))
                 (goalie--update-hilight-index nil
                                               (list (list nil 'commit1)
                                                     (list nil 'commit2)))))

  (should (equal (list (list t 'commit1)
                       (list nil 'commit2))
                 (goalie--update-hilight-index 0
                                               (list (list nil 'commit1)
                                                     (list t 'commit2))))))

(ert-deftest prev-index ()
  "goalie--prev-index"
  (let ((exist (list (list nil 'commit1)
                     (list nil 'commit2))))
    ;; stops at 0
    (should (equal 0 (goalie--prev-index 0 exist)))
    ;; moves previous
    (should (equal 0 (goalie--prev-index 1 exist)))
    ;; last when at nil
    (should (equal 1 (goalie--prev-index nil exist)))))

(ert-deftest next-index ()
  "goalie--next-index"
  (let ((exist (list (list nil 'commit1)
                     (list nil 'commit2))))
    ;; stops at nil
    (should (null (goalie--next-index nil exist)))
    ;; go to nil if at end
    (should (null (goalie--next-index 1 exist)))
    ;; should move next
    (should (equal 1 (goalie--next-index 0 exist)))))

(ert-deftest parse ()
  "goalie--parse-saved-content"
  (should (equal (list (list nil 'commit1)
                       (list nil 'commit2))
                 (goalie--parse-saved-content "(commit1 commit2)")))
  (should (null (goalie--parse-saved-content "")))
  (should (null (goalie--parse-saved-content "parsable non list"))))

(ert-deftest prepare ()
  "goalie--prepare-saved-content"
  (should (equal "(commit1 commit2)"
                 (goalie--prepare-content (list (list nil 'commit1)
                                                (list nil 'commit2))))))
