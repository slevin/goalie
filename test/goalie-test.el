;;; -*- lexical-binding: t -*-

(defvar saved-content-string "()")
(defvar delete-prompt-return '())
(defvar *goalie-saved-content* nil)
(defvar *goalie-initialized* nil)
(defvar *goalie-render-commitments* nil)
(defvar *new-commitments* nil)
(defvar *delete-prompted* nil)

(defvar *test-time* (current-time))

(defun testhi (text) text)

(defclass goalie--external-test () ()
  "test interface")

(defmethod goalie--read-saved-content ((obj goalie--external-test))
  saved-content-string)

(defmethod goalie--save-content ((obj goalie--external-test) content-string)
  (setq *goalie-saved-content* content-string))

(defmethod goalie--initialize-ui ((obj goalie--external-test))
  (setq *goalie-initialized* t))

(defmethod goalie--render-ui ((obj goalie--external-test) coms)
  (setq *goalie-render-commitments* coms))

(defmethod goalie--prompt-for-new-commitment ((obj goalie--external-test))
  (let ((return-commit (car *new-commitments*)))
    (setq *new-commitments* (cdr *new-commitments*))
    return-commit))

(defmethod goalie--prompt-for-delete ((obj goalie--external-test))
  (setq *delete-prompted* t)
  delete-prompt-return)

(defmethod goalie--hilight-fun ((obj goalie--external-test) text)
  text)

(defmethod goalie--current-time ((obj goalie--external-test))
  *test-time*)

(defmacro with-my-fixture (&rest body)
  `(progn
     (setq *new-commitments* (list "commit1" "commit2"))
     (goalie-start
      (make-instance 'goalie--external-test))
     ,@body))

(ert-deftest start-initializes ()
    "Start goalie calls initialize and render functions"
    (with-my-fixture
     (should (equal *goalie-initialized* t))
     (should (equal *goalie-render-commitments* '()))))

(ert-deftest start-reads-in-saved-content ()
  "read in content"
  (setq saved-content-string "([object goalie--commitment-c \"commit1\" \"commit1\" t nil])")
  (with-my-fixture
   (let ((cl1 (car *goalie-render-commitments*)))
     (should (equal "commit1" (oref cl1 text)))
     (should (equal #'goalie--commit-marker-complete (oref cl1 commit-marker-fun))))))

(ert-deftest add-renders-new ()
  "After adding commitment it should show up in today list hilighted"
  (with-my-fixture
   (goalie--prompt-for-commitment)
   (should (equal "commit1" (oref (car *goalie-render-commitments*) text)))
   (should (equal #'goalie--hilight-fun (oref (car *goalie-render-commitments*) hilight-fun)))))

(ert-deftest add-multiple-renders-multiple ()
  "adding multiple times should return multiple"
  (with-my-fixture
   (goalie--prompt-for-commitment)
   (goalie--prompt-for-commitment)
   (let ((c1 (car *goalie-render-commitments*))
         (c2 (cadr *goalie-render-commitments*)))
     (should (equal "commit1" (oref c1 text)))
     (should (equal "commit2" (oref c2 text))))))

(ert-deftest add-commits-saves-state ()
  "After adding content is saved through save function"
  (with-my-fixture
   (goalie--prompt-for-commitment)
   (goalie--prompt-for-commitment)
   (should (not (null *goalie-saved-content*)))))

(ert-deftest delete-nothing ()
  "delete in initial nothing state should do nothing"
  (with-my-fixture
   (goalie--request-delete)
   (should (null *delete-prompted*))
   ))

(ert-deftest delete-something ()
  "if something is hilighted then it should prompt for it and delete it"
  (with-my-fixture
   (goalie--prompt-for-commitment)
   (goalie--prompt-for-commitment)
   (goalie--move-previous)
   (goalie--move-previous)
   (setq delete-prompt-return t)
   (goalie--request-delete)
   (should (equal t *delete-prompted*))
   (should (equal "commit2" (oref (car *goalie-render-commitments*) text)))))

;;; line builder

(ert-deftest build-complete-line ()
  "a completed commitment should have a renderable completed line"
  (let* ((coms (list (goalie--commitment-c "c1" :text "c1" :completed t)))
        (cl (car (goalie--build-commit-lines coms 0))))
    (should (equal #'goalie--commit-marker-complete (oref cl commit-marker-fun)))))

(ert-deftest build--line-hi ()
  "first commitment line is hilighted line"
  (let* ((coms (list (goalie--commitment-c "commit1" :text "commit1")
                     (goalie--commitment-c "commit2" :text "commit2")))
         (cl (car (goalie--build-commit-lines coms 0)))
         (cl2 (cadr (goalie--build-commit-lines coms 0))))
    (should (equal #'goalie--hilight-fun (oref cl hilight-fun)))
    (should (equal #'goalie--commit-marker (oref cl commit-marker-fun)))
    (should (equal #'goalie--non-hilight (oref cl2 hilight-fun)))
    ))

(ert-deftest build--line-commit-time ()
  "commitment time passes to line"
  (let* ((time (current-time))
         (coms (list (goalie--commitment-c "commit1" :text "commit1" :commit-time time)))
         (cl (car (goalie--build-commit-lines coms 1))))
    (should (equal time (oref cl commit-time)))))

;;; Simpler function tests

(ert-deftest index-to-commitment ()
  "given index into lines, should get the corresponding commitment"
  (let* ((c1 (goalie--commitment-c "c1" :text "c1"))
         (c2 (goalie--commitment-c "c2" :text "c2"))
         (coms (list c1 c2))
         (lines (goalie--build-commit-lines coms 1)))
    (should (equal c1 (goalie--index-to-commitment 0 lines coms)))
    (should (equal c2 (goalie--index-to-commitment 1 lines coms)))))


(ert-deftest add-commitment ()
  (let* ((interface (make-instance 'goalie--external-test))
         (new-cs (goalie--add-commitment interface
                                         '()
                                         "newcommit")))
    (should (equal *test-time* (oref (car new-cs) commit-time)))))


(ert-deftest toggle-complete ()
  "goalie--toggle-complete"
  (let* ((coms (list (goalie--commitment-c "commit1" :text "commit1")
                     (goalie--commitment-c "commit2" :text "commit2")))
         (new (goalie--toggle-complete 1 coms)))
    (should (equal nil (oref (car new) completed)))
    (should (equal t (oref (cadr new) completed)))))



(ert-deftest line-class ()
  "goalie--line-c"
  (let ((line (goalie--line-c "test"
                              :text "testline"
                              :hilight-fun #'identity
                              :commit-marker-fun #'ignore)))
    (should (equal "testline" (oref line text)))
    (should (equal #'identity (oref line hilight-fun)))
    (should (equal #'ignore (oref line commit-marker-fun))))
  )

(ert-deftest commitment-class ()
  "goalie--commitment-c"
  (let ((commit (goalie--commitment-c "com"
                                      :text "com1")))
    (should (equal "com1" (oref commit text)))
    (should (null (oref commit completed)))
    (oset commit completed t)
    (should (equal t (oref commit completed)))))

(ert-deftest prev-index ()
  "goalie--prev-index"
  ;; empty list always goes to 0
  (should (equal 0 (goalie--prev-index 0 '())))
  (let ((exist (list 'commit1 'commit2)))
    ;; stops at 0
    (should (equal 0 (goalie--prev-index 0 exist)))
    ;; moves previous
    (should (equal 0 (goalie--prev-index 1 exist)))
    ;; last when greater
    (should (equal 1 (goalie--prev-index 10 exist)))))

(ert-deftest next-index ()
  "goalie--next-index"
  ;; empty list always goes to 0
  (should (equal 0 (goalie--next-index 0 '())))
  (let ((exist (list 'commit1 'commit2)))
    ;; stops at end
    (should (equal 1 (goalie--next-index 1 exist)))
    ;; should move next
    (should (equal 1 (goalie--next-index 0 exist)))))

(ert-deftest parse ()
  "goalie--parse-saved-content"
  (let* ((coms (list (goalie--commitment-c "commit1" :text "commit1" :completed t)
                     (goalie--commitment-c "commit2" :text "commit2")))
         (str (goalie--prepare-content coms))
         (parsed (goalie--parse-saved-content str)))
    (should (equal coms parsed)))
  (should (null (goalie--parse-saved-content "")))
  (should (null (goalie--parse-saved-content "parsable non list"))))
