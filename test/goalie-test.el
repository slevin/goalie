;;; -*- lexical-binding: t -*-

(defvar saved-content-string "()")
(defvar delete-prompt-return '())
(defvar *goalie-saved-content* nil)
(defvar *goalie-initialized* nil)
(defvar *goalie-render-commitments* nil)
(defvar *goalie-past-commitments* nil)
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

(defmethod goalie--render-ui ((obj goalie--external-test) coms past-coms)
  (setq *goalie-render-commitments* coms)
  (setq *goalie-past-commitments* past-coms))

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
  (setq saved-content-string "([object goalie--commitment-c \"commit1\" \"commit1\" complete nil])")
  (with-my-fixture
   (let ((cl1 (car *goalie-past-commitments*)))
     (should (equal "commit1" (oref cl1 text)))
     (should (equal #'goalie--commit-marker-complete (oref cl1 commit-marker-fun))))))

(ert-deftest add-renders-new ()
  "After adding commitment it should show up in today list hilighted"
  (with-my-fixture
   (goalie--prompt-for-commitment (current-time))
   (should (equal "commit1" (oref (car *goalie-render-commitments*) text)))
   (should (equal #'goalie--hilight-fun (oref (car *goalie-render-commitments*) hilight-fun)))))

(ert-deftest add-multiple-renders-multiple ()
  "adding multiple times should return multiple"
  (with-my-fixture
   (goalie--prompt-for-commitment (current-time))
   (goalie--prompt-for-commitment (current-time))
   (let ((c1 (car *goalie-render-commitments*))
         (c2 (cadr *goalie-render-commitments*)))
     (should (equal "commit1" (oref c1 text)))
     (should (equal "commit2" (oref c2 text))))))

(ert-deftest add-commits-saves-state ()
  "After adding content is saved through save function"
  (with-my-fixture
   (goalie--prompt-for-commitment (current-time))
   (goalie--prompt-for-commitment (current-time))
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
   (goalie--prompt-for-commitment (current-time))
   (goalie--prompt-for-commitment (current-time))
   (goalie--move-previous)
   (goalie--move-previous)
   (setq delete-prompt-return t)
   (goalie--request-delete)
   (should (equal t *delete-prompted*))
   (should (equal "commit2" (oref (car *goalie-render-commitments*) text)))))

;;; line builder

(ert-deftest build-complete-line ()
  "a completed commitment should have a renderable completed line"
  (let* ((coms (list (goalie--commitment-c "c1" :text "c1" :completed 'complete)))
        (cl (car (goalie--build-commit-lines coms))))
    (should (equal #'goalie--commit-marker-complete (oref cl commit-marker-fun)))))

(ert-deftest build-line-commit-time ()
  "commitment time passes to line"
  (let* ((time (current-time))
         (coms (list (goalie--commitment-c "commit1" :text "commit1" :commit-time time)))
         (cl (car (goalie--build-commit-lines coms))))
    (should (equal time (oref cl commit-time)))))

(ert-deftest line-has-commitment ()
  "line gets built with commitment pointer"
  (let* ((c1 (goalie--commitment-c "c1" :text "c1"))
         (coms (list c1))
         (l1 (car (goalie--build-commit-lines coms))))
    (should (equal c1 (oref l1 commitment)))))

(ert-deftest line-hilights ()
  "which line is hilighted"
  (let* ((c1 (goalie--commitment-c "c1" :text "c1"))
         (c2 (goalie--commitment-c "c2" :text "c2"))
         (coms (list c1 c2))
         (lines (goalie--build-commit-lines coms))
         (hlines (goalie--update-line-hilight lines 1)))
    (should (equal #'goalie--non-hilight (oref (car lines) hilight-fun)))
    (should (equal #'goalie--hilight-fun (oref (cadr lines) hilight-fun)))))

(ert-deftest hide-old ()
  "older commitments disappear"

  ;; lines is two buckets I guess
  )

;;; Simpler function tests

(ert-deftest filter-old ()
  (let* ((l1 (goalie--add-commitment '() "c1" (current-time)))
         (l2 (goalie--add-commitment l1 "c2" (goalie--days-before (current-time) (+ goalie--too-old-days 1))))
         (after (goalie--exclude-old-lines l2 (goalie--days-before (current-time) goalie--too-old-days))))
    (should (equal 1 (length after)))
    (should (equal "c1" (oref (car after) text)))))

(ert-deftest partition-commitments ()
  "puts completed commitments into separate bucket"
  (let* ((c1 (goalie--commitment-c "c1" :text "c1" :completed 'complete))
         (c2 (goalie--commitment-c "c2" :text "c2" :completed 'skip))
         (c3 (goalie--commitment-c "c3" :text "c3" :completed nil))
         (coms (list c1 c2 c3))
         (parts (goalie--partition-commitments coms))
         (current (nth 0 parts))
         (past (nth 1 parts)))
    (should (equal c3 (nth 0 current))) ; current list
    (should (equal c1 (nth 0 past))) ; complete in past list
    (should (equal c2 (nth 1 past))) ; skip in past list
    ))

(ert-deftest index-to-commitment ()
  "given index into lines, should get the corresponding commitment"
  (let* ((c1 (goalie--commitment-c "c1" :text "c1"))
         (c2 (goalie--commitment-c "c2" :text "c2"))
         (coms (list c1 c2))
         (lines (goalie--build-commit-lines coms)))
    (should (equal c1 (goalie--index-to-commitment 0 lines)))
    (should (equal c2 (goalie--index-to-commitment 1 lines)))))


(ert-deftest add-commitment ()
  (let* ((interface (make-instance 'goalie--external-test))
         (new-cs (goalie--add-commitment '()
                                         "newcommit"
                                         *test-time*)))
    (should (equal *test-time* (oref (car new-cs) commit-time)))))


(ert-deftest toggle-complete ()
  "goalie--toggle-complete"
  (let* ((c1 (goalie--commitment-c "c1" :text "c1"))
         (c2 (goalie--commitment-c "c2" :text "c2"))
         (coms (list c1 c2)))
    (should (equal nil (oref c1 completed))) ;; default is nil
    (goalie--toggle-complete c1)
    (should (equal 'complete (oref c1 completed)))
    (goalie--toggle-complete c1)
    (should (equal nil (oref c1 completed)))
    (goalie--toggle-skip c1)
    (goalie--toggle-complete c1)
    (should (equal 'complete (oref c1 completed)))))


(ert-deftest toggle-skip ()
  "goalie--toggle-skip"
  (let* ((c1 (goalie--commitment-c "c1" :text "c1"))
         (c2 (goalie--commitment-c "c2" :text "c2"))
         (coms (list c1 c2)))
    (should (equal nil (oref c1 completed))) ;; default is nil
    (goalie--toggle-skip c1)
    (should (equal 'skip (oref c1 completed)))
    (goalie--toggle-skip c1)
    (should (equal nil (oref c1 completed)))
    (goalie--toggle-complete c1)
    (goalie--toggle-skip c1)
    (should (equal 'skip (oref c1 completed)))))

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
