(defmacro with-my-fixture (&rest body)
  `(let* ((initialized nil)
          (render-commitments nil)
          (render-hilight nil)
          (new-commitments (list "commit1" "commit2"))
          (ifun (lambda () (setq initialized t)))
          (rfun (lambda (coms hl)
                  (setq render-commitments coms)
                  (setq render-hilight hl)))
          (pfun (lambda ()
                  (let ((return-commit (car new-commitments)))
                    (setq new-commitments (cdr new-commitments))
                    return-commit))))
     (goalie-start ifun rfun pfun)
     ,@body))

(ert-deftest start-initializes ()
    "Start goalie calls initialize and render functions"
    (with-my-fixture
     (should (equal initialized t))
     (should (equal render-commitments '()))))

(ert-deftest add-renders-new ()
  "After adding commitment it should show up in today list"
  (with-my-fixture
   (goalie--handle-execute)
   (should (equal render-commitments
                  (list (list nil "commit1"))))
   (should (equal render-hilight t))))


(ert-deftest add-multiple-renders-multiple ()
  "adding multiple times should return multiple"
  (with-my-fixture
   (goalie--handle-execute)
   (goalie--handle-execute)
   (should (equal render-commitments
                  (list (list nil "commit1")
                        (list nil  "commit2"))))))

(ert-deftest add-move-previous ()
  "adding one and move previous should highlight have that one highlighted"
  (with-my-fixture
   (goalie--handle-execute)
   (goalie--move-previous)
   (should (equal render-commitments
                  (list (list t "commit1"))))
   (should (equal render-hilight nil))))

(ert-deftest add-move-previous-2x ()
  "moving previous twice hilights top one"
  (with-my-fixture
   (goalie--handle-execute)
   (goalie--handle-execute)
   (goalie--move-previous)
   (goalie--move-previous)
   (should (equal render-commitments
                  (list (list t "commit1")
                        (list nil "commit2"))))))
