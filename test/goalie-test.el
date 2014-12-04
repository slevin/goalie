(defvar saved-content-string "()")

(defmacro with-my-fixture (&rest body)
  `(let* ((initialized nil)
          (render-commitments nil)
          (render-hilight nil)
          (new-commitments (list "commit1" "commit2"))
          (readfun (lambda () saved-content-string))
          (ifun (lambda () (setq initialized t)))
          (rfun (lambda (coms hl)
                  (setq render-commitments coms)
                  (setq render-hilight hl)))
          (pfun (lambda ()
                  (let ((return-commit (car new-commitments)))
                    (setq new-commitments (cdr new-commitments))
                    return-commit))))
     (goalie-start readfun ifun rfun pfun)
     ,@body))

(ert-deftest start-initializes ()
    "Start goalie calls initialize and render functions"
    (with-my-fixture
     (should (equal initialized t))
     (should (equal render-commitments '()))))

(ert-deftest start-reads-in-saved-content ()
  "read in content"
  (setq saved-content-string "(\"commit one\" \"commit two\")")
  (with-my-fixture
   (should (equal render-commitments
                  (list (list nil "commit one")
                        (list nil "commit two"))))))

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
