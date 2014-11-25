(ert-deftest start-initializes ()
  "Start goalie calls initialize and render functions"
  (let* ((icalled nil)
         (rcalled nil)
         (ifun (lambda () (setq icalled t)))
         (rfun (lambda (coms) (setq rcalled t))))
    (goalie-start ifun rfun #'ignore)
    (should (equal icalled t))
    (should (equal rcalled t))))

(ert-deftest execute-on-add-prompts ()
  "Execute when current line is add prompts for new commitment"
  (let* ((prompt-called nil)
         (pfun (lambda ()
                 (progn
                   (setq prompt-called t)
                   "new prompt"))))
    (goalie-start #'ignore #'ignore pfun)
    (goalie--handle-execute)
    (should (equal prompt-called t))))

(ert-deftest add-renders-new ()
  "After adding commitment it should show up in today list"
  (let* ((commitment nil)
         (new-commit "work hard!")
         (rfun (lambda (coms) (setq commitment coms)))
         (pfun (lambda () new-commit)))
    (goalie-start #'ignore rfun pfun)
    (goalie--handle-execute)
    (should (equal commitment (list new-commit)))))

(ert-deftest add-multiple-renders-multiple ()
  "adding multiple times should return multiple"
  (let* ((commitments nil)
         (new-commits (list "work hard" "play hard"))
         (rfun (lambda (coms) (setq commitment coms)))
         (pfun (lambda ()
                 (let ((return-commit (car new-commits)))
                   (setq new-commits (cdr new-commits))
                   return-commit))))
    (goalie-start #'ignore rfun pfun)
    (goalie--handle-execute)
    (goalie--handle-execute)
    (should (equal commitment (list "work hard" "play hard")))))
