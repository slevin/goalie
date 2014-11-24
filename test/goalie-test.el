(ert-deftest start-initializes ()
  "Start goalie calls initialize and render functions"
  (let* ((icalled nil)
         (rcalled nil)
         (ifun (lambda () (setq icalled t)))
         (rfun (lambda () (setq rcalled t))))
    (goalie-start ifun rfun)
    (should (equal icalled t))
    (should (equal rcalled t))))

(ert-deftest execute-on-add-prompts ()
  "Execute when current line is add prompts for new commitment"
  (let* ((prompt-called nil)
         (pfun (lambda () (setq prompt-called t))))
    (goalie-start #'ignore #'ignore pfun)
    (goalie--handle-execute)
    (should (equal prompt-called t))))
