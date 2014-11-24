(ert-deftest start-initializes ()
  "start goalie calls initialize and render functions"
  (let* ((icalled nil)
         (rcalled nil)
         (ifun (lambda () (setq icalled t)))
         (rfun (lambda () (setq rcalled t))))
    (goalie-start ifun rfun)
    (should (equal icalled t))
    (should (equal rcalled t))))
