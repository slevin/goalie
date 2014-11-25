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

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
