; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(asdf:defsystem "motion-tests"
  :author "Jens Thiede"
  :license "BSD-like"
  :depends-on ("motion" "xlunit" "events-extra" "click")
  :serial t
  :components ((:file "package")
               (:file "vec-test")
               (:file "poly-igo")
               (:file "poly-test")
               (:file "matter-test")
               (:file "collision-cell-test")
               (:file "motion-control-test")))