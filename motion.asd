; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(asdf:defsystem "motion"
  :author "Jens Thiede"
  :license "BSD-like"
  :depends-on ("meta-package" "events")
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "vec")
               (:file "poly")
               (:file "collision-cell")
               (:file "motion-control")
               (:file "export")))