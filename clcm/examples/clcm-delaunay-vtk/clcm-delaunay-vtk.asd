(defsystem "clcm-delaunay-vtk"
  :description "Delaunay triangulation example"
  :version "0.0.1"
  :author "Nagamine, Hideaki <nagamine.hideaki.88x@kyoto-u.jp>"
  :license "BSD"
  :depends-on (:cafemielk)
  :components ((:file "create-mesh")))
