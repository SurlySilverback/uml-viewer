(ns uml-viewer.gdscript-language.source-gdscript-spec
  (:require [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.source :as source]
            [uml-viewer.gdscript-language.source-gdscript :as gd-src]))

(def slime-path "spec/examples/gdscript/enemies/slime.gd")
(def slime-ns "spec/examples/gdscript/enemies/slime")

(describe "gdscript extractor"
  (it "maps a module path to a .gd file"
    (should= slime-path (gd-src/ns->source-path slime-ns))
    (should= slime-path (gd-src/ns->source-path (str slime-ns ".gd")))
    (should= slime-path
             (gd-src/ns->source-path "spec.examples.gdscript.enemies.slime")))

  (it "extracts a top-level func by name"
    (let [body (gd-src/extract-member (slurp slime-path) "hop")]
      (should (str/starts-with? body "func hop"))
      (should (str/includes? body "position.y"))))

  (it "extracts a private _ready func"
    (let [body (gd-src/extract-member (slurp slime-path) "_ready")]
      (should (str/starts-with? body "func _ready"))))

  (it "finds the 1-based line of a func"
    (should= 7 (gd-src/member-line (slurp slime-path) "hop")))

  (it "extracts static and async func declarations"
    (let [src (str "static func create():\n\treturn 1\n"
                   "async func wait():\n\tpass\n")
          st (gd-src/extract-member src "create")
          as (gd-src/extract-member src "wait")]
      (should (str/starts-with? st "static func create"))
      (should (str/starts-with? as "async func wait"))))

  (it "returns nil for an unknown member"
    (should-be-nil (source/member-source {:lang :gdscript
                                          :ns slime-ns
                                          :name "no_such_fn"})))

  (it "opens a module at the top of the file when no member name is given"
    (let [found (source/member-source {:lang :gdscript :ns slime-ns})]
      (should= slime-path (:file found))
      (should= slime-path (:title found))
      (should (str/starts-with? (:body found) "class_name Slime"))
      (should-be-nil (:line found)))))
