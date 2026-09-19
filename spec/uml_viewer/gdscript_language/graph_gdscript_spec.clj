(ns uml-viewer.gdscript-language.graph-gdscript-spec
  (:require [clojure.java.io :as io]
            [speclj.core :refer :all]
            [uml-viewer.graph :as graph]
            [uml-viewer.gdscript-language.graph-gdscript]))

(defn- spit-gd [dir rel content]
  (let [f (io/file dir rel)]
    (io/make-parents f)
    (spit f content)
    f))

(defn- by-id [g]
  (into {} (map (juxt :id identity) (:classes g))))

(defn- edge-set [g]
  (set (map (juxt :from :to :kind) (:edges g))))

(describe "gdscript graph"
  (it "scans the fixture tree for classes, inheritance, and preload edges"
    (let [g (graph/scan (graph/lookup :gdscript) "spec/examples/gdscript" {})
          classes (by-id g)
          edges (edge-set g)]
      (should= #{:player :enemies.enemy :enemies.slime
                 :CharacterBody2D :Node :addons.vendor.widget}
               (set (keys classes)))
      (should= "Player" (:name (classes :player)))
      (should= "Enemy" (:name (classes :enemies.enemy)))
      (should= "Slime" (:name (classes :enemies.slime)))
      (should= "spec/examples/gdscript/enemies/slime" (:ns (classes :enemies.slime)))
      (should= "spec/examples/gdscript/player" (:ns (classes :player)))
      (should (:foreign (classes :CharacterBody2D)))
      (should (:foreign (classes :Node)))
      (should (:foreign (classes :addons.vendor.widget)))
      (should-not (:foreign (classes :player)))
      (should (contains? edges [:player :CharacterBody2D :inheritance]))
      (should (contains? edges [:enemies.enemy :Node :inheritance]))
      (should (contains? edges [:enemies.slime :enemies.enemy :inheritance]))
      (should (contains? edges [:player :enemies.slime :dependency]))
      (should (contains? edges [:player :addons.vendor.widget :dependency]))
      (should-not (some #(= :nope (second %)) edges))))

  (it "names a script from its path when class_name is missing"
    (let [dir (io/file (System/getProperty "java.io.tmpdir")
                       (str "uml-gd-anon-" (System/nanoTime)))]
      (spit-gd dir "mobs/base_enemy.gd" "extends Node\n")
      (let [g (graph/scan (graph/lookup :gdscript) dir {})
            c (first (filter #(= :mobs.base_enemy (:id %)) (:classes g)))]
        (should= "BaseEnemy" (:name c)))))

  (it "reads class_name extends on one line and a same-folder preload"
    (let [dir (io/file (System/getProperty "java.io.tmpdir")
                       (str "uml-gd-line-" (System/nanoTime)))]
      (spit-gd dir "actors/hero.gd"
               "class_name Hero extends Actor\nconst A = preload(\"actor.gd\")\n")
      (spit-gd dir "actors/actor.gd" "class_name Actor\nextends Node\n")
      (let [g (graph/scan (graph/lookup :gdscript) dir {})
            edges (edge-set g)
            classes (by-id g)]
        (should= "Hero" (:name (classes :actors.hero)))
        (should (contains? edges [:actors.hero :actors.actor :inheritance]))
        (should (contains? edges [:actors.hero :actors.actor :dependency]))
        (should (contains? edges [:actors.actor :Node :inheritance])))))

  (it "treats a string path extends as inheritance to the project script"
    (let [dir (io/file (System/getProperty "java.io.tmpdir")
                       (str "uml-gd-path-" (System/nanoTime)))]
      (spit-gd dir "enemies/slime.gd" "extends \"res://enemies/enemy.gd\"\n")
      (spit-gd dir "enemies/enemy.gd" "class_name Enemy\n")
      (let [edges (edge-set (graph/scan (graph/lookup :gdscript) dir {}))]
        (should (contains? edges [:enemies.slime :enemies.enemy :inheritance]))))))
