(ns uml-viewer.main.ir-generator
  (:require [uml-viewer.clojure-language.graph-clojure]
            [uml-viewer.gdscript-language.graph-gdscript]
            [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.graph :as graph])
  (:gen-class))

(defn -main [& args]
  (let [policy-path (or (first args) "examples/uml-viewer.policy.edn")
        out (second args)
        policy (ir-generator/read-policy policy-path)
        lang (or (:lang policy) :clojure)
        impl (graph/lookup lang)]
    (when-not impl
      (throw (ex-info (str "no LanguageGraph for " lang) {:lang lang})))
    (println "Wrote" (ir-generator/generate impl policy-path out))))
