(ns uml-viewer.gdscript-language.source-gdscript
  "GDScript LanguageSource: path from :ns + top-level func slice."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.source :as source])
  (:import [java.util.regex Pattern]))

(defn- as-rel [ns-name]
  (let [s (-> (str ns-name) (str/replace #"\.gd$" ""))]
    (if (or (str/includes? s "/") (str/starts-with? s "/"))
      s
      (str/replace s "." "/"))))

(defn ns->source-path
  "Resolve `ns-name` to an existing `.gd` file, or nil.

  Accepts a slash path (`enemies/slime`), a dotted id (`enemies.slime`),
  or either form with a `.gd` suffix."
  [ns-name]
  (when ns-name
    (let [rel (as-rel ns-name)
          candidates [(str rel ".gd") rel]]
      (first (filter #(.exists (io/file %)) candidates)))))

(defn- member-start
  [source member-name]
  (when (and source member-name)
    (let [p (Pattern/compile
              (str "(?m)^(?:static\\s+|async\\s+)*func\\s+"
                   (Pattern/quote (str member-name))
                   "(?=[\\s\\(]|$)"))
          m (.matcher p source)]
      (when (.find m)
        (.start m)))))

(defn- member-end
  [source start]
  (let [nl (str/index-of source "\n" start)
        after (if nl (inc nl) (count source))
        rest (subs source after)
        m (re-matcher #"(?m)^(?:static\s+|async\s+)*func\s|^class_name\s|^extends\s|^class\s|^signal\s" rest)]
    (if (.find m)
      (+ after (.start m))
      (count source))))

(defn extract-member
  "Source text of the top-level `func` named `member-name`, or nil."
  [source member-name]
  (when-let [start (member-start source member-name)]
    (str/trimr (subs source start (member-end source start)))))

(defn member-line
  "1-based line of `member-name` in `source`, or nil."
  [source member-name]
  (when-let [start (member-start source member-name)]
    (inc (count (re-seq #"\n" (subs source 0 start))))))

(defrecord GDScriptSource []
  source/LanguageSource
  (locate [_ ident]
    (ns->source-path (:ns ident)))
  (extract [_ source ident]
    (extract-member source (:name ident)))
  (start-line [_ source ident]
    (member-line source (:name ident)))
  (title [_ ident]
    (str (:ns ident) "/" (:name ident))))

(def impl (->GDScriptSource))

(source/register! :gdscript impl)
