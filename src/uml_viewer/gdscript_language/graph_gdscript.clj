(ns uml-viewer.gdscript-language.graph-gdscript
  "GDScript LanguageGraph: one class per .gd file.

  Folders under the source root become dotted ids so the hierarchical
  viewer still drills by layer (`enemies/slime.gd` → `:enemies.slime`).
  `class_name` is the display name. `extends` is `:inheritance`.
  `preload` of a `.gd` script is `:dependency`. Godot engine classes
  and paths outside the scanned tree are foreign."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.graph :as graph]))

(defn- posix [path]
  (str/replace (str path) \\ \/))

(defn- source-files [root]
  (->> (file-seq (io/file root))
       (filter #(.isFile %))
       (filter #(re-find #"\.gd$" (.getName %)))
       (sort-by #(.getPath %))))

(defn- relativize [base file]
  (let [base-path (.getCanonicalPath (io/file base))
        file-path (.getCanonicalPath (io/file file))
        prefix (if (str/ends-with? base-path "/")
                 base-path
                 (str base-path "/"))]
    (if (str/starts-with? file-path prefix)
      (posix (subs file-path (count prefix)))
      (.getName file))))

(defn- module-ns [file]
  (let [cwd (.getCanonicalPath (io/file "."))
        fp (.getCanonicalPath file)
        prefix (if (str/ends-with? cwd "/") cwd (str cwd "/"))
        path (if (str/starts-with? fp prefix) (subs fp (count prefix)) fp)]
    (-> path posix (str/replace #"\.gd$" ""))))

(defn- path-id [rel]
  (-> rel
      (str/replace #"\.gd$" "")
      (str/replace #"\\" ".")
      (str/replace #"/" ".")
      keyword))

(defn- last-seg-name [id]
  (->> (str/split (last (str/split (name id) #"\.")) #"[_-]+")
       (remove str/blank?)
       (map str/capitalize)
       (str/join)))

(defn- strip-hash-comments [source]
  (let [sb (StringBuilder.)]
    (loop [i 0 in-str false q nil]
      (if (>= i (count source))
        (str sb)
        (let [ch (.charAt source i)]
          (cond
            (and in-str (= ch \\) (< (inc i) (count source)))
            (do (.append sb ch)
                (.append sb (.charAt source (inc i)))
                (recur (+ i 2) true q))

            (and in-str (= ch q))
            (do (.append sb ch) (recur (inc i) false nil))

            in-str
            (do (.append sb ch) (recur (inc i) true q))

            (or (= ch \") (= ch \'))
            (do (.append sb ch) (recur (inc i) true ch))

            (= ch \#)
            (let [nl (or (str/index-of source "\n" i) (count source))]
              (recur nl false nil))

            :else
            (do (.append sb ch) (recur (inc i) false nil))))))))

(defn- class-name-of [s]
  (second (re-find #"(?m)^class_name\s+([A-Za-z_][A-Za-z0-9_]*)" s)))

(defn- extends-of [s]
  (let [pick (fn [[_ path ident]] (or path ident))]
    (or (pick (re-find #"(?m)^extends\s+(?:[\"']([^\"']+)[\"']|([A-Za-z_][A-Za-z0-9_]*))" s))
        (pick (re-find #"(?m)^class_name\s+[A-Za-z_][A-Za-z0-9_]*\s+extends\s+(?:[\"']([^\"']+)[\"']|([A-Za-z_][A-Za-z0-9_]*))" s)))))

(defn- preloads-of [s]
  (mapv second (re-seq #"preload\s*\(\s*[\"']([^\"']+\.gd)[\"']" s)))

(defn- normalize-rel [path]
  (->> (str/split (posix path) #"/")
       (reduce (fn [acc p]
                 (cond
                   (or (str/blank? p) (= p ".")) acc
                   (= p "..") (if (seq acc) (pop acc) acc)
                   :else (conj acc p)))
               [])
       (str/join "/")))

(defn- script-ref? [ref]
  (boolean (and (string? ref) (re-find #"\.gd$" ref))))

(defn- strip-res [ref]
  (normalize-rel (-> ref
                     (str/replace #"^res://" "")
                     (str/replace #"^\\./" ""))))

(defn- beside [current-rel norm]
  (let [dir (let [i (str/last-index-of current-rel "/")]
              (if i (subs current-rel 0 i) ""))]
    (normalize-rel (if (str/blank? dir) norm (str dir "/" norm)))))

(defn- resolve-script [ref current-rel idx]
  (let [norm (strip-res ref)
        local (beside current-rel norm)]
    (or (get (:by-rel idx) norm)
        (get (:by-rel idx) local)
        (first (for [[rel id] (:by-rel idx)
                     :when (or (str/ends-with? rel norm)
                               (str/ends-with? norm rel))]
                 id)))))

(defn- resolve-ref [ref current-rel idx]
  (when (seq ref)
    (if (script-ref? ref)
      (resolve-script ref current-rel idx)
      (get (:by-name idx) ref))))

(defn- foreign-id [ref]
  (if (script-ref? ref)
    (path-id (strip-res ref))
    (keyword ref)))

(defn- parse-file [file root]
  (let [rel (relativize root file)
        text (strip-hash-comments (slurp file))
        cname (class-name-of text)
        id (path-id rel)]
    {:id id
     :name (or cname (last-seg-name id))
     :ns (module-ns file)
     :rel rel
     :class-name cname
     :extends (extends-of text)
     :preloads (preloads-of text)}))

(defn- project-index [parsed]
  {:by-rel (into {} (map (juxt :rel :id) parsed))
   :by-name (into {} (keep (fn [p]
                             (when-let [n (:class-name p)]
                               [n (:id p)]))
                           parsed))})

(defn- resolved [p idx]
  (let [inherit (when-let [ref (:extends p)]
                  (or (resolve-ref ref (:rel p) idx)
                      (foreign-id ref)))
        deps (mapv (fn [ref]
                     (or (resolve-ref ref (:rel p) idx)
                         (foreign-id ref)))
                   (:preloads p))]
    {:id (:id p)
     :inherit inherit
     :deps (->> deps (remove #(= % (:id p))) distinct vec)}))

(defn- as-edges [r]
  (concat
    (when-let [to (:inherit r)]
      (when (not= to (:id r))
        [{:from (:id r) :to to :kind :inheritance}]))
    (map (fn [to] {:from (:id r) :to to :kind :dependency}) (:deps r))))

(defn- foreign-class [id]
  {:id id
   :name (name id)
   :ns (name id)
   :foreign true})

(defn- project-class [p]
  {:id (:id p)
   :name (:name p)
   :ns (:ns p)})

(defrecord GDScriptGraph []
  graph/LanguageGraph
  (scan [_ root _opts]
    (let [parsed (mapv #(parse-file % root) (source-files root))
          idx (project-index parsed)
          resolved (mapv #(resolved % idx) parsed)
          project-ids (set (map :id parsed))
          foreign-ids (->> resolved
                           (mapcat (fn [r] (cond-> (:deps r)
                                             (:inherit r) (conj (:inherit r)))))
                           (remove project-ids)
                           distinct)
          classes (into (mapv project-class parsed)
                        (mapv foreign-class foreign-ids))
          edges (vec (mapcat as-edges resolved))]
      {:classes classes :edges edges})))

(def impl (->GDScriptGraph))

(graph/register! :gdscript impl)
