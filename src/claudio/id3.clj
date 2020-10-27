(ns claudio.id3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.set :as set])
  (:import org.jaudiotagger.audio.AudioFileIO
           org.jaudiotagger.tag.FieldKey))

(def ^{:private true} field-keys
  (let [constants (.getEnumConstants org.jaudiotagger.tag.FieldKey)]
    (map #(.name %) constants)))

(defn- ->constant [k]
  (.get (.getField org.jaudiotagger.tag.FieldKey k) nil))

(defn- keywordify [k]
  (keyword (string/replace (string/lower-case k) #"_" "-")))

(defn- constantify [k]
  (string/replace (string/upper-case (name k)) #"-" "_"))

(defn- retrieve-field [tag k]
  (let [v (.getFirst tag (->constant k))]
    (when-not (empty? v)
      (vector (keywordify k) v))))

;;
;; API
;;
(defn read-tag
  "Given an mp3 File f, returns a map of ID3 tag or nil if none exists"
  [f]
  (when-let [tag (.getTag (org.jaudiotagger.audio.AudioFileIO/read f))]
    (into {} (remove nil? (map (partial retrieve-field tag) field-keys)))))

(defn write-tag!
  "Given a File f and fieldkey-string pairs kvs, modifies the file with the new value.
Field keys can be given Clojure-style, as lower-case, hyphenated keywords, or as
upper-case strings. Keys must correspond to a valid field key category or an
exception will be thrown.

To delete a field key, provide nil as its corresponding value.

Returns the updated tag map."
  [f & kvs]
  (when-not (empty? kvs)
    (when-not (even? (count kvs))
      (throw (Exception. "Tag-value pairs must an even number")))
    (let [audio-file (org.jaudiotagger.audio.AudioFileIO/read f)]
      (when-let [tag (.getTagOrCreateAndSetDefault audio-file)]
        (doseq [[k v] (partition 2 kvs)]
          (let [field-key (->constant (or (and (keyword? k) (constantify k)) k))]
            (if v
              (.setField tag field-key v)
              (.deleteField tag field-key))))
        (.commit audio-file)
        (read-tag f)))))

(defn read-path [path]
  (for [f (file-seq (io/file path))
        :when (and (.isFile f)
                   (= "mp3" (last (str/split (str f) #"\."))))
        :let [path (.toPath f)
              fn (str (.getFileName path))
              tag (try (read-tag f)
                       (catch org.jaudiotagger.audio.exceptions.CannotReadException e nil)
                       (catch java.lang.UnsupportedOperationException e nil))
              file-bare (subs fn 0 (str/last-index-of fn "."))
              file-for-num (str/replace file-bare #"\d{4,}" "")
              [num-pre num-post] (str/split file-for-num #"(?!^)\s*(\d\d\d?)(?!th)\s*" 2)
              num-pre (when-not (= num-pre file-for-num) num-pre)
              num (first (re-find  #"(\d\d\d?)(?!th|st|nd| -)" file-for-num))
              [dash-pre dash-post] (str/split file-bare #"\s+-+\s+" 2)
              dash-pre (when-not (= num-pre file-bare) dash-pre)
              dirname (str (.getFileName (.getParent path)))
              parent-name (str (.getFileName (.getParent (.getParent path))))
              [dir-pre-dash dir-post-dash] (str/split dirname #"\s+-+\s+" 2)
              dir-pre-dash (when-not (= dir-pre-dash dirname) dir-pre-dash)]]
    (assoc tag
           :file/bare file-bare
           :file/num0 num-pre
           :file/num2 num-post
           :file/num1 (when num (last (re-find #"^0*(.*)" num)))
           :file/dash0 dash-pre
           :file/dash1 dash-post
           :dir/dash0 dir-pre-dash
           :dir/dash1 dir-post-dash
           :z/dirname dirname
           :z/parent parent-name
           :z/filename fn
           :z/path (str path))))

(defn read-tags [path]
  (for [f (file-seq (io/file path))
        :when (and (.isFile f)
                   (= "mp3" (last (str/split (str f) #"\."))))]
    (assoc (try (read-tag f)
                (catch org.jaudiotagger.audio.exceptions.CannotReadException e nil)
                (catch java.lang.UnsupportedOperationException e nil))
           :z/path (str (.toPath f)))))

(defn group-count
  ([a]
   (reduce (fn [r i]
             (update r i (fnil inc 0)))
           {}
           a))
  ([f a]
   (reduce (fn [r i]
             (update r (f i) (fnil inc 0)))
           {}
           a)))

(defn most-frequent [f a]
  (when (seq a)
    (let [counted (set/map-invert (group-count f a))]
      (counted (apply max (keys counted))))))

(defn clean [tags]
  (->> tags
       (filter val)
       (remove #(= "" (val %)))
       (remove #(and (= :file/num0 (key %))
                     (= "Ballads" (val %))))
       (into {})))

(def defaults {:track-total nil
               :encoder nil
               :media nil
               :year nil
               :album-artist nil
               :comment nil})

(defn retag [tags]
  (for [album-tags (->> tags
                        (map clean)
                        (group-by :z/dirname)
                        vals)
        :let [album {:album (:z/dirname (first album-tags))
                     :album-artist (most-frequent (some-fn :dir/dash0
                                                           :artist
                                                           :file/num2)
                                                  album-tags)
                     :genre (:z/parent (first album-tags))}
              titles? (some :file/num2 album-tags)]
        tag album-tags
        :let [artist ((some-fn :file/num0 :artist
                               #(let [a (:dir/dash0 %)] (when-not (#{"Various" "Unknown"} a) a))) tag)
              artist (if (and (some #{\,} (:album-artist album))
                              (#{"Anon" "Anonymous"} artist))
                       (:album-artist album)
                       artist)]]
    [(:z/path tag)
     (merge defaults
            album
            {:track (:file/num1 tag)
             :title (if titles? (:file/num2 tag) (:file/bare tag))
             :grouping (when titles? (when-not (:file/num2 tag) "commentary"))
             :artist artist})]))

(defn update-tags! [retagged]
  (doseq [[path tag] retagged]
    (apply write-tag! (io/file path) (flatten (into [] tag)))))

(defn write-csv [path row-data]
  (let [columns (->> row-data
                     (map keys)
                     flatten
                     distinct
                     (sort-by str)
                     (into []))
        headers (map str columns)
        rows (map #(map % columns) row-data)]
    (with-open [file (io/writer path)]
      (csv/write-csv file (cons headers rows)))))

(comment
  (def path "/Users/dw/Audio Books Poetry/Epic Poetry")
  (def originals (read-path path))
  (write-csv (str path "/poetry.csv") originals)
  (def tagged (retag originals))
  (def tagged' (map (fn [[path tag]] (assoc tag :z/path path)) tagged))
  (write-csv (str path "/retag.csv") tagged')
  (keep (fn [[o t]]
          (let [f (remove #(do
                             (prn (o (key %)) (val %))
                             (= (o (key %)) (val %)))
                          t)]
            (when (seq f)
              (assoc (into {} f) :z/path (:z/path o)))))
        (take 10
              (map vector (read-tags path) tagged')))

  (filter #(= 1 (count %))(read-tags path))

  (take 10 originals)

  (update-tags! tagged))

