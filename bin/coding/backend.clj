(ns coding.backend
  (:require [abracad.avro :as avro]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [me.raynes.fs :as f]
            [clojure.set :as set])
  (:import [java.util.jar JarFile JarEntry])
 (:gen-class :main true
      :name coding.backend
      :methods [#^{:static true} [extract_dir_from_jar [String String String]void]
                #^{:static true} [all_files [] clojure.lang.PersistentVector]
                #^{:static true} [validateid [] Boolean]
                ])
  )

;;global declarations
(def listoffiles
    (file-seq (io/file "C:\\Users\\ss056651\\new-workspace\\frontend\\new dir\\algorithms\\registry-programs"))
    )   
  (def readyfiles (filter #(.isFile %) listoffiles))

  
  
;;code to extract a jar
(defn extract_dir_from_jar
    "Takes the string path of a jar, a dir name inside that jar and a destination
  dir, and copies the from dir to the to dir."
    [jar-dir from to]
    (let [jar (JarFile. jar-dir)]
      (doseq [^JarEntry file (enumeration-seq (.entries jar))]
        (if (.startsWith (.getName file) from)
          (let [f (f/file to (.getName file))]
            (if (.isDirectory file)
              (f/mkdir f)
              (do (f/mkdirs (f/parent f))
                (with-open [is (.getInputStream jar file)
                            os (io/output-stream f)]
                  (io/copy is os)))))))))


;;Code to read all the files
 (defn all_files[] 
  
  (def final_data2 [])
    (doseq [k readyfiles]  
    (def actual_data [])                  
    (let [temp-data (edn/read-string (clojure.string/replace (slurp k) #"#synapse.clinical.programs.ProgramDef|#synapse.clinical.eventprograms.EventProgramDef" " "))]
      (def actual_data (doseq [n (for [y (get temp-data :concept-context-relationships)] 
                                   (select-keys y [:aliases :id]))] 
                         (def final_data2 (conj final_data2 n))))))
    
    (let [after_grp (group-by :id final_data2)
        after_seq(seq after_grp)]
   (def ednset (into {} (for [s after_seq]
                          (do
     (let [value (second s)
           key (first s)
           sets (mapv #(get % :aliases) value)
                    edn (reduce into #{} sets)]
            [key edn]
       )))))
    )

;;code to read avro files
    (let [file_url        (clojure.java.io/file "C:\\Users\\ss056651\\Documents\\Discernontology\\v1\\versionedcontents")
      fs              (file-seq file_url)
      ready_files     (filter #(.isFile %) fs)
      latest_version  (last ready_files)]     

  (def avroset (into {} (for [i ready_files]
                           (do
    (let [data       (with-open [adf (avro/data-file-reader i)]
                             (doall (seq adf)))
         ids          (get (first data) :id)
         concepts     (select-keys (first data) [:concepts])
         myvec        (first (vals concepts))
         set          (conj [] (map #(get % :aliases) myvec))
         avro         (into #{} (re-seq #"\w+" (str set)))]
                 [ids avro]
  ))))))    

    (def test-map {}) 
    (def finaldata (into [] (for [i ednset
          j avroset]
      (if (= (first i) (first j))
       (if (= (empty? (set/difference (second i)(second j))) true)
         (do (assoc test-map  :aliases "nil" :id (first j)))
         (do (assoc test-map  :aliases (set/difference (second i)(second j)) :id (first j)))
                            )))))
    (def final (into [] (remove nil? finaldata)))
    (vec final))
     
(defn validateid[] 
  
  (boolean (= (keys ednset) (keys avroset)))
   )

      (defn -validateid[]
  "A Java-callable wrapper around the 'validateid' function."
  (validateid))
      
      (defn -all_files[]
  "A Java-callable wrapper around the 'all_files' function."
  (all_files))
      
      (defn -extract_dir_from_jar[jar-dir from to]
  "A Java-callable wrapper around the 'extract_dir_from_jar' function."
  (extract_dir_from_jar "C:\\Users\\ss056651\\Documents\\app-rules-5.0.0.jar" "algorithms" "new dir"))
      
      (defn -main []
      (extract_dir_from_jar "C:\\Users\\ss056651\\Documents\\app-rules-5.0.0.jar" "algorithms" "new dir")
      (println  (all_files))
      (println  (validateid)))