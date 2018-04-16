(ns coding.testcode
  (:require [abracad.avro :as avro]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [me.raynes.fs :as f]
            [clojure.set :as set])
  (:import [java.util.jar JarFile JarEntry]))


;;global declarations
(def listoffiles
    (file-seq (io/file "C:\\Users\\ss056651\\new-workspace\\frontend\\new dir\\algorithms\\registry-programs"))
    )   
  (def readyfiles (filter #(.isFile %) listoffiles))
 

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
  ))))) 
  )    
   
(def test-map {}) 
    (def finaldata (into [] (for [i ednset
          j avroset]
      (when (= (first i) (first j)) 
       (if (= (empty? (set/difference (second i)(second j))) true)
         (do (assoc test-map  :aliases "nil" :id (first j)))
         (do (assoc test-map  :aliases (set/difference (second i)(second j)) :id (first j)))
                     )))))
    (def final (into [] (remove nil? finaldata)))   
 
    #_(if (= (keys ednset) (keys avroset))
    (def flag 1)) 