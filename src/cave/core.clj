(ns cave.core
 (:gen-class :main true))

(require '[clojure.string :as str]
         '[schema.core :as s]
         '[clojure.edn :as edn])

;Command maps
(def command-verb-map {"exit" :exit "go" :go})
(def command-go-directions {"north" :n "south" :s "east" :e "west" :w})

;Schema definitions
(def LocationMap {s/Keyword {(s/required-key :name) s/Str (s/optional-key :description) s/Str s/Keyword s/Keyword}})

;Regular expressions
(def command-structure-regex #"\s*([A-Za-z]+)(\s+([A-Za-z0-9]+))?\s*") ;Matches (white space)(command)(white space)(opt parameter)(white space)

(defn load-test-locations []

  {
   :town-square {:name "Town Square" :description "Center of the city!" :e :great-hall :n :Blacksmiths-shop :w :plains}
   :great-hall  {:name "Great Hall" :w :town-square :e :throne-room :n :master-bedroom :s :study}
   :master-bedroom {:name "Master Bedroom" :e :master-bathroom :s :great-hall}
   :study {:name "Study" :n :great-hall}
   :throne-room {:name "Throne Room" :w :great-hall}
   :master-bathroom {:name "Master Bathroom" :w :master-bedroom}
   :Blacksmiths-shop {:name "Blacksmith's shop" :s :town-square}
   :plains {:name "Plains" :e :town-square :n :forest :s :cave}
   :forest {:name "Forest" :s :plains}
   :cave {:name "Cave" :n :plains}
   })


(defn process-command [cmd]
  (let [cmd-vec       (re-matches command-structure-regex cmd)
        lookup-result (if (nil? cmd-vec) nil (command-verb-map (cmd-vec 1)))] ;Second vector position is verb
    (if (nil? lookup-result)
      (vector :unrecognized)
      (vector lookup-result (into [] (rest (rest (rest cmd-vec)))))))) ;Returns vector with command and params or :unrecognized


(defn write-prompt [loc-key locations]
  (let [loc (loc-key locations)]
    (println)
    (println (format "[%s]" (:name loc)))
    (if
      (contains? loc :description)
      (println "-- " (:description loc)))
    (println)
    (print "Your command, sire? ")
    (flush)))


(defn validate-map [m]
  (s/validate LocationMap m))


(defn directional-cmd? [k]
  (contains? #{:n :s :e :w} k))


(defn load-and-validate
  "Load and validate the location map. If a filename is provided, load mapa data from the file; otherwise, use test data."
  ([] (let [m (load-test-locations)]
    (validate-map m)            ;throws an exception if data doesn't validate
     m))                        ;return successfully loaded test location map
  ([file-path] (let [m (edn/read-string (slurp file-path))]
    (validate-map m)            ;throws an exception if data doesn't validate
     m)))                       ;return map loaded from disk


(defn eval-loop [m]

      (loop [a (atom :town-square)] 
        (do
          (write-prompt @a m)
          
          (let [ln (read-line) 
                cmd-result (process-command ln)
                cmd-verb (cmd-result 0)]

            (cond
             (= cmd-verb :go) (let [direction (command-go-directions ((cmd-result 1) 0))]
                                      (if (contains? ( m @a) direction) (swap! a (fn [x] (direction (@a m)))) (println "Sorry, can't go that way!")))
             (= cmd-verb :exit) (println "Goodbye!")
             :else (println "Unrecognized command"))
            
            (if (not (= cmd-verb :exit))
              (recur a))))))

(defn -main
  "Generic Clojure-based text adventure game by Camden McFarlane and Keith McFarlane"
  [& args]
  (try
    (let [m (if (> (count args) 0) (load-and-validate (first args)) (load-and-validate))]
      (eval-loop m))
    (catch Exception e (println "An error occurred: " (.getMessage e)))))

