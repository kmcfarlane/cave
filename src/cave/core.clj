(ns cave.core
 (:gen-class :main true))

(require '[clojure.string :as str]
         '[schema.core :as s]
         '[clojure.edn :as edn]
         '[cave.command :as cmd])

;Command maps
(def command-verb-map {"exit" :exit              ;Leave the game
                       "go" :go                  ;Move to a new location
                       "inventory" :inventory})

;Schema definitions
(def LocationMap {(s/required-key :start-location) s/Keyword
                  (s/required-key :location-list) {s/Keyword {(s/required-key :name) s/Str (s/optional-key :description) s/Str s/Keyword s/Keyword}}})

;Regular expressions
(def command-structure-regex #"\s*([A-Za-z]+)(\s+([A-Za-z0-9]+))?\s*") ;Matches (white space)(command)(white space)(opt parameter)(white space)

(defn load-test-locations []

  {
   :start-location :test-loc
   :location-list
   { :test-loc {:name "Test loc" :description "Middle test loc" :e :east-loc :n :north-loc :w :west-loc :s :south-loc}
     :east-loc {:name "East loc" :w :test-loc}
     :west-loc {:name "West loc" :e :test-loc}
     :north-loc {:name "North loc" :s :test-loc}
    :south-loc {:name "South loc" :n :test-loc}}})

(defn process-command [cmd]
  (let [cmd-vec       (re-matches command-structure-regex cmd)
        [_ cmd-verb _ cmd-target] cmd-vec
        lookup-result (if (nil? cmd-verb) nil (command-verb-map cmd-verb))
        ]
    (if (nil? lookup-result)
      (vector :unrecognized)
      (vector lookup-result cmd-target)))) ;Returns vector with command and params or :unrecognized


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


(defn load-and-validate
  "Load and validate the location map. If a filename is provided, load map data from the file; otherwise, use test data."
  ([] (let [m (load-test-locations)]
    (validate-map m)            ;throws an exception if data doesn't validate
     m))                        ;return successfully loaded test location map
  ([file-path] (let [m (edn/read-string (slurp file-path))]
    (validate-map m)            ;throws an exception if data doesn't validate
     m)))                       ;return map loaded from disk


(defn eval-loop [m]

  (loop [locations (:location-list m)
         current-loc (atom (:start-location m))
         inventory (atom [:sword "Claymore"])] 
        (do
          (write-prompt @current-loc locations)
          
          (let [ln (read-line) 
                cmd-result (process-command ln)
                [cmd-verb cmd-target] cmd-result]

            (cond
             (= cmd-verb :go) (let [new-location (cmd/go cmd-target @current-loc locations)]
                                (if (nil? new-location)
                                  (println "Sorry, can't go that way!")
                                  (swap! current-loc  (fn [x] new-location))))
             (= cmd-verb :inventory) (println (cmd/inventory @inventory)) 
             (= cmd-verb :exit) (println "Goodbye!")
             :else (println "Unrecognized command"))
            
            (if (not (= cmd-verb :exit))
              (recur locations current-loc inventory ))))))

(defn -main
  "Generic Clojure-based text adventure game by Camden McFarlane and Keith McFarlane"
  [& args]
  (try
    (let [m (if (> (count args) 0) (load-and-validate (first args)) (load-and-validate))]
      (eval-loop m))
    (catch Exception e (println "An error occurred: " (.getMessage e)))))

