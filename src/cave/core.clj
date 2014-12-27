(ns cave.core
 (:gen-class :main true))

(require '[clojure.string :as str]
         '[schema.core :as s]
         '[clojure.edn :as edn])

;Command maps
(def command-map {"exit" :exit "north" :n "south" :s "east" :e "west" :w})

;Schema definitions
(def LocationMap {s/Keyword {(s/required-key :name) s/Str (s/optional-key :description) s/Str s/Keyword s/Keyword}})


( defn load-test-locations []

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
  (let [clean-cmd (str/trim (str/lower-case cmd))
        lookup-result (command-map cmd)]
      (if (nil? lookup-result) :unrecognized lookup-result)))


(defn write-prompt [loc-key locations]
  (let [loc (loc-key locations)]
    (println (format "[%s]" (:name loc)))
    (if
      (contains? loc :description)
      (println "-- " (:description loc)))
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
                cmd (process-command ln)]

            (cond
             (directional-cmd? cmd) (if (contains? ( m @a) cmd) (swap! a (fn [x] (cmd (@a m)))) (println "Sorry, can't go that way!"))
             (= cmd :exit) (println "Goodbye!")
             :else (println "Unrecognized command"))
            
            (if (not (= cmd :exit))
              (recur a))))))

(defn -main
  "Generic Clojure-based text adventure game by Camden McFarlane and Keith McFarlane"
  [& args]
  (try
    (let [m (if (> (count args) 0) (load-and-validate (first args)) (load-and-validate))]
      (eval-loop m))
    (catch Exception e (println "An error occurred: " (.getMessage e)))))

