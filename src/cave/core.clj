
(ns cave.core
 (:gen-class :main true))

(require '[clojure.string :as str]
         '[schema.core :as s])

;Schema definitions
(def LocationMap {s/Keyword {(s/required-key :name) s/Str (s/optional-key :description) s/Str s/Keyword s/Keyword}})


( defn load-locations []

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
   
   }
  
  )



(defn process-command [cmd]
  (let [clean-cmd (str/trim (str/lower-case cmd))]
    (cond 
     (= clean-cmd "exit") :exit
     (= clean-cmd "north") :n
     (= clean-cmd "south") :s
     (= clean-cmd "east") :e
     (= clean-cmd "west") :w
     :else :unrecognized)))

(defn write-prompt [loc-key locations]
  (let [loc (loc-key locations)]
    (println (format "[%s]" (:name loc)))
    (if
      (contains? loc :description)
      (println "-- " (:description loc)))
    (print "Your command, sire? ")
    (flush)
  )
)

(defn load-and-validate []
  (let [m (load-locations)]
    (s/validate LocationMap m) ;throws an exception if data doesn't validate
    m )  ;return successfully loaded location map
)
(defn eval-loop [m]

      (loop [a (atom :town-square)] 
        (do
          (write-prompt @a m)
          
          (let [ln (read-line) 
                cmd (process-command ln)]

            (cond
             (contains? #{:n :s :e :w} cmd) (if (contains? ( m @a) cmd) (swap! a (fn [x] (cmd (@a m)))) (println "Sorry, can't go that way!"))
             (= cmd :exit) (println "Goodbye!")
             :else (println "Unrecognized command"))
            
            (if (not (= cmd :exit))
              (recur a))
                 )
               )
        )
)

(defn -main
  "Generic Clojure-based text adventure game by Camden McFarlane and Keith McFarlane"
  [& args]
  (try
    (let [m (load-and-validate)]
      (eval-loop m))
    (catch Exception e (println "Could not read location data."))))

