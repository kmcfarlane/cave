
(ns cave.core
 (:gen-class :main true))

(require '[clojure.string :as str])


( defn load-map []
  { :great-hall {:name "Great Hall" :items ["Sword" "Spear" "Chicken McNuggets"] :s :study :e :master-bedroom}
   :master-bedroom {:name "Master Bedroom" :w :great-hall :s :kitchen :e :master-bathroom}
   :master-bathroom {:name "Master Bathroom"}
   :study {:name "Study" :s :great-hall}
   :kitchen {:name "Kitchen"}
   :guest-bedroom {:name "Guest Bedroom"}
   :garage {:name "Garage"}
   :laundry-room {:name "Laundry Room"}
   :guest-bathroom {:name "Guest Bathroom"}
   })

( defn load-map-2 []

  { :town-square {:name "Town Square" :e :great-hall}
    :great-hall  {:name "Great Hall" :w :town-square :e :throne-room :n :master-bedroom :s :study}
    :master-bedroom {:name "Master Bedroom" :e :master-bathroom :s :great-hall}
    :study {:name "Study" :n :great-hall}
    :throne-room {:name "Throne Room" :w :great-hall}
    :master-bathroom {:name "Master Bathroom" :w :master-bedroom}
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


(defn eval-loop []
      (loop [m (load-map) 
      	     a (atom :great-hall) ] 
	       (do
                 (println (format "(%s)" (:name (@a m))))
                 (print "Your command, sire? ")
	         (flush)
	         (let [ln (read-line) 
		       cmd (process-command ln)]
		       (cond
                        (contains? #{:n :s :e :w} cmd) (swap! a (fn [x] (cmd (@a m))))
                      	 (= cmd :exit) (println "Goodbye!")
			 :else (println "Unrecognized command"))

               	         (if (not (= cmd :exit))
                           (recur m a))
                 )
               )
       )
)



(defn -main
  "I don't do a whole lot."
  [& args]
	(eval-loop))

