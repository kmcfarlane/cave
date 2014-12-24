
(ns cave.core
 (:gen-class :main true))

(require '[clojure.string :as str])


(defn load-map []
  { :great-hall {:name "Great Hall" :items ["Sword" "Spear" "Chicken McNuggets"] s: :study e: :master-bedroom} :master-bedroom {:name "Master Bedroom" w: :great-hall s: kitchen w: :master-bathroom} :master-bathroom {:name "Master Bathroom"}
   :study {:name "Study"} :kitchen {:name "Kitchen"} :guest-bedroom {:name "Guest Bedroom"}
   :gasrage {:name "Garage"} :laundry-room {:name "Laundry Room"} :guest-bathroom {:name "Guest Bathroom"}
    }


  (defn process-command [cmd]
    (let [clean-cmd (str/trim (str/lower-case cmd))]
      (cond 
       (= clean-cmd "exit") :exit
       (= clean-cmd "north") :north
       (= clean-cmd "south") :south
       (= clean-cmd "east") :east
       (= clean-cmd "west") :west
       :else :unrecognized))))


(defn eval-loop []
      (loop [m (load-map) 
      	     a (atom ((m (rand-int 3)) (rand-int 3)))] 
	       (do
      	       (println (format "(%s)" (:name @a)))
               (print "Your command, sire? ")
	       (flush)
	       (let [c (count m) 
	       	     r (count (first m))
		     ln (read-line) 
		     cmd (process-command ln)]
		     (cond
                      (= cmd :room-change)
                      (let [rndc (rand-int c) rndr (rand-int r) itm ] (swap! a (fn [x] ((m rndc ) rndr))))
			(= cmd :exit) (println "Goodbye!")
			:else (println "Unrecognized command"))

               	    (if (not (= cmd :exit))
	       	    	(recur m a))))))



(defn -main
  "I don't do a whole lot."
  [& args]
	(eval-loop))

