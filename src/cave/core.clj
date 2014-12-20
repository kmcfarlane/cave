
(ns cave.core
 (:gen-class :main true))

(require '[clojure.string :as str])


(defn load-map []
      [
	[{:name "Great Hall" :items ["Sword" "Spear" "Chicken McNuggets"]} {:name "Master Bedroom"} {:name "Master Bathroom"}]
	[{:name "Study"} {:name "Kitchen"} {:name "Guest Bedroom"}]
	[{:name "Garage"} {:name "Laundry Room"} {:name "Guest Bathroom"}]
      ])


(defn process-command [cmd]
      (let [clean-cmd (str/trim (str/lower-case cmd))]
         (cond 
                (= clean-cmd "exit") :exit
		(= clean-cmd "move") :room-change
                 :else :unrecognized)))


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

