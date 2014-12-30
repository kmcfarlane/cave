;Copyright 2014 Keith McFarlane
;
; cave/core.clj
; Main game loop and map loading functions
;
;Licensed under the Apache License, Version 2.0 (the "License");
;you may not use this file except in compliance with the License.
;You may obtain a copy of the License at
;
;    http://www.apache.org/licenses/LICENSE-2.0
;
;Unless required by applicable law or agreed to in writing, software
;distributed under the License is distributed on an "AS IS" BASIS,
;WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;See the License for the specific language governing permissions and
;limitations under the License.

(ns cave.core
 (:gen-class :main true))

(require '[clojure.string :as str]
         '[schema.core :as s]
         '[clojure.edn :as edn]
         '[cave.command :as cmd])

;Schema definitions
(def LocationMap {(s/required-key :start-location) s/Keyword
                  (s/required-key :location-list)
                  {s/Keyword {(s/required-key :name) s/Str (s/optional-key :description) s/Str s/Keyword s/Keyword
                              (s/optional-key :visited) s/Bool (s/optional-key :inventory) #{s/Str}
                               (s/optional-key :access-requires-use)
                               {s/Str s/Str}}}
                  (s/required-key :items-list) {s/Str {(s/required-key :name) s/Str
                                                (s/required-key :description) s/Str}}})

(defn load-test-locations []

  {
   :start-location :test-loc
   :location-list
   { :test-loc {:name "Test loc" :description "Middle test loc" :e :east-loc :n :north-loc :w :west-loc :s :south-loc}
     :east-loc {:name "East loc" :w :test-loc}
     :west-loc {:name "West loc" :e :test-loc}
     :north-loc {:name "North loc" :s :test-loc}
    :south-loc {:name "South loc" :n :test-loc}}})

(defn write-description [loc items-list]
  (println (str "--" \newline (:description loc) \newline "--" \newline "Items:" \newline 
                (if (> (count (:inventory loc)) 0)
                  (str (apply str (cmd/inventory (:inventory loc) items-list)))
                  "None\n")
                "--")))


(defn write-prompt [loc-key locations m]
  (let [loc (loc-key locations)]
    (println)
    (println (format "[%s]" (:name loc)))
    (if
      (and (contains? loc :description) (not (loc :visited)))
      (write-description loc (:items-list m)))
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


(defn eval-loop
"Takes a map (structured as defined by the LocationMap schema) and executes the main game loop,
storing state modification over time in several atoms."
  [m]
  (loop [locations (atom (:location-list m))
         current-loc (atom (:start-location m))
         inventory (atom #{})] 
        (do
          (write-prompt @current-loc @locations m)
          
          (let [ln (read-line)
                cmd-result (if (= (str/trim ln) "") [:nothing] (cmd/process-command ln))
                [cmd-verb cmd-target] cmd-result]
            (swap! locations #(assoc-in % [@current-loc :visited] true))
            (cond
             (= cmd-verb :go) (let [new-location (cmd/go cmd-target @current-loc @locations)]
                                (if (nil? new-location)
                                  (println "Sorry, can't go that way!")
                                  (swap! current-loc  (fn [x] new-location))))
             (= cmd-verb :inventory) (println (apply str (cmd/inventory @inventory (:items-list m))))
             (= cmd-verb :describe)  (swap! locations #(assoc-in % [@current-loc :visited] false))
             (= cmd-verb :take) (let [[new-inv new-map] (cmd/take-cmd cmd-target @inventory @current-loc @locations)]
                                  (if (string? new-inv) (println new-inv)
                                      (do
                                        (swap! locations (fn [x] new-map))
                                        (swap! inventory (fn [x] new-inv)))))
             (= cmd-verb :leave) (let [[new-inv new-map] (cmd/leave cmd-target @inventory @current-loc @locations)]
                                  (if (string? new-inv) (println new-inv)
                                      (do
                                        (swap! locations (fn [x] new-map))
                                        (swap! inventory (fn [x] new-inv)))))
             (= cmd-verb :nothing) nil
             (= cmd-verb :exit) (println "Goodbye!")
             :else (println "Unrecognized command"))
            
            (if (not (= cmd-verb :exit))
              (recur locations 
                     current-loc
                     inventory ))))))

(defn -main
  "Generic Clojure-based text adventure game by Camden McFarlane and Keith McFarlane"
  [& args]
  (try
    (let [m (if (> (count args) 0) (load-and-validate (first args)) (load-and-validate))]
      (eval-loop m))
    (catch Exception e (println "An error occurred: " (.getMessage e)))))

