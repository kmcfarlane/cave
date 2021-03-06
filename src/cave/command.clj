;Copyright 2014 Keith McFarlane
;
; cave/command.clj
; Functions for command interpretation and processing
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

(ns cave.command)

;Command mappings
(def command-verb-map {"" :nothing               ;Null command
                       "exit" :exit              ;Leave the game
                       "describe" :describe      ;Print full description of current location
                       "go" :go                  ;Move to a new location
                       "take" :take              ;Remove an item fromcurrent room and add to player inventory
                       "leave" :leave            ;Remove item from player inventory and place in this room
                       "use"  :use               ;Make use of an item in player inventory
                       "inventory" :inventory})  ;Display player inventory

(def command-go-directions {"north" :n "south" :s "east" :e "west" :w})

;Regular expressions
(def command-structure-regex #"\s*([A-Za-z]+)(\s+([A-Za-z0-9]+))?\s*")
                                        ;Matches (white space)(command)(white space)(opt parameter)(white space)



(defn command-allowed?
"For a given location and requested direction, determine whether or not travel is currently allowed."
[direction current-loc]
  (let [constraints (:items (direction (:constraints current-loc)))
        inventory (get current-loc :inventory #{})]
    (if (every? inventory constraints)
      (let [allowed-text (get (direction (:constraints current-loc)) :allowed-text nil)]
        (if (not (nil? allowed-text)) (println allowed-text))
        true)
      (do (println (:disallowed-text (direction (:constraints current-loc)))) false))))

(defn go
"Computes new map location based on given map and requrested direction."
[direction-str current-loc locations]
  (let [direction (command-go-directions direction-str)]
    (if (and (contains? ( locations current-loc ) direction)(command-allowed? direction (locations current-loc)))
      (direction (current-loc locations)) nil )))

(defn inventory
"Builds a list of strings describing all items in provided inventory set."
[inventory items-list]
  (if (> (count inventory) 0) 
    (for [item inventory
        :let [n (:name (items-list item))
              d (:description (items-list item))]
        :when (contains? items-list item)]
      (format "%-12s: %s\n" n d))
    '("You aren't holding any items.")))

(defn take-cmd
"If the desired item exists in the location's inventory, 'move' it to the player's
inventory, returning modified version of both the map and player inventory. Return
an error message if this can't be done."
  [item inventory location m]
  (let [loc-inventory (:inventory (location m))
        found-item (contains? loc-inventory item)]
    (if found-item [(conj inventory item) (assoc-in m [location :inventory] (into #{} (remove #(= % item) loc-inventory)))]
      ["That item isn't here."])))

(defn leave
"If the item is held by the player, place it in the current room's inventory."
[item inventory location m]
  (let [found-item (contains? inventory item)]
    (if found-item [(into #{} (remove #(= % item) inventory))
                    (update-in m [location :inventory] #(into #{} (conj %1 %2)) item)]
        ["You do not have that item."])))

(defn process-command
"Extract command verb and parameters, returning all in a vector."
[cmd]
  (let [cmd-vec       (re-matches command-structure-regex cmd)
        [_ cmd-verb _ cmd-target] cmd-vec
        lookup-result (if (nil? cmd-verb) nil (command-verb-map cmd-verb))
        ]
    (if (nil? lookup-result)
      [:unrecognized]
      [lookup-result cmd-target]))) ;Returns vector with command and target or :unrecognized

