(ns cave.command)

(def command-go-directions {"north" :n "south" :s "east" :e "west" :w})

(defn go [direction-str current-loc locations]
  (let [direction (command-go-directions direction-str)]
    (if (contains? ( locations current-loc ) direction)
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
