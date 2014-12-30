(ns cave.command)

(def command-go-directions {"north" :n "south" :s "east" :e "west" :w})

(defn go [direction-str current-loc locations]
  (let [direction (command-go-directions direction-str)]
    (if (contains? ( locations current-loc ) direction)
      (direction (current-loc locations)) nil )))

(defn inventory [inventory, items-list]
  (for [item inventory
        :let [n (:name (item items-list))
              d (:description (item items-list))]
        :when (contains? items-list item)]
        (format "%-12s: %s\n" n d)))
