(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
  (Math/pow sum sum)))


(defn spiff [v]
  (if (>= (count v) 3)
  (+ (get v 0) (get v 2))
  (str \?)))

(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[xp1 yp1] [xp2 yp2]] rectangle]
        (- xp2 xp1)))

(defn height [rectangle]
  (let [[[xp1 yp1] [xp2 yp2]] rectangle]
        (- yp2 yp1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))


(defn contains-point? [rectangle point]
  (let [[[xp1 yp1] [xp2 yp2]] rectangle
       [x y] point]
      (and (<= xp1 x xp2) (<= yp1 y yp2))))


(defn contains-rectangle? [outer inner]
  (let [[[xo1 yo1] [xo2 yo2]] outer
        [[xi1 yi1] [xi2 yi2]] inner]
      (and (<= xo1 xi1 xi2 xo2) (<= yo1 yi1 yi2 yo2))))


(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))


(defn multiple-authors? [book]
  (> (count (get book :authors)) 1))

(defn add-author [book new-author]
  (let [authors (conj (get book :authors) new-author)]
    (assoc book :authors authors)))


(defn alive? [author]
  (not (contains? author :death-year)))

(defn cont [x]
  (count x))

(defn element-lengths [collection]
  (map cont collection))

(defn second-elements [collection]
  (let [take-sec (fn [x] (get x 1))]
    (map take-sec collection)))

(defn titles [books]
  (let [take-title (fn [x] (get x :title))]
    (map take-title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [another (set a-seq)]
    (not (= (count another) (count a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (let [[b1 b2 b3 b4] books]
    (apply clojure.set/union [(:authors b1)
                          (:authors b2)
                          (:authors b3)
                          (:authors b4)])))

(defn all-author-names [books]
  (let [autores (fn [a] (map :name a))]
    (set (autores (authors books)))))

(defn author->string [author]
  (let [name (:name author)
      yearb (:birth-year author)
      yeard (:death-year author)]
    (if (contains? author :birth-year)
      (apply str name " (" yearb " - " yeard ")")
      (apply str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
      names (authors->string (:authors book))]
      (apply str title ", written by "  names)))

(defn books->string [books]
  (let [numb (count books)
      autor (apply str (map book->string books))
      autors (apply str (interpose ". " (map book->string books)))  ]
    (if (> numb 0)
      (if (= numb 1)
        (apply str numb " book. " autor ".")
        (apply str numb " books. " autors "."))
      (str "No books."))))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [aut] (= name (:name aut))) authors)))

(defn living-authors [authors]
  (filter (fn [aut] (alive? aut)) authors))

(defn has-a-living-author? [book]
  (let [check-aut (fn [a] (alive? a))]
     (first (map check-aut (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
