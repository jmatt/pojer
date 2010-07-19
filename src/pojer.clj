;;;
;;;
;;;
;;;
;;;
;;;

(ns pojer
  (:gen-class))



;; Definitions
(def suit {1 :Club, 2 :Diamond, 3 :Heart, 4 :Spade})

(def vsuit (zipmap (vals suit) (keys suit)))

(def rank {2 :2 3 :3 4 :4 5 :5 6 :6 7 :7 8 :8 9 :9 10 :10 11 :Jack  12 :Queen 13 :King 14 :Ace})

(def vrank (zipmap (vals rank) (keys rank)))

(defstruct card :suit :rank)

(defn card+ [s r] (struct-map card :suit s :rank r))

(def sorted-deck (map (fn [ss] (map (fn [s r] (card+ s r))(repeat ss) rank)) suit))

(def deck (flatten sorted-deck))



;; Utilities 2
(defn shuffle
  "Return a random permutation of coll"
  {:added "1.2"}
  [coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al)
    (clojure.lang.RT/vector (.toArray al))))

(defn rand-cards [n]
  (if (and (> n 0) (< n 53))
     (take n (shuffle deck))))

(defmacro random-run
  "Prints results of n random-runs using handsize hs.
  If pred is true prints results otherwise continues."
  {:added "0.2"}
  [n hs pred]
  `(dotimes [i# ~n]
	(let [hand# (rand-cards ~hs)]
	  (if (~pred hand#)
	    (println i# (name '~pred) hand#)))))

(defmacro random-combination-run
  "Prints results of n random-runs using handsize hs
  different hands from combo many cards.
  If pred is true prints results otherwise continues."
  {:added "0.2"}
  [n hs pred combo]
  `(dotimes [i# ~n]
     (loop [combohand# (rand-cards ~combo)
	    hand# (combinations combohand# ~hs)
	    fhand# (first hand#)
	    result# (~pred fhand#)]
       (when (seq? hand#)
	 (if result#
	   (println i# (name '~pred) result#
		      "\ncombination hand: " combohand#
		      "\nhs hand:" fhand#)
	   (recur combohand# (next hand#) fhand# result#))))))

(defmacro random-run-eval
  "Results of n random-runs using handsize hs.
  If pred is true prints results otherwise continues."
  {:added "0.2"}
  [n hs eval]
  `(for [i# (range 0 ~n)]
	(let [hand# (rand-cards ~hs) result# (~eval hand#)] (list i# (name '~eval) result#))))

(defmacro reduce-attribute [c attribute]
  `(let [c# ~c]
     (if (map? c#)
       (if (vector? (~attribute c#))
	 (last (~attribute c#))
	 (~attribute c#)))))

(defmacro reduce-rank [c]
  ""
  `(reduce-attribute ~c :rank))

(defmacro reduce-suit [c]
  ""
  `(reduce-attribute ~c :suit))

(defn rr
  "reduce card to rank"
  [c]
  (reduce-rank c))

(defn rs
  "reduce card to suit"
  [c]
  (reduce-suit c))

(defn rv
  "reduce card to rank value"
  [c]
  (vrank (reduce-rank c)))

(defn numbers?
  "Applies number? to a sequence or series of numbers
  eg (numbers? 1 2 3 4) or (numbers? '(1 2 3 4))"
  ([ns]
     (if (seq? ns)
       (reduce = (map number? ns))
       (number? ns)))
  ([n1 n2]
     (and (number? n1)
	  (number? n2)))
  ([n1 n2 & more]
     (and (numbers? n1 n2)
	  (numbers? more))))

(defn compact-complex?
  "True if the sequence is in order defined by inc.
   False otherwise. Yeah I know this isn't a real definition
   to test a compact set or even test sequential compactness.
   Sorry :-/"
  ([] true)
  ([v]
     (if (seq? v)
       (reduce compact-complex? (compact-complex? (first v)) (rest v))
       v))
  ([v1 v2]
     (if (numbers? v1 v2)
       (let [w2 (inc v1)]
	 (if (= w2 v2)
	   v2
	   false))
       false))
; ([v1 v2 v3 v4 v5] ;sped up approximately 15% for exact match case.
;   (if (numbers? v1 v2 v3 v4 v5)
;     (let [w2 (inc v1) w3 (inc w2) w4 (inc w3) w5 (inc w4)]
;       (if
;         (and (= w2 v2) (= w3 v3) (= w4 v4) (= w5 v5))
;           v5
;           false))
;      false))
  ([v1 v2 & more]
     (reduce compact-complex? (compact-complex? v1 v2) more)))

(defn compact-range [vs] (range (first vs) (+ (first vs) (count vs))))

(defn compact-seq? [s] (apply = (map = (compact-range s) s)))

(defn compact?
  "True if the sequence is in order defined by inc.
   False otherwise. Yeah I know this isn't a real definition
   to test a compact set or even test sequential compactness.
   Sorry :-/"
  ([]
     true)
  ([vs]
     (if (seq? vs)
       (compact-seq? vs)
       vs))
  ([v1 v2] (compact? (list v1 v2)))
  ([v1 v2 & more] (compact? (cons v1 (cons v2 more)))))

(defn same? [s] (apply = s))

(defnk frequencies-sorted-n [s n :comparer >]
  "sort frequencies of sequence s by nth item of each element."
  (sort (fn [f1 f2] (comparer (last f1) (last f2))) (frequencies (map (fn [e] (nth e n)) s))))

(defn sr-frequencies [hand]
  "sorted frequency of card ranks in hand"
  (sort (fn [f1 f2] (> (last f1)
		      (last f2)))
	(frequencies (sort > (map rv hand)))))

(defnk meta-frequencies
  "Tests to find the largest same rank cards.
   does not return true if another valid frequency exists."
  [same hand :groupcount nil]
  (let [sf (sr-frequencies hand)
	f1 (first sf)
	size (second f1) n (count hand)
	groups (if (nil? groupcount) (- (inc n) same) groupcount)]
  (if (and (= groups (count sf)) (= same size)) sf false)))

(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
	 iter-comb
	 (fn iter-comb [c j]
	   (if (> j n) nil
	       (let [c (assoc c j (dec (c j)))]
		 (if (< (c j) j) [c (inc j)]
		     (loop [c c, j j]
		       (if (= j 1) [c j]
			   (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
	 step
	 (fn step [c j]
	   (cons (rseq (subvec c 1 (inc n)))
		 (lazy-seq (let [next-step (iter-comb c j)]
			     (when next-step (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

(defn combinations
  "All the unique ways of taking n different elements from items"
  [items n]
  (let [v-items (vec (reverse items))]
    (if (zero? n) (list ())
	(let [cnt (count items)]
	  (cond (> n cnt) nil
		(= n cnt) (list (seq items))
		:else
		(map #(map v-items %) (index-combinations n cnt)))))))



;; Hand Evaluation
(defn no-pair? [hand] (meta-frequencies 1 hand))

(defn pair? [hand] (meta-frequencies 2 hand))

(defn two-pair? [hand] (meta-frequencies 2 hand :groupcount (- (count hand) 2)))

(defn three-of-a-kind? [hand] (meta-frequencies 3 hand))

(defn full-house? [hand] (meta-frequencies 3 hand :groupcount (- (count hand) 3)))

(defn straight? [hand] (compact? (sort (map rv hand))))

(defn flush? [hand] (same? (map rs hand)))

(defn four-of-a-kind? [hand] (meta-frequencies 4 hand))

(defn straight-flush? [hand] (and (flush? hand) (straight? hand)))

(defn eval-hand? [hand]
  (let [straight (straight? hand)
	flush (flush? hand)]
    (cond
     (and straight flush) "Straight Flush."
     (four-of-a-kind? hand) "Four of a Kind."
     (full-house? hand) "Full House."
     flush "Flush."
     straight "Straight."
     (three-of-a-kind? hand) "Three of a Kind."
     (two-pair? hand) "Two Pair."
     (pair? hand) "Pair."
     (no-pair? hand) "No Pair."
     )))



;gross nasty learning... ugh complexity
(defn rankle-borked-complex ([] nil)
  ([c]
     (if (map? c)
       (list (reduce-rank c))
       (if (seq? c)
	 (let [c1 (first c) more (rest c)] (if (empty? more) (rankle-borked-complex c1) (flatten (cons (rankle-borked-complex c1) (rankle-borked-complex more)))))
	 (print "c " c))))
  ([c1 c2] (let [r1 (if (map? c1) (:rank c1) c1)
		 r2 (if (map? c2) (:rank c2) c2)]
	     (list r1 r2)))
  ([c1 c2 & more] (flatten (cons (rankle-borked-complex c1 c2) (rankle-borked-complex more)))))

(defn flush-complex?
  ([c]
     (if (map? c)
       (:suit c)
       (if (seq? c)
	 (reduce flush-complex? c)
	 c)))
  ([c1 c2] (let [s1 (if (map? c1) (:suit c1) c1)
		 s2 (if (map? c2) (:suit c2) c2)]
	     (if (= s1 s2)
	       s1
	       false)))
  ([c1 c2 & more] (reduce flush-complex? (flush-complex? c1 c2) more)))
