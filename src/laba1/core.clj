(ns laba1.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.math.numeric-tower :as math])
  (import java.lang.Math))
(def epsH 0.5)
(def epsL 0.15)
(def rA 3)
(def rB (* 1.5 rA))

(defn consider-potentials [points distance]
  (let [point-potential 
       (fn [x]
         (let [len (reduce + 0 (map #(Math/exp (- (* (/ 4 (math/expt rA 2)) (distance % x)))) points))]
         (list len x))
		)]
  (map point-potential points))
)


(defn partition-string [string]
  (into [] 
        (map read-string
             (drop-last (clojure.string/split string #","))
         )
   )
 )

 (def lines (atom []))
 
(defn read-file [file-path]
       (with-open [rdr (io/reader file-path)]
         (doseq [line (line-seq rdr)]
           (if (not= line "")
             (swap! lines conj (partition-string line))
           )
         )
       )
)

(defn hamming [p1 p2]
  ( count
    (filter true? 
            (map not= p1 p2)
    )
  )
)

(defn euclid [p1 p2]
  (->> (map - p1 p2)
       (map #(* % %))
       (reduce +) 
       (Math/sqrt)
  )
)

(defn clusterization [points distance]
    (let [potentials (consider-potentials points distance)]
		(loop [potentials potentials clusters ()]
			(let 	[	
						P1  (or (first (first clusters)) 0)          
						Pxk (apply max-key first potentials)       
						Pk  (first Pxk)
						Xk  (last Pxk)
						potentials-n (map #(cons (- (first %)
													(* Pk (Math/exp (- (* (/ 4 (math/expt rB 2)) (distance (last %) Xk)))))
												 ) 
												 (rest %)
											)
										  potentials
									  )
					]
				(cond
				 (> Pk (* epsH P1)) (recur potentials-n (conj clusters Pxk)) 
				 (< Pk (* epsL P1)) clusters                              
				 :else 
				   (let [Dmin (apply min (map #(distance Xk (last %)) clusters))] 
						(if (>= (+ (/ Dmin rA) (/ Pk P1)) 1)
							(recur potentials-n (conj clusters Pxk))                  
							(recur (map #(if (= Xk (last %)) (list 0 Xk) %) potentials) clusters)
						)
					)
				)
			)
		)
	)
)



(defn -main
  [& args]
  (if (>= (count args) 2) 
      (let [points (read-file (first args))
          distance (if (= (last args) "h") hamming euclid)]
        (println (clusterization @lines distance))
      )    
      (println "Not enough arguments specified")
   )
 )