(ns org-struct.core)

#_(function (* 3 x))
#_(def problem
    {:objectives [[:maximize f1] [:minimize f2] [:maximize f3]]
     :constraints [(ex (= (function (sum xi)) 1))]})
#_((solve problem) ==> {:status :solved
                        :result {:x 1 :y 2 :z 3}})
#_(println (ex (+ 1 2)))
