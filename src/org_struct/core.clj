(ns org-struct.core)

#_(def problem
      {:goals [[:maximize f1] [:minimize f2] [:maximize f3]]
       :constraints [(constraint-= (function (sum xi)) 1)]})
#_((solve problem) ==> {:status :solved
                        :result {:x 1 :y 2 :z 3}})
