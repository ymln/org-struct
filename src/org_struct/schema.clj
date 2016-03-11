(ns org-struct.schema
  (:require [schema.core :as s]))

(def Dir (s/enum :minimize :maximize))
(def Product [(s/one (s/eq '*) "*") (s/one s/Num "Num") (s/one s/Symbol "Symbol")])
(def Function [(s/one (s/eq '+) "+") Product])
(def Condition (s/enum '<= '= '>=))
(def Constraint [(s/one Condition "Condition")
                 (s/one Function "Function")
                 (s/one s/Num "Num")])

(def Type (s/enum :binary))
(def Solution s/Any) ; TODO
