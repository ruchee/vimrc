(ns vim-clojure-static.generate-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [clojure.set :as set]
            [vim-clojure-static.generate :as sut]))

(deftest character-properties-test
  (is (= #{"LC" "Lo" "So" "Lm" "Sc" "Lt" "Co" "Me" "Zp" "L" "Sm" "Ps" "Cf" "M"
           "LD" "S" "Ll" "Z" "Pi" "Cc" "C" "Pe" "Sk" "Pf" "Nd" "Nl" "P" "Cn"
           "Zs" "Zl" "Mn" "Mc" "Lu" "N" "Cs" "Pc" "Pd" "Po" "No"}
         (:category sut/character-properties)))

  (testing "Script names"
    (is (set/subset?
          #{"COPT" "COPTIC"}
          (:script sut/character-properties))))

  (testing "Block aliases"
    (is (set/subset?
          #{"LATIN-1 SUPPLEMENT" "LATIN_1_SUPPLEMENT" "LATIN-1SUPPLEMENT"}
          (:block sut/character-properties)))))
