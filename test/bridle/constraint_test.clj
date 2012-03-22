(ns bridle.constraint-test
  (:use
   bridle.constraint
   clojure.test
   [slingshot.slingshot :only [try+]]
   [slingshot.support :only [ex-info-ns]]))

;; this is in slingshot 0.10.3, but not yet released
(def ex-class (ns-resolve ex-info-ns 'ExceptionInfo))

(defmethod assert-expr 'thrown+? [msg form]
  ;; (is (thrown+? selector expr))
  ;; Asserts that evaluating expr throws an object that matches
  ;; selector. Returns the thrown object.
  (let [selector (nth form 1)
        body (nthnext form 2)]
    `(try+
      ~@body
      (do-report {:type :fail :message ~msg :expected '~form
                  :actual "thrown+?: nothing was thrown"})
      (catch ~selector e#
        (do-report {:type :pass :message ~msg :expected '~form :actual e#})
        e#)
      (catch Object e#
        (do-report {:type :fail :message ~msg :expected '~form
                    :actual (format "thrown+?: %s did not match %s"
                                    (pr-str e#) '~selector)})
        e#))))

(deftest map-check-test
  (testing "allow map keys"
    (let [p (map-check)]
      (is (fn? p))
      (is (p {:a 1}) "succeed for partial keys")
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (p {:a 1 :b 2 :c 3}) "succeed for extra keys")
      (is (thrown+? map? (p [:a 1 :b 2])) "fail on arbitrary data")
      (is (thrown+? map? (p 1)) "fail on arbitrary data")))
  (testing "allow limited map keys"
    (let [p (map-check :allow [:a :b])]
      (is (fn? p))
      ;; (is (p {:a 1}) "succeed for partial keys") ;; fails pending MATCH-39
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (thrown+? map? (p {:a 1 :b 2 :c 3})) "fail for extra keys")))
  (testing "has keys"
    (let [p (map-check :has [:a :b])]
      (is (fn? p))
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (p {:a 1 :b 2}) "fail for partial keys")
      (is (p {:a 1 :b 2 :c 3}) "succeed for extra keys")
      (is (thrown+? map? (p [:a 1 :b 2])) "fail on arbitrary data")))
  (testing "mandatory and limited map keys"
    (let [p (map-check :has [:a :b] :allow [])]
      (is (fn? p))
      (is (thrown+? map? (p {:a 1})) "fail for partial keys")
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (thrown+? map? (p {:a 1 :b 2 :c 3})) "fail for extra keys")))
  (testing "nested keys"
    (let [p1 (map-check :has [:a :b] :allow [])
          p2 (map-check :types {:c p1})]
      (is (fn? p1))
      (is (fn? p2))
      (is (p2 {}) "succeed for partial keys")
      (is (p2 {:c {:a 1 :b 2}}) "succeed for partial keys")
      (is (p2 {:c {:a 1 :b 2} :d 1}) "succeed for all keys")
      (is (thrown+? map? (p2 {:c {:a 1 :b 2 :c 3} :d 1}))
          "fail for extra nested keys"))))

(deftest seq-check-test
  (testing "seqs"
    (let [v0 (seq-check)]
      (is (v0 []) "Matches empty vector")
      (is (v0 '(1)) "Matches a list")
      (is (v0 (filter identity [1 2])) "Matches a seq")
      (is (thrown+? map? (v0 {})) "fail for map"))
    (let [v1 (seq-check :count 1)]
      (is (v1 [1]) "Matches single element vector")
      (is (v1 '(1)) "Matches single list")
      (is (thrown+? map? (v1 [])) "fail for no elements")
      (is (thrown+? map? (v1 [1 2])) "fail for too many elements")
      (is (thrown+? map? (v1 {})) "fail for map"))
    (let [v2 (seq-check :min-count 1)]
      (is (v2 [1]) "Match single element vector")
      (is (v2 [1 2]) "Match two element vector")
      (is (thrown+? map? (v2 [])) "fail for no elements")
      (is (thrown+? map? (v2 {})) "fail for map"))))
