(ns eximia.core-test
  (:require [eximia.core :as e]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]])
  (:import [javax.xml XMLConstants]
           [eximia.core CData]))

(defn coalesce-strs [string? ->string coll]
  (loop [acc [], coll coll]
    (if (seq coll)
      (let [[strs coll] (split-with string? coll)]
        (if (seq strs)
          (recur (conj acc (str/join (map ->string strs))) coll)
          (recur (conj acc (first coll)) (rest coll))))
      acc)))

(defn canonicalize [string? ->string  tree]
  (postwalk (fn [tree]
              (cond
                (and (map? tree) (contains? tree :tag))
                (-> tree
                    e/map->Element
                    (update :content (comp #(filterv (fn [child] (or (not (string? child)) (seq child)))
                                                     %)
                                                #(coalesce-strs string? ->string %))))

                (and (map? tree) (contains? tree :target))
                (-> tree
                    (update :target #(if (= % "") nil %))
                    (update :data #(if (= % "") nil %)))

                :else tree))
            tree))

(defn characters? [v] (or (string? v) (instance? CData v)))

(defn characters->string [v] (if (string? v) v (.chars ^CData v)))

;;;; # Generators

(def name-gen
  (gen/let [c gen/char-alpha
            cs gen/string-alphanumeric]
           (str c cs)))

(def content-char-gen
  (gen/such-that (fn [^Character c]
                   (let [c (int c)]
                     (and (>= c 10) (< (.indexOf "<&" c) 0))))
                 gen/char-ascii))

(def characters-gen (gen/fmap str/join (gen/vector content-char-gen)))

(def cdata-gen (gen/fmap e/->CData (gen/such-that #(not (.contains ^String % "]]>")) characters-gen)))

(def comment-gen (gen/fmap e/->Comment gen/string-alphanumeric))

(def pi-gen
  (gen/let [target name-gen
            data (gen/one-of [(gen/return nil) gen/string-alphanumeric])]
    (e/->ProcessingInstruction target data)))

(def qname-gen
  (gen/let [local-name name-gen
            namespace-uri (gen/one-of [(gen/return XMLConstants/NULL_NS_URI) name-gen])
            prefix (gen/one-of [(gen/return XMLConstants/DEFAULT_NS_PREFIX) name-gen])]
    (e/qname namespace-uri local-name prefix)))

(def attrs-gen (gen/map qname-gen characters-gen))

(defn element-of-gen [element-gen]
  (gen/let [tag qname-gen
            attrs attrs-gen
            content (gen/vector element-gen)]
    (gen/one-of [(gen/return (e/->Element tag attrs content))
                 (gen/return {:tag tag, :attrs attrs, :content content})])))

(def element-gen-all
  (element-of-gen (gen/recursive-gen element-of-gen
                                     (gen/one-of [characters-gen cdata-gen pi-gen comment-gen]))))

(def element-gen-cdata
  (element-of-gen (gen/recursive-gen element-of-gen
                                     (gen/one-of [characters-gen cdata-gen]))))

;;;; # Tests

(deftest test-qnames
  (let [qname (e/qname "foo")]
    (is (= "foo" (e/local-name qname)))
    (is (= XMLConstants/NULL_NS_URI (e/ns-uri qname)))
    (is (= XMLConstants/DEFAULT_NS_PREFIX (e/prefix qname)))
    (is (= :foo (e/qname->keyword qname)))
    (is (= :foo (e/qname->unq-keyword qname))))

  (let [qname (e/qname "http://example.com" "foo")]
    (is (= "foo" (e/local-name qname)))
    (is (= "http://example.com" (e/ns-uri qname)))
    (is (= XMLConstants/DEFAULT_NS_PREFIX (e/prefix qname)))
    (is (= :foo (e/qname->keyword qname)))
    (is (= :foo (e/qname->unq-keyword qname))))

  (let [qname (e/qname "http://example.com" "foo" "example")]
    (is (= "foo" (e/local-name qname)))
    (is (= "http://example.com" (e/ns-uri qname)))
    (is (= "example" (e/prefix qname)))
    (is (= :example/foo (e/qname->keyword qname)))
    (is (= :foo (e/qname->unq-keyword qname)))))

(defspec write-read
  50
  (for-all [el element-gen-cdata]
    (let [xml (e/write-str el {:xml-version "1.1"})         ; Enable some extra encoding
          el* (e/read-str xml)]
      (= (canonicalize characters? characters->string el)
         (canonicalize characters? characters->string el*)))))

(def noncoalescing-input-factory (e/input-factory {}))

(defspec write-read-all
  50
  (for-all [el element-gen-all]
    (let [xml (e/write-str el {:xml-version "1.1"})         ; Enable some extra encoding
          el* (e/read-str xml
                          {:wrap-cdata true
                           :preserve #{:processing-instruction :comment}
                           :xml-input-factory noncoalescing-input-factory})]
      (= (canonicalize string? identity el)
         (canonicalize string? identity el*)))))

(defspec write-read-cdata
  50
  (for-all [el element-gen-cdata]
    (let [xml (e/write-str el {:xml-version "1.1"})         ; Enable some extra encoding
          el* (e/read-str xml
                          {:wrap-cdata true
                           :xml-input-factory noncoalescing-input-factory})]
      (= (canonicalize string? identity el)
         (canonicalize string? identity el*)))))
