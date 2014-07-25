(ns io.gregson.icfp.monroeville.um
  (:gen-class)
  (:import [jline Terminal]
           [java.nio ByteBuffer]
           [org.apache.commons.io IOUtils])
  (:require [gloss.core :as g]
            [gloss.io :as gio]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]))

(defn read-char
  "Reads the next character from stream that is the current value of *in* ."
  {:added "1.0"
   :static true}
  []
  (if (instance? clojure.lang.LineNumberingPushbackReader *in*)
    (.read ^clojure.lang.LineNumberingPushbackReader *in*)
    (.read ^java.io.BufferedReader *in*)))

(def
  ^{:private true
    :doc "Gloss frame for loading programs from files."}
  data-frame
  (g/compile-frame :uint32-be))

(defrecord machine [finger registers arrays])
(defn mk-machine
  "Make a new machine!"
  [prog]
  (->machine 0 (make-array Long/TYPE 8) {0 (aclone ^longs prog)}))

(def
  ^{:private true
    :doc "Stores the system UM registers."}
  registers
  (ref (vector 0 0 0 0 0 0 0 0)))

(def
  ^{:private true
    :doc "Stores the UM arrags."}
  arrays
  (ref (vector (vector))))

(def
  ^{:private true
    :doc "Stores the set of available array indices."}
  array-pool
  (ref '()))

(def
  ^{:private true
    :doc "The UM output."}
  display
  (ref 0))

(def
  ^{:private true
    :doc "The UM finger pointer."}
  finger
  (ref 0))

(defn- raise-error [])

(defn- read-register
  "Read the value in an UM register."
  [m reg]
  (bit-and 0xFFFFFFFF (aget ^longs (:registers m) reg)))

(defn- set-register
  "Sets the value in an UM register."
  [m reg v]
  (aset-long (:registers m) reg (bit-and 0xFFFFFFFF v))
  m)

(defn- read-array
  "Read the value in an array at an offset."
  [m array offset]
  (let [data (get (:arrays m) array)
        size (count data)]
    (if (< size offset)
      (raise-error m (format "ARRAY OFFSET: %d (%d)"
                             offset
                             size))
      (bit-and 0xFFFFFFFF (aget ^longs data offset)))))

(defn- set-array
  "Set the value in an array at an offset."
  [m array offset v]
  (if (not (= (type v) java.lang.Long))
    (raise-error m "INVALID-PARAM"))
  (if (not (contains? (:arrays m) array))
    (raise-error m "INVALID ARRAY"))
  (let [data (get (:arrays m) array)
        size (count data)]
    (if (< size offset)
      (raise-error m "ARRAY OFFSET")
      (aset-long data offset v))
    m))

(defn- get-free-array-id
  "Find an available array identifier."
  [m]
  (loop [idx (rand-int Integer/MAX_VALUE)]
    (if (contains? (:arrays m) idx)
      (recur (rand-int Integer/MAX_VALUE))
      idx)))

(defn- allocate-array
  "Create a new array in the UM, return the array identifier."
  [m size]
  (let [array-id (get-free-array-id m)
        m2 (assoc m :arrays (assoc (:arrays m)
                              array-id
                              (make-array Long/TYPE size)))]
    (list array-id m2)))

(defn- free-array
  "Free an array in the UM."
  [m array]
  (if (or (= 0 array)
          (not (contains? (:arrays m) array)))
    (raise-error m "INVALID ARRAY"))
  (assoc m :arrays (dissoc (:arrays m) array)))

(defn- set-display
  "Set the display output."
  [data]
  (if (< 255 data)
    (throw (Exception. "OUTPUT")))
  (print (char data))
  (flush))

(defn- read-input
  "Read input in from the console."
  []
  (try
    (long (read-char))
    (catch Exception e
      0xFFFFFFFF)))

(def
  ^{:private true
    :doc "The UM instructions."}
  instructions
  (vector
   ;; Conditional Move
   (fn [m a b c]
     (if (not (= 0 (read-register m c)))
       (set-register m a (read-register m b)))
     m)

   ;; Array Index
   (fn [m a b c]
     (set-register m
                   a
                   (read-array m
                               (read-register m b)
                               (read-register m c)))
     m)

   ;; Array Amendment
   (fn [m a b c]
     (set-array m
                (read-register m a)
                (read-register m b)
                (read-register m c))
     m)

   ;; Addition
   (fn [m a b c]
     (set-register m
                   a
                   (+ (read-register m b)
                      (read-register m c)))
     m)

   ;; Multiplication
   (fn [m a b c]
     (set-register m
                   a
                   (* (read-register m b)
                      (read-register m c)))
     m)

   ;; Division
   (fn [m a b c]
     (let [c-val (read-register m c)]
       (if (= 0 c-val)
         (throw (Exception. "INVALID DIVISION")))
       (set-register m
                     a
                     (long (/ (read-register m b)
                              c-val)))
       m))

   ;; Not-And
   (fn [m a b c]
     (set-register m
                   a
                   (bit-or (bit-not (read-register m b))
                           (bit-not (read-register m c))))
     m)

   ;; Halt
   (fn [m a b c]
     (log/info "HALT")
     (throw (Exception. "HALT")))

   ;; Allocation
   (fn [m a b c]
     (let [[array-id m2] (allocate-array m
                                         (read-register m c))]
       (set-register m2
                     b
                     array-id)))

   ;; Abandonment
   (fn [m a b c]
     (free-array m (read-register m c)))

   ;; Output
   (fn [m a b c]
     (set-display (read-register m c))
     m)

   ;; Input
   (fn [m a b c]
     (set-register m c (read-input))
     m)

   ;; Load Program
   (fn [m a b c]
     (let [b-val (read-register m b)
           c-val (read-register m c)]
       (if (not (contains? (:arrays m) b-val))
         (raise-error m  "INVALID ARRAY")
         (let [m2 (if (< 0 b-val)
                    (assoc m
                      :arrays
                      (assoc (:arrays m)
                        0
                        (aclone ^longs (get (:arrays m)
                                            b-val))))
                    m)]
           (assoc m2 :finger c-val)))))

   ;; Orthography
   (fn [m a v]
     (set-register m a v)
     m)))

(defn decode-instruction
  "Convert an instruction number into a function."
  [m num]
  (if (> num (count instructions))
    (raise-error m "INVALID INSTRUCTION")
    (nth instructions num)))

(defn decoder-for
  "Produce a function to decode instruction parameters from an instruction."
  [instr]
  (cond
   ;; Regular Instructions
   (< instr 13) (fn [data]
                  (list
                   (bit-and 0x00000007 (unsigned-bit-shift-right data 6))
                   (bit-and 0x00000007 (unsigned-bit-shift-right data 3))
                   (bit-and 0x00000007 data)))

   ;; Special Instructions
   (= instr 13) (fn [data]
                  (list
                   (bit-and 0x00000007 (unsigned-bit-shift-right data 25))
                   (bit-and (bit-not 0xFE000000) data)))))

(defn- read-finger
  "Read the point at the finger."
  [m]
  (read-array m 0 (:finger m)))

(defn set-finger
  "Set the UM finger point to a specified location."
  [m loc]
  (assoc m :finger loc))

(defn- raise-error
  "Raise an error - an exception is thrown with a message that includes state."
  [m message]
  (let [address (:finger m)
        data (read-finger m)
        instr-num (unsigned-bit-shift-right data 28)
        instr (decode-instruction m instr-num)
        decoder (decoder-for instr-num)
        decoded-params (decoder data)
        details (format "Address: %08d   Data: %08x   Instruction: %02d %s"
                        address data instr-num decoded-params)]
    (log/error details)
    (log/error (:registers m))
    (throw (Exception. ^String message))))

(defn load-scroll
  "Load a scroll, zero all registers, and set the finger to 0."
  [scroll-name]
  (with-open [i (io/input-stream scroll-name)]
    (let [bytes (gio/decode-all data-frame (IOUtils/toByteArray i))
          prog-array (make-array Long/TYPE (count bytes))]
      (doseq [itr (map (fn [a b] {:idx a :data b}) (iterate inc 0) bytes)]
        (aset-long prog-array (:idx itr) (:data itr)))
      (log/info (format "Scroll loaded: %s" scroll-name))
      (mk-machine prog-array))))

(defn execute-step
  "Execute the instruction at the UM finger and increment the finger."
  [m]
  (let [finger-val (read-finger m)
        instr-num (unsigned-bit-shift-right finger-val 28)
        instr (decode-instruction m instr-num)
        decoder (decoder-for instr-num)
        decoded-params (decoder finger-val)]
    (apply instr (cons (set-finger m (+ 1 (:finger m)))
                       decoded-params))))

(defn execute-scroll
  [scroll]
  (loop [m (load-scroll scroll)
         c 0]
    (if (= (mod c 1000000) 0)
      (log/infof "Registers: %s  Finger: %d  Arrays: %d"
                 (apply format (cons "%08x %08x %08x %08x %08x %08x %08x %08x"
                                     (seq (:registers m))))
                 (:finger m)
                 (count (:arrays m))))
    (recur (execute-step m) (+ 1 c))))

(defn -main
  [& args]
  (log/info "BOOT")
  (try
    (execute-scroll (or (first args)
                        "/home/mgregson/downloads/codex.umz"))
    (catch Exception e
      (log/fatal e "Eplosion:"))))
