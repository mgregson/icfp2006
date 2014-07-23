(ns io.gregson.icfp.monroeville.um
  (:import [jline Terminal]
           [java.nio ByteBuffer]
           [org.apache.commons.io IOUtils])
  (:require [gloss.core :as g]
            [gloss.io :as gio]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [lanterna.terminal :as t]))

(def
  ^{:private true
    :doc "Gloss frame for loading programs from files."}
  data-frame
  (g/compile-frame :uint32-be))

(def
  ^{:private true
    :doc "A Lanterna terminal to act as the UM terminal."}
  term
  (t/get-terminal :swing))

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
  [reg]
  (bit-and 0xFFFFFFFF (nth @registers reg)))

(defn- set-register
  "Sets the value in an UM register."
  [reg v]
  (dosync
   (ref-set registers (assoc @registers reg (bit-and 0xFFFFFFFF v)))))

(defn- read-array
  "Read the value in an array at an offset."
  [array offset]
  (let [data (nth @arrays array)
        size (count data)]
    (if (< size offset)
      (raise-error (format "ARRAY OFFSET: %d (%d)"
                           offset
                           size))
      (bit-and 0xFFFFFFFF (nth data offset)))))

(defn- set-array
  "Set the value in an array at an offset."
  [array offset v]
  (dosync
   (if (not (= (type v) java.lang.Long))
     (raise-error "INVALID-PARAM"))
   (if (< (count @arrays) array)
     (raise-error "INVALID ARRAY"))
   (let [start-array (nth @arrays array)
         size (count start-array)]
     (if (< size offset)
       (raise-error "ARRAY OFFSET")
       (ref-set arrays
                (assoc @arrays array (assoc start-array offset v)))))))

(defn- get-free-array-id
  "Find an available array identifier."
  []
  (dosync
   (if (not (empty? @array-pool))
     (let [id (first @array-pool)]
       (ref-set array-pool (rest @array-pool))
       id))
   (if (> 4294967295 (count @arrays))
     (count @arrays)
     (raise-error "ARRAY SPACE EXHAUSTED"))))

(defn- allocate-array
  "Create a new array in the UM, return the array identifier."
  [size]
  (let [array-id (get-free-array-id)]
    (dosync
     (ref-set arrays
              (assoc @arrays array-id (into [] (repeat size 0))))
     array-id)))

(defn- free-array
  "Free an array in the UM."
  [array]
  (dosync
   (if (or (= 0 array)
           (< (count @arrays) array)
           (= 0 (nth @arrays array)))
     (raise-error "INVALID ARRAY"))
   (ref-set arrays (assoc @arrays array 0))
   (ref-set array-pool (cons array @array-pool))))

(defn- set-display
  "Set the display output."
  [data]
  (if (< 255 data)
    (throw (Exception. "OUTPUT")))
  (dosync
   (ref-set display (bit-and 0x000000FF data))
   (t/put-character term (char @display))))

(defn- read-input
  "Read input in from the console."
  []
  (try
    (t/get-key-blocking term)
    (catch Exception e
      0xFFFFFFFF)))

(def
  ^{:private true
    :doc "The UM instructions."}
  instructions
  (vector
   ;; Conditional Move
   (fn [a b c]
     (if (not (= 0 (read-register c)))
       (do
         (log/debug (format "MOVE %d ({%d}) -> {%d}"
                            (read-register b)
                            b
                            a))
         (set-register a (read-register b)))
       (log/debug "NOOP")))

   ;; Array Index
   (fn [a b c]
     (log/debug (format "LOAD %d[%d] -> {%d}"
                        (read-register b)
                        (read-register c)
                        a))
     (set-register a (read-array (read-register b)
                                 (read-register c))))

   ;; Array Amendment
   (fn [a b c]
     (log/debug (format "PUT %d -> %d[%d]"
                        (read-register c)
                        (read-register a)
                        (read-register b)))
     (set-array (read-register a)
                (read-register b)
                (read-register c)))

   ;; Addition
   (fn [a b c]
     (log/debug (format "ADD %d + %d -> {%d}"
                        (read-register b)
                        (read-register c)
                        a))
     (set-register a (+ (read-register b)
                        (read-register c))))

   ;; Multiplication
   (fn [a b c]
     (log/debug (format "MUL %d * %d -> {%d}"
                        (read-register b)
                        (read-register c)
                        a))
     (set-register a (* (read-register b)
                        (read-register c))))

   ;; Division
   (fn [a b c]
     (let [c-val (read-register c)]
       (if (= 0 c-val)
         (throw (Exception. "INVALID DIVISION")))
       (log/debug (format "DIV %d / %d -> {%d}"
                          (read-register b)
                          c-val
                          a))
       (set-register a (int (/ (read-register b)
                               c-val)))))

   ;; Not-And
   (fn [a b c]
     (log/debug (format "NAND %d nand %d -> {%d}"
                        (read-register b)
                        (read-register c)
                        a))
     (set-register a (bit-or (bit-not (read-register b))
                             (bit-not (read-register c)))))

   ;; Halt
   (fn [a b c]
     (log/info "HALT")
     (throw (Exception. "HALT")))

   ;; Allocation
   (fn [a b c]
     (log/debug (format "ALLOC %d -> {%d}"
                        (read-register c)
                        b))
     (set-register b (allocate-array (read-register c))))

   ;; Abandonment
   (fn [a b c]
     (log/debug (format "FREE %d[]"
                        c))
     (free-array (read-register c)))

   ;; Output
   (fn [a b c]
     (log/debug (format "PRINT %d"
                        (read-register c)))
     (set-display (read-register c)))

   ;; Input
   (fn [a b c]
     (log/debug (format "READ {%d}" c))
     (set-register c (read-input)))

   ;; Load Program
   (fn [a b c]
     (let [b-val (read-register b)
           c-val (read-register c)]
       (if (or (< (count @arrays) b-val)
               (= 0 (nth @arrays b-val)))
         (raise-error  "INVALID ARRAY")
         (dosync
          (log/debug (format "PROGN %d[] %d"
                             b-val
                             c-val))
          (ref-set arrays
                   (assoc @arrays 0 (nth @arrays b-val)))
          (ref-set finger c-val)))))

   ;; Orthography
   (fn [a v]
     (log/debug (format "ORTHO {%d} %d" a v))
     (set-register a v))))

(defn decode-instruction
  "Convert an instruction number into a function."
  [num]
  (if (> num (count instructions))
    (raise-error  "INVALID INSTRUCTION")
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
              

(defn zero-registers
  "Zero the UM registers."
  []
  (dosync
   (ref-set registers (vector 0 0 0 0 0 0 0 0))))

(defn- read-finger
  "Read the point at the finger."
  []
  (read-array 0 @finger))

(defn set-finger
  "Set the UM finger point to a specified location."
  [loc]
  (dosync
   (ref-set finger loc)))

(defn- raise-error
  "Raise an error - an exception is thrown with a message that includes state."
  [message]
  (dosync
   (let [address @finger
         data (read-finger)
         instr-num (unsigned-bit-shift-right data 28)
         instr (decode-instruction instr-num)
         decoder (decoder-for instr-num)
         decoded-params (decoder data)
         details (format "Address: %08d   Data: %08x   Instruction: %02d %s"
                         address data instr-num decoded-params)]
     (log/error details)
     (log/error @registers)
     (throw (Exception. message)))))

(defn load-scroll
  "Load a scroll, zero all registers, and set the finger to 0."
  [scroll-name]
  (with-open [i (io/input-stream scroll-name)]
    (let [bytes (gio/decode-all data-frame (IOUtils/toByteArray i))]
      (dosync
       (ref-set arrays (assoc @arrays 0 bytes)))))
  (zero-registers)
  (set-finger 0)
  (log/info (format "Scroll loaded: %s" scroll-name)))

(defn execute-step
  "Execute the instruction at the UM finger and increment the finger."
  []
  (let [finger-val (read-finger)
        instr-num (unsigned-bit-shift-right finger-val 28)
        instr (decode-instruction instr-num)
        decoder (decoder-for instr-num)
        decoded-params (decoder finger-val)]
    (log/trace (format "Address: %08d    Data: %08x    Instruction: %02d %s"
                       @finger finger-val instr-num decoded-params))
    (log/trace @registers)
    (set-finger (+ 1 @finger))
    (apply instr decoded-params)))

(defn execute-scroll
  [scroll]
  (load-scroll scroll)
  (while true 
    (doseq [i (range 1000000)]
      (execute-step))
    (log/infof "Registers: %s  Finger: %d  Arrays: %d"
               @registers
               @finger
               (count @arrays))))

(defn -main
  []
  (log/info "BOOT")
  (try
    (t/start term)
    (execute-scroll "/home/mgregson/downloads/codex.umz")
    (catch Exception e
      (log/fatal e "Eplosion:"))
    (finally
      (t/stop term))))
