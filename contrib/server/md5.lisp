;;;; This file implements The MD5 Message-Digest Algorithm, as defined in
;;;; RFC 1321 by R. Rivest, published April 1992.
;;;;
;;;; It was written by Pierre R. Mai, with copious input from the
;;;; cmucl-help mailing-list hosted at cons.org, in November 2001 and
;;;; has been placed into the public domain.
;;;;
;;;; While the implementation should work on all conforming Common
;;;; Lisp implementations, it has only been optimized for CMU CL,
;;;; where it achieved comparable performance to the standard md5sum
;;;; utility (within a factor of 1.5 or less on iA32 and UltraSparc
;;;; hardware).
;;;;
;;;; Since the implementation makes heavy use of arithmetic on
;;;; (unsigned-byte 32) numbers, acceptable performance is likely only
;;;; on CL implementations that support unboxed arithmetic on such
;;;; numbers in some form.  For other CL implementations a 16bit
;;;; implementation of MD5 is probably more suitable.
;;;;
;;;; The code implements correct operation for files of unbounded size
;;;; as is, at the cost of having to do a single generic integer
;;;; addition for each call to update-md5-state.  If you call
;;;; update-md5-state frequently with little data, this can pose a
;;;; performance problem.  If you can live with a size restriction of
;;;; 512 MB, then you can enable fast fixnum arithmetic by putting
;;;; :md5-small-length onto *features* prior to compiling this file.
;;;;
;;;; Testing code can be compiled by including :md5-testing on
;;;; *features* prior to compilation.  In that case evaluating
;;;; (md5::test-rfc1321) will run all the test-cases present in
;;;; Appendix A.5 of RFC 1321 and report on the results.
;;;; Evaluating (md5::test-other) will run further test-cases
;;;; gathered by the author to cover regressions, etc.
;;;;
;;;; This software is "as is", and has no warranty of any kind.  The
;;;; authors assume no responsibility for the consequences of any use
;;;; of this software.

(defpackage #:md5 (:use #:cl)
  (:export
   ;; Low-Level types and functions
   #:md5-regs #:initial-md5-regs #:md5regs-digest
   #:update-md5-block #:fill-block #:fill-block-ub8 #:fill-block-char
   ;; Mid-Level types and functions
   #:md5-state #:md5-state-p #:make-md5-state
   #:update-md5-state #:finalize-md5-state
   ;; High-Level functions on sequences, streams and files
   #:md5sum-sequence #:md5sum-stream #:md5sum-file
   ;; Very High level functions
   #:md5))

(in-package #:md5)

#+cmu
(eval-when (:compile-toplevel)
  (defparameter *old-expansion-limit* ext:*inline-expansion-limit*)
  (setq ext:*inline-expansion-limit* (max ext:*inline-expansion-limit* 1000)))

#+cmu
(eval-when (:compile-toplevel :execute)
  (defparameter *old-features* *features*)
  (pushnew (c:backend-byte-order c:*target-backend*) *features*))

;;; Section 2:  Basic Datatypes

#-lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype ub32 ()
    "Corresponds to the 32bit quantity word of the MD5 Spec"
    `(unsigned-byte 32)))

#+lispworks
(deftype ub32 ()
    "Corresponds to the 32bit quantity word of the MD5 Spec"
    `(unsigned-byte 32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro assemble-ub32 (a b c d)
    "Assemble an ub32 value from the given (unsigned-byte 8) values,
where a is the intended low-order byte and d the high-order byte."
    `(the ub32 (logior (ash ,d 24) (ash ,c 16) (ash ,b 8) ,a))))

;;; Section 3.4:  Auxilliary functions

(declaim (inline f g h i)
         (ftype (function (ub32 ub32 ub32) ub32) f g h i))

(defun f (x y z)
  (declare (type ub32 x y z)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+cmu
  (kernel:32bit-logical-or (kernel:32bit-logical-and x y)
                           (kernel:32bit-logical-andc1 x z))
  #-cmu
  (logior (logand x y) (logandc1 x z)))

(defun g (x y z)
  (declare (type ub32 x y z)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+cmu
  (kernel:32bit-logical-or (kernel:32bit-logical-and x z)
                           (kernel:32bit-logical-andc2 y z))
  #-cmu
  (logior (logand x z) (logandc2 y z)))

(defun h (x y z)
  (declare (type ub32 x y z)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+cmu
  (kernel:32bit-logical-xor x (kernel:32bit-logical-xor y z))
  #-cmu
  (logxor x y z))

(defun i (x y z)
  (declare (type ub32 x y z)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+cmu
  (kernel:32bit-logical-xor y (kernel:32bit-logical-orc2 x z))
  #-cmu
  (ldb (byte 32 0) (logxor y (logorc2 x z))))

(declaim (inline mod32+)
         (ftype (function (ub32 ub32) ub32) mod32+))
(defun mod32+ (a b)
  (declare (type ub32 a b) (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (ldb (byte 32 0) (+ a b)))

#+cmu
(define-compiler-macro mod32+ (a b)
  `(ext:truly-the ub32 (+ ,a ,b)))

(declaim (inline rol32)
         (ftype (function (ub32 (unsigned-byte 5)) ub32) rol32))
(defun rol32 (a s)
  (declare (type ub32 a) (type (unsigned-byte 5) s)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+cmu
  (kernel:32bit-logical-or #+little-endian (kernel:shift-towards-end a s)
                           #+big-endian (kernel:shift-towards-start a s)
                           (ash a (- s 32)))
  #-cmu
  (logior (ldb (byte 32 0) (ash a s)) (ash a (- s 32))))

;;; Section 3.4:  Table T

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *t* (make-array 64 :element-type 'ub32
                                :initial-contents
                                (loop for i from 1 to 64
                                      collect
                                      (truncate
                                       (* 4294967296
                                          (abs (sin (float i 0.0d0)))))))))

;;; Section 3.4:  Helper Macro for single round definitions

(defmacro with-md5-round ((op block) &rest clauses)
  (loop for (a b c d k s i) in clauses
        collect
        `(setq ,a (mod32+ ,b (rol32 (mod32+ (mod32+ ,a (,op ,b ,c ,d))
                                            (mod32+ (aref ,block ,k)
                                                    ,(aref *t* (1- i))))
                                    ,s)))
        into result
        finally
        (return `(progn ,@result))))

;;; Section 3.3:  (Initial) MD5 Working Set

(deftype md5-regs ()
  "The working state of the MD5 algorithm, which contains the 4 32-bit
registers A, B, C and D."
  `(simple-array (unsigned-byte 32) (4)))

(defmacro md5-regs-a (regs)
  `(aref ,regs 0))

(defmacro md5-regs-b (regs)
  `(aref ,regs 1))

(defmacro md5-regs-c (regs)
  `(aref ,regs 2))

(defmacro md5-regs-d (regs)
  `(aref ,regs 3))

(defconstant +md5-magic-a+ (assemble-ub32 #x01 #x23 #x45 #x67)
  "Initial value of Register A of the MD5 working state.")
(defconstant +md5-magic-b+ (assemble-ub32 #x89 #xab #xcd #xef)
  "Initial value of Register B of the MD5 working state.")
(defconstant +md5-magic-c+ (assemble-ub32 #xfe #xdc #xba #x98)
  "Initial value of Register C of the MD5 working state.")
(defconstant +md5-magic-d+ (assemble-ub32 #x76 #x54 #x32 #x10)
  "Initial value of Register D of the MD5 working state.")

(declaim (inline initial-md5-regs))
(defun initial-md5-regs ()
  "Create the initial working state of an MD5 run."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((regs (make-array 4 :element-type '(unsigned-byte 32))))
    (declare (type md5-regs regs))
    (setf (md5-regs-a regs) +md5-magic-a+
          (md5-regs-b regs) +md5-magic-b+
          (md5-regs-c regs) +md5-magic-c+
          (md5-regs-d regs) +md5-magic-d+)
    regs))

;;; Section 3.4:  Operation on 16-Word Blocks

(defun update-md5-block (regs block)
  "This is the core part of the MD5 algorithm.  It takes a complete 16
word block of input, and updates the working state in A, B, C, and D
accordingly."
  (declare (type md5-regs regs)
           (type (simple-array ub32 (16)) block)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((A (md5-regs-a regs)) (B (md5-regs-b regs))
        (C (md5-regs-c regs)) (D (md5-regs-d regs)))
    (declare (type ub32 A B C D))
    ;; Round 1
    (with-md5-round (f block)
      (A B C D  0  7  1)(D A B C  1 12  2)(C D A B  2 17  3)(B C D A  3 22  4)
      (A B C D  4  7  5)(D A B C  5 12  6)(C D A B  6 17  7)(B C D A  7 22  8)
      (A B C D  8  7  9)(D A B C  9 12 10)(C D A B 10 17 11)(B C D A 11 22 12)
      (A B C D 12  7 13)(D A B C 13 12 14)(C D A B 14 17 15)(B C D A 15 22 16))
    ;; Round 2
    (with-md5-round (g block)
      (A B C D  1  5 17)(D A B C  6  9 18)(C D A B 11 14 19)(B C D A  0 20 20)
      (A B C D  5  5 21)(D A B C 10  9 22)(C D A B 15 14 23)(B C D A  4 20 24)
      (A B C D  9  5 25)(D A B C 14  9 26)(C D A B  3 14 27)(B C D A  8 20 28)
      (A B C D 13  5 29)(D A B C  2  9 30)(C D A B  7 14 31)(B C D A 12 20 32))
    ;; Round 3
    (with-md5-round (h block)
      (A B C D  5  4 33)(D A B C  8 11 34)(C D A B 11 16 35)(B C D A 14 23 36)
      (A B C D  1  4 37)(D A B C  4 11 38)(C D A B  7 16 39)(B C D A 10 23 40)
      (A B C D 13  4 41)(D A B C  0 11 42)(C D A B  3 16 43)(B C D A  6 23 44)
      (A B C D  9  4 45)(D A B C 12 11 46)(C D A B 15 16 47)(B C D A  2 23 48))
    ;; Round 4
    (with-md5-round (i block)
      (A B C D  0  6 49)(D A B C  7 10 50)(C D A B 14 15 51)(B C D A  5 21 52)
      (A B C D 12  6 53)(D A B C  3 10 54)(C D A B 10 15 55)(B C D A  1 21 56)
      (A B C D  8  6 57)(D A B C 15 10 58)(C D A B  6 15 59)(B C D A 13 21 60)
      (A B C D  4  6 61)(D A B C 11 10 62)(C D A B  2 15 63)(B C D A  9 21 64))
    ;; Update and return
    (setf (md5-regs-a regs) (mod32+ (md5-regs-a regs) A)
          (md5-regs-b regs) (mod32+ (md5-regs-b regs) B)
          (md5-regs-c regs) (mod32+ (md5-regs-c regs) C)
          (md5-regs-d regs) (mod32+ (md5-regs-d regs) D))
    regs))

;;; Section 3.4:  Converting 8bit-vectors into 16-Word Blocks

(declaim (inline fill-block fill-block-ub8 fill-block-char))

(defun fill-block-ub8 (block buffer offset)
  "Convert a complete 64 (unsigned-byte 8) input vector segment
starting from offset into the given 16 word MD5 block."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
           (type (simple-array ub32 (16)) block)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
;;  #+(and :cmu :little-endian)
;;  (kernel:bit-bash-copy     ;; There is a problem with this specific code (PBrochard)
;;   buffer (+ (* vm:vector-data-offset vm:word-bits) (* offset vm:byte-bits))
;;   block (* vm:vector-data-offset vm:word-bits)
;;   (* 64 vm:byte-bits))
;;  #-(and :cmu :little-endian)
  (loop for i of-type (integer 0 16) from 0
        for j of-type (integer 0 #.most-positive-fixnum)
        from offset to (+ offset 63) by 4
        do
        (setf (aref block i)
              (assemble-ub32 (aref buffer j)
                             (aref buffer (+ j 1))
                             (aref buffer (+ j 2))
                             (aref buffer (+ j 3))))))

(defun fill-block-char (block buffer offset)
  "Convert a complete 64 character input string segment starting from
offset into the given 16 word MD5 block."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
           (type (simple-array ub32 (16)) block)
           (type simple-string buffer)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
;;  #+(and :cmu :little-endian)
;;  (kernel:bit-bash-copy     ;; There is a problem with this specific code (PBrochard)
;;   buffer (+ (* vm:vector-data-offset vm:word-bits) (* offset vm:byte-bits))
;;   block (* vm:vector-data-offset vm:word-bits)
;;   (* 64 vm:byte-bits))
;;  #-(and :cmu :little-endian)
  (loop for i of-type (integer 0 16) from 0
        for j of-type (integer 0 #.most-positive-fixnum)
        from offset to (+ offset 63) by 4
        do
        (setf (aref block i)
              (assemble-ub32 (char-code (schar buffer j))
                             (char-code (schar buffer (+ j 1)))
                             (char-code (schar buffer (+ j 2)))
                             (char-code (schar buffer (+ j 3)))))))

(defun fill-block (block buffer offset)
  "Convert a complete 64 byte input vector segment into the given 16
word MD5 block.  This currently works on (unsigned-byte 8) and
character simple-arrays, via the functions `fill-block-ub8' and
`fill-block-char' respectively."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
           (type (simple-array ub32 (16)) block)
           (type (simple-array * (*)) buffer)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (etypecase buffer
    ((simple-array (unsigned-byte 8) (*))
     (fill-block-ub8 block buffer offset))
    (simple-string
     (fill-block-char block buffer offset))))

;;; Section 3.5:  Message Digest Output

(declaim (inline md5regs-digest))
(defun md5regs-digest (regs)
  "Create the final 16 byte message-digest from the MD5 working state
in regs.  Returns a (simple-array (unsigned-byte 8) (16))."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type md5-regs regs))
  (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (16)) result))
    (macrolet ((frob (reg offset)
                 (let ((var (gensym)))
                   `(let ((,var ,reg))
                      (declare (type ub32 ,var))
                      (setf
                       (aref result ,offset) (ldb (byte 8 0) ,var)
                       (aref result ,(+ offset 1)) (ldb (byte 8 8) ,var)
                       (aref result ,(+ offset 2)) (ldb (byte 8 16) ,var)
                       (aref result ,(+ offset 3)) (ldb (byte 8 24) ,var))))))
      (frob (md5-regs-a regs) 0)
      (frob (md5-regs-b regs) 4)
      (frob (md5-regs-c regs) 8)
      (frob (md5-regs-d regs) 12))
    result))

;;; Mid-Level Drivers

(defstruct (md5-state
             (:constructor make-md5-state ())
             (:copier))
  (regs (initial-md5-regs) :type md5-regs :read-only t)
  (amount 0 :type
          #-md5-small-length (integer 0 *)
          #+md5-small-length (unsigned-byte 29))
  (block (make-array 16 :element-type '(unsigned-byte 32)) :read-only t
         :type (simple-array (unsigned-byte 32) (16)))
  (buffer (make-array 64 :element-type '(unsigned-byte 8)) :read-only t
         :type (simple-array (unsigned-byte 8) (64)))
  (buffer-index 0 :type (integer 0 63))
  (finalized-p nil))

(declaim (inline copy-to-buffer))
(defun copy-to-buffer (from from-offset count buffer buffer-offset)
  "Copy a partial segment from input vector from starting at
from-offset and copying count elements into the 64 byte buffer
starting at buffer-offset."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type (unsigned-byte 29) from-offset)
           (type (integer 0 63) count buffer-offset)
           (type (simple-array * (*)) from)
           (type (simple-array (unsigned-byte 8) (64)) buffer))
;;  #+cmu
;;  (kernel:bit-bash-copy     ;; There is a problem with this specific code (PBrochard)
;;   from (+ (* vm:vector-data-offset vm:word-bits) (* from-offset vm:byte-bits))
;;   buffer (+ (* vm:vector-data-offset vm:word-bits)
;;             (* buffer-offset vm:byte-bits))
;;   (* count vm:byte-bits))
;;  #-cmu
  (etypecase from
    (simple-string
     (loop for buffer-index of-type (integer 0 64) from buffer-offset
           for from-index of-type fixnum from from-offset
           below (+ from-offset count)
           do
           (setf (aref buffer buffer-index)
                 (char-code (schar (the simple-string from) from-index)))))
    ((simple-array (unsigned-byte 8) (*))
     (loop for buffer-index of-type (integer 0 64) from buffer-offset
           for from-index of-type fixnum from from-offset
           below (+ from-offset count)
           do
           (setf (aref buffer buffer-index)
                 (aref (the (simple-array (unsigned-byte 8) (*)) from)
                       from-index))))))

(defun update-md5-state (state sequence &key (start 0) (end (length sequence)))
  "Update the given md5-state from sequence, which is either a
simple-string or a simple-array with element-type (unsigned-byte 8),
bounded by start and end, which must be numeric bounding-indices."
  (declare (type md5-state state)
           (type (simple-array * (*)) sequence)
           (type fixnum start end)
           (optimize (speed 3) #+cmu (safety 0) (space 0) (debug 0))
           #+cmu
           (ext:optimize-interface (safety 1) (debug 1)))
  (let ((regs (md5-state-regs state))
        (block (md5-state-block state))
        (buffer (md5-state-buffer state)))
    (declare (type md5-regs regs)
             (type (simple-array (unsigned-byte 32) (16)) block)
             (type (simple-array (unsigned-byte 8) (64)) buffer))
    ;; Handle old rest
    (unless (zerop (md5-state-buffer-index state))
      (let* ((buffer-index (md5-state-buffer-index state))
             (remainder (- 64 buffer-index))
             (length (- end start))
             (amount (min remainder length)))
        (declare (type (integer 0 63) buffer-index remainder amount)
                 (type fixnum length))
        (copy-to-buffer sequence start amount buffer buffer-index)
        (setf (md5-state-amount state)
              #-md5-small-length (+ (md5-state-amount state) amount)
              #+md5-small-length (the (unsigned-byte 29)
                                      (+ (md5-state-amount state) amount)))
        (setq start (the fixnum (+ start amount)))
        (if (< length remainder)
            (setf (md5-state-buffer-index state)
                  (the (integer 0 63) (+ buffer-index amount)))
          (progn
            (fill-block-ub8 block buffer 0)
            (update-md5-block regs block)
            (setf (md5-state-buffer-index state) 0)))))
    ;; Leave when nothing to do
    (when (>= start end)
      (return-from update-md5-state state))
    ;; Handle main-part and new-rest
    (etypecase sequence
      ((simple-array (unsigned-byte 8) (*))
       (locally
           (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
         (loop for offset of-type (unsigned-byte 29) from start below end by 64
               until (< (- end offset) 64)
               do
               (fill-block-ub8 block sequence offset)
               (update-md5-block regs block)
               finally
               (let ((amount (- end offset)))
                 (unless (zerop amount)
                   (copy-to-buffer sequence offset amount buffer 0))
                 (setf (md5-state-buffer-index state) amount)))))
      (simple-string
       (locally
           (declare (type simple-string sequence))
         (loop for offset of-type (unsigned-byte 29) from start below end by 64
               until (< (- end offset) 64)
               do
               (fill-block-char block sequence offset)
               (update-md5-block regs block)
               finally
               (let ((amount (- end offset)))
                 (unless (zerop amount)
                   (copy-to-buffer sequence offset amount buffer 0))
                 (setf (md5-state-buffer-index state) amount))))))
    (setf (md5-state-amount state)
          #-md5-small-length (+ (md5-state-amount state)
                                (the fixnum (- end start)))
          #+md5-small-length (the (unsigned-byte 29)
                               (+ (md5-state-amount state)
                                  (the fixnum (- end start)))))
    state))

(defun finalize-md5-state (state)
  "If the given md5-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the MD5 standard.

The resulting MD5 message-digest is returned as an array of sixteen
(unsigned-byte 8) values.  Calling `update-md5-state' after a call to
`finalize-md5-state' results in unspecified behaviour."
  (declare (type md5-state state)
           (optimize (speed 3) #+cmu (safety 0) (space 0) (debug 0))
           #+cmu
           (ext:optimize-interface (safety 1) (debug 1)))
  (or (md5-state-finalized-p state)
      (let ((regs (md5-state-regs state))
            (block (md5-state-block state))
            (buffer (md5-state-buffer state))
            (buffer-index (md5-state-buffer-index state))
            (total-length (* 8 (md5-state-amount state))))
        (declare (type md5-regs regs)
                 (type (integer 0 63) buffer-index)
                 (type (simple-array ub32 (16)) block)
                 (type (simple-array (unsigned-byte 8) (*)) buffer))
        ;; Add mandatory bit 1 padding
        (setf (aref buffer buffer-index) #x80)
        ;; Fill with 0 bit padding
        (loop for index of-type (integer 0 64)
              from (1+ buffer-index) below 64
              do (setf (aref buffer index) #x00))
        (fill-block-ub8 block buffer 0)
        ;; Flush block first if length wouldn't fit
        (when (>= buffer-index 56)
          (update-md5-block regs block)
          ;; Create new fully 0 padded block
          (loop for index of-type (integer 0 16) from 0 below 16
                do (setf (aref block index) #x00000000)))
        ;; Add 64bit message bit length
        (setf (aref block 14) (ldb (byte 32 0) total-length))
        #-md5-small-length
        (setf (aref block 15) (ldb (byte 32 32) total-length))
        ;; Flush last block
        (update-md5-block regs block)
        ;; Done, remember digest for later calls
        (setf (md5-state-finalized-p state)
              (md5regs-digest regs)))))

;;; High-Level Drivers

(defun md5sum-sequence (sequence &key (start 0) end)
  "Calculate the MD5 message-digest of data in sequence.  On CMU CL
this works for all sequences whose element-type is supported by the
underlying MD5 routines, on other implementations it only works for 1d
simple-arrays with such element types."
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type vector sequence) (type fixnum start))
  (let ((state (make-md5-state)))
    (declare (type md5-state state))
    #+cmu
    (lisp::with-array-data ((data sequence) (real-start start) (real-end end))
      (update-md5-state state data :start real-start :end real-end))
    #-cmu
    (let ((real-end (or end (length sequence))))
      (declare (type fixnum real-end))
      (update-md5-state state sequence :start start :end real-end))
    (finalize-md5-state state)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +buffer-size+ (* 128 1024)
    "Size of internal buffer to use for md5sum-stream and md5sum-file
operations.  This should be a multiple of 64, the MD5 block size."))

(deftype buffer-index () `(integer 0 ,+buffer-size+))

(defun md5sum-stream (stream)
  "Calculate an MD5 message-digest of the contents of stream.  Its
element-type has to be either (unsigned-byte 8) or character."
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (let ((state (make-md5-state)))
    (declare (type md5-state state))
    (cond
      ((equal (stream-element-type stream) '(unsigned-byte 8))
       (let ((buffer (make-array +buffer-size+
                                 :element-type '(unsigned-byte 8))))
         (declare (type (simple-array (unsigned-byte 8) (#.+buffer-size+))
                        buffer))
         (loop for bytes of-type buffer-index = (read-sequence buffer stream)
               do (update-md5-state state buffer :end bytes)
               until (< bytes +buffer-size+)
               finally
               (return (finalize-md5-state state)))))
      ((equal (stream-element-type stream) 'character)
       (let ((buffer (make-string +buffer-size+)))
         (declare (type (simple-string #.+buffer-size+) buffer))
         (loop for bytes of-type buffer-index = (read-sequence buffer stream)
               do (update-md5-state state buffer :end bytes)
               until (< bytes +buffer-size+)
               finally
               (return (finalize-md5-state state)))))
      (t
       (error "Unsupported stream element-type ~S for stream ~S."
              (stream-element-type stream) stream)))))

(defun md5sum-file (pathname)
  "Calculate the MD5 message-digest of the file specified by pathname."
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (md5sum-stream stream)))



(defun md5-string (md5-digest)
  (format nil "~(~{~2,'0X~}~)"
	  (map 'list #'identity md5-digest)))


(defun md5 (sequence)
  (md5-string (md5sum-sequence sequence)))



#+md5-testing
(defconstant +rfc1321-testsuite+
  '(("" . "d41d8cd98f00b204e9800998ecf8427e")
    ("a" ."0cc175b9c0f1b6a831c399e269772661")
    ("abc" . "900150983cd24fb0d6963f7d28e17f72")
    ("message digest" . "f96b697d7cb7938d525a2f31aaf161d0")
    ("abcdefghijklmnopqrstuvwxyz" . "c3fcd3d76192e4007dfb496cca67e13b")
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" .
     "d174ab98d277d9f5a5611c2c9f419d9f")
    ("12345678901234567890123456789012345678901234567890123456789012345678901234567890" .
     "57edf4a22be3c955ac49da2e2107b67a"))
  "AList of test input strings and stringified message-digests
according to the test suite in Appendix A.5 of RFC 1321")

#+md5-testing
(defconstant +other-testsuite+
  '(;; From padding bug report by Edi Weitz
    ("1631901HERR BUCHHEISTERCITROEN NORD1043360796beckenbauer" .
     "d734945e5930bb28859ccd13c830358b")
    ;; Test padding for strings from 0 to 69*8 bits in size.
    ("" . "d41d8cd98f00b204e9800998ecf8427e")
    ("a" . "0cc175b9c0f1b6a831c399e269772661")
    ("aa" . "4124bc0a9335c27f086f24ba207a4912")
    ("aaa" . "47bce5c74f589f4867dbd57e9ca9f808")
    ("aaaa" . "74b87337454200d4d33f80c4663dc5e5")
    ("aaaaa" . "594f803b380a41396ed63dca39503542")
    ("aaaaaa" . "0b4e7a0e5fe84ad35fb5f95b9ceeac79")
    ("aaaaaaa" . "5d793fc5b00a2348c3fb9ab59e5ca98a")
    ("aaaaaaaa" . "3dbe00a167653a1aaee01d93e77e730e")
    ("aaaaaaaaa" . "552e6a97297c53e592208cf97fbb3b60")
    ("aaaaaaaaaa" . "e09c80c42fda55f9d992e59ca6b3307d")
    ("aaaaaaaaaaa" . "d57f21e6a273781dbf8b7657940f3b03")
    ("aaaaaaaaaaaa" . "45e4812014d83dde5666ebdf5a8ed1ed")
    ("aaaaaaaaaaaaa" . "c162de19c4c3731ca3428769d0cd593d")
    ("aaaaaaaaaaaaaa" . "451599a5f9afa91a0f2097040a796f3d")
    ("aaaaaaaaaaaaaaa" . "12f9cf6998d52dbe773b06f848bb3608")
    ("aaaaaaaaaaaaaaaa" . "23ca472302f49b3ea5592b146a312da0")
    ("aaaaaaaaaaaaaaaaa" . "88e42e96cc71151b6e1938a1699b0a27")
    ("aaaaaaaaaaaaaaaaaa" . "2c60c24e7087e18e45055a33f9a5be91")
    ("aaaaaaaaaaaaaaaaaaa" . "639d76897485360b3147e66e0a8a3d6c")
    ("aaaaaaaaaaaaaaaaaaaa" . "22d42eb002cefa81e9ad604ea57bc01d")
    ("aaaaaaaaaaaaaaaaaaaaa" . "bd049f221af82804c5a2826809337c9b")
    ("aaaaaaaaaaaaaaaaaaaaaa" . "ff49cfac3968dbce26ebe7d4823e58bd")
    ("aaaaaaaaaaaaaaaaaaaaaaa" . "d95dbfee231e34cccb8c04444412ed7d")
    ("aaaaaaaaaaaaaaaaaaaaaaaa" . "40edae4bad0e5bf6d6c2dc5615a86afb")
    ("aaaaaaaaaaaaaaaaaaaaaaaaa" . "a5a8bfa3962f49330227955e24a2e67c")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaa" . "ae791f19bdf77357ff10bb6b0e97e121")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaa" . "aaab9c59a88bf0bdfcb170546c5459d6")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "b0f0545856af1a340acdedce23c54b97")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "f7ce3d7d44f3342107d884bfa90c966a")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "59e794d45697b360e18ba972bada0123")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "3b0845db57c200be6052466f87b2198a")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "5eca9bd3eb07c006cd43ae48dfde7fd3")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "b4f13cb081e412f44e99742cb128a1a5")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "4c660346451b8cf91ef50f4634458d41")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "11db24dc3f6c2145701db08625dd6d76")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "80dad3aad8584778352c68ab06250327")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "1227fe415e79db47285cb2689c93963f")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "8e084f489f1bdf08c39f98ff6447ce6d")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "08b2f2b0864bac1ba1585043362cbec9")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "4697843037d962f62a5a429e611e0f5f")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "10c4da18575c092b486f8ab96c01c02f")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "af205d729450b663f48b11d839a1c8df")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "0d3f91798fac6ee279ec2485b25f1124")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "4c3c7c067634daec9716a80ea886d123")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "d1e358e6e3b707282cdd06e919f7e08c")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "8c6ded4f0af86e0a7e301f8a716c4363")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "4c2d8bcb02d982d7cb77f649c0a2dea8")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "bdb662f765cd310f2a547cab1cfecef6")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "08ff5f7301d30200ab89169f6afdb7af")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "6eb6a030bcce166534b95bc2ab45d9cf")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "1bb77918e5695c944be02c16ae29b25e")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "b6fe77c19f0f0f4946c761d62585bfea")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "e9e7e260dce84ffa6e0e7eb5fd9d37fc")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "eced9e0b81ef2bba605cbc5e2e76a1d0")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "ef1772b6dff9a122358552954ad0df65")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "3b0c8ac703f828b04c6c197006d17218")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "652b906d60af96844ebd21b674f35e93")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "dc2f2f2462a0d72358b2f99389458606")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "762fc2665994b217c52c3c2eb7d9f406")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "cc7ed669cf88f201c3297c6a91e1d18d")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "cced11f7bbbffea2f718903216643648")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "24612f0ce2c9d2cf2b022ef1e027a54f")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "b06521f39153d618550606be297466d5")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "014842d480b571495a4a0363793f7367")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "c743a45e0d2e6a95cb859adae0248435")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "def5d97e01e1219fb2fc8da6c4d6ba2f")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "92cb737f8687ccb93022fdb411a77cca")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "a0d1395c7fb36247bfe2d49376d9d133")
    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .
     "ab75504250558b788f99d1ebd219abf2"))
  "AList of test input strings and stringified message-digests
according to my additional test suite")

#+md5-testing
(defun test-with-testsuite (testsuite)
  (loop for count from 1
        for (source . md5-string) in testsuite
        for md5-digest = (md5sum-sequence source)
        for md5-result-string = (md5-string md5-digest)
        do
        (format
         *trace-output*
         "~2&Test-Case ~D:~%  Input: ~S~%  Required: ~A~%  Returned: ~A~%"
         count source md5-string md5-result-string)
        when (string= md5-string md5-result-string)
        do (format *trace-output* "  OK~%")
        else
        count 1 into failed
        and do (format *trace-output* "  FAILED~%")
        finally
        (format *trace-output*
                "~2&~[All ~D test cases succeeded~:;~:*~D of ~D test cases failed~].~%"
                failed (1- count))
        (return (zerop failed))))

#+md5-testing
(defun test-rfc1321 ()
  (test-with-testsuite +rfc1321-testsuite+))

#+md5-testing
(defun test-other ()
  (test-with-testsuite +other-testsuite+))

#+cmu
(eval-when (:compile-toplevel :execute)
  (setq *features* *old-features*))

#+cmu
(eval-when (:compile-toplevel)
  (setq ext:*inline-expansion-limit* *old-expansion-limit*))
