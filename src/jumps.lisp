(in-package :trowel)

(defun jump-table-p (origin)
  "Does the procedure ORIGIN jumps to fool with the return address on
the stack?"
  ;; TODO: Check for stuff being pushed on the stack before the PLA?
  (let ((code (second (get-basic-block (get-word (1+ origin))))))
    (let ((pla-index (position :pla code :key #'first))
          (rts-index (position :rts code :key #'first)))
      (and pla-index (< pla-index (or rts-index #x10000))))))

(defun list-jumps (origin)
  ;; TODO: There must be a better way. What is it?
  "Given a starting address, ORIGIN, return each successive word that
points to a valid opcode."
  (loop for pc = origin then (+ pc 2)
     for opcode = (get-word pc)
     while (get-instruction opcode) collect pc))

(defgeneric compute-jump (opcode origin)
  (:documentation "Given an OPCODE and an ORIGIN, compute all possible
destinations of the jump."))

(defmethod compute-jump ((opcode (eql :brk)) origin)
  (6502::stack-push-word (1+ origin) *cpu*)
  (list (get-word #xfffe)))

(defmethod compute-jump ((opcode (eql :jmp)) origin)
  (let ((destination (ecase (get-byte origin)
                       (#x4c (get-word (1+ origin)))
                       (#x6c (get-word (get-word (1+ origin)) t)))))
    (if (= origin destination) ; branch-to-self, waiting for an NMI
        (progn ; go ahead and do an NMI
          (6502::stack-push-word origin *cpu*)
          (list (get-word #xfffa)))
        (list destination))))

(defmethod compute-jump ((opcode (eql :jsr)) origin)
  (let ((length (first (disasm-ins origin))))
    (6502::stack-push-word (+ origin length) *cpu*)
    (if (jump-table-p origin)
        (list-jumps (+ origin length))
        (list (get-word (1+ origin))))))

(defmethod compute-jump ((opcode (eql :rti)) origin)
  (declare (ignore origin))
  (list (6502::stack-pop-word *cpu*)))

(defmethod compute-jump ((opcode (eql :rts)) origin)
  (declare (ignore origin))
  (list (6502::stack-pop-word *cpu*)))

(defmethod compute-jump ((opcode (eql :branch)) origin)
  (let ((offset (get-byte (1+ origin)))
        (length (first (disasm-ins origin))))
    (let ((branch (if (logbitp 7 offset)
                      (- origin (- #x100 offset length))
                      (+ origin (+ length offset)))))
      (list branch (+ origin length)))))
