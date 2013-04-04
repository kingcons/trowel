(in-package :trowel)

;; A very simple set of exploratory decompiling tools.
;; Example:
;> (load-game "smb")
; #S(NES)
;> (now)
; (:SEI)
;> (get-basic-block)
; (32781
;  ((:SEI) (:CLD) (:LDA :|#$10|) (:STA :$2000) (:LDX :|#$FF|) (:TXS)
;   (:LDA :$2002) (:BPL :&FB)))
;> (compute-jumps 32781)
; '(32777 32783)
;> (decompile-rom)
; ... do a bit, crash.
;> (inspect *program*)

(defvar *program* (make-graph :directed? t)
  "A control flow graph of the program.")

(defvar *asm-map* (make-hash-table)
  "A hash-table with addresses as keys and node ids as values.")

(defun load-game (name)
  "Reset *program* and *asm-map*. Load a new rom, NAME, to decompile."
  (setf *program* (make-graph :directed? t) *asm-map* (make-hash-table))
  (load-rom (app-path (format nil "roms/~A.nes" name))))

(defun now ()
  "Disassemble the current instruction."
  (current-instruction (nes-cpu *nes*)))

(defun compute-jumps (origin)
  "Compute all the endpoints of the jump instruction at ORIGIN."
  (case (get-byte origin)
    (#x00        (compute-jump :brk origin))
    ((#x4c #x6c) (compute-jump :jmp origin))
    (#x20        (compute-jump :jsr origin))
    (#x40        (compute-jump :rti origin))
    (#x60        (compute-jump :rts origin))
    (otherwise   (compute-jump :branch origin))))

(defun get-basic-block (&optional (pc (cpu-pc (nes-cpu *nes*))))
  "Get the opcodes from the supplied PC to the next branch."
  (let ((branch-ops '(:bcc :bcs :beq :bmi :bne :bpl :bvc :bvs
                      :brk :jmp :jsr :rti :rts)))
    (loop for address = pc then (+ address len)
       for (len asm) = (disasm-ins address #'sexpify-instruction)
       collect asm into code until (member (first asm) branch-ops)
       finally (return (list address code)))))

(defun decompile-block (start)
  "Decompile the block at START returning the last opcode's address."
  (destructuring-bind (end code) (get-basic-block start)
    (let ((node (add-node *program* code)))
      (setf (gethash start *asm-map*) node)
      (list node end))))

(defun build-cfg (pc)
  "Given a PC address for the root, build a Control Flow Graph for the
currently loaded ROM."
  (destructuring-bind (node-id jump-addr) (decompile-block pc)
    (loop for destination in (compute-jumps jump-addr)
       do (if-let (seen-p (gethash destination *asm-map*))
            (add-edge *program* node-id seen-p)
            ;; KLUDGE: Recursive DFS! Blow all the stacks!
            (add-edge *program* node-id (build-cfg destination))))
    node-id))

(defun decompile-rom ()
  "Decompile the current binary."
  (build-cfg (cpu-pc (nes-cpu *nes*)))
  *program*)
