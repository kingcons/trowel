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

(defun get-node (id)
  "Get the node from *program* with the given ID."
  (graph-utils:lookup-node *program* id))

(defun load-game (name)
  "Reset *program* and *asm-map*. Load a new rom, NAME, to decompile."
  (setf *program* (make-graph :directed? t) *asm-map* (make-hash-table))
  ;; Place the game code at the beginning of NES mapper memory.
  ;; NOTE: This will not work for non-NROM games.
  (let ((rom (romreader:load-rom (app-path "roms/~A.nes" name))))
    (if (zerop (getf (romreader:rom-metadata rom) :mapper-id))
        (progn
          (setf (get-range #x8000) (romreader:rom-prg rom))
          (reset *cpu*))
        (error "Only NROM mapped games are currently supported."))))

(defun now ()
  "Disassemble the current instruction."
  (current-instruction *cpu*))

(defun compute-jumps (origin)
  "Compute all the endpoints of the jump instruction at ORIGIN."
  (case (get-byte origin)
    (#x00        (compute-jump :brk origin))
    ((#x4c #x6c) (compute-jump :jmp origin))
    (#x20        (compute-jump :jsr origin))
    (#x40        (compute-jump :rti origin))
    (#x60        (compute-jump :rts origin))
    (otherwise   (compute-jump :branch origin))))

(defun get-basic-block (&optional (pc (cpu-pc *cpu*)))
  "Get the opcodes from the supplied PC to the next branch."
  (let ((branch-ops '(:bcc :bcs :beq :bmi :bne :bpl :bvc :bvs
                      :brk :jmp :jsr :rti :rts)))
    (loop for address = pc then (+ address len)
       for (len asm) = (disasm-ins address #'sexpify-instruction)
       collect asm into code until (member (first asm) branch-ops)
       finally (return (list address code)))))

(defun add-basic-block (start code)
  "Add a basic block for CODE to the CFG, keyed by START in *asm-map*."
  (setf (gethash start *asm-map*) (add-node *program* code)))

(defun decompile-block (start)
  "Decompile the block at START returning the CFG node and the
addresses of its children."
  (destructuring-bind (end code) (get-basic-block start)
    (list (add-basic-block start code)
          (compute-jumps end))))

(defun process-child (address parent)
  "Take the ADDRESS of a child and add an edge to it from PARENT,
recursively calling BUILD-CFG if necessary."
  (if-let (seen-p (gethash address *asm-map*))
    (add-edge *program* parent seen-p)
    (add-edge *program* parent (build-cfg address))))

;; differences from NESrev:
;; NESrev builds up a metadata table listing each byte in rom
;;  as one of: CODE, LABEL, DATA
;; NESrev marks opcode args as code, not data
;; NESrev recursively processes all 3 code vectors: fffa, fffc, fffe

(defun build-cfg (pc)
  "Given a PC address for the root, build a Control Flow Graph for the
currently loaded ROM."
  (destructuring-bind (node-id children) (decompile-block pc)
    (mapc (lambda (x) (process-child x node-id)) children)
    node-id))

(defun decompile-rom ()
  "Decompile the current binary."
  (build-cfg (cpu-pc *cpu*))
  *program*)
