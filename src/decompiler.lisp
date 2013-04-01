(in-package :trowel)

;; On Jump Tables:
;; A jump table is basically a compiled switch statement. A JSR
;; pushes the initial PC on the stack. A Jump Table pulls it off
;; and screws with it to decide where to jump. In practice, this
;; means that the possible destinations of a jump table immediately
;; follow the JSR instruction in RAM. The problem is we don't know
;; how many possible jumps there are, i.e. how many u16s to read.

;; A possible solution:
;; When a jump subroutine calls PLA before RTS, i.e. modifies the
;; return address on the stack, immediately backtrack to the return
;; address and try jumping to each word after the JSR until error.
;; TODO: What about data that follow jump tables, i.e. WSelectBufferTemplate?

;; A very simple set of exploratory decompiling tools.
;; Example:
;> (load-game "smb")
;  #S(NES)
;> (get-basic-block)
; '(bunch o disasm)
;> (now)
; '(:jsr :$90cc)
;> (maybe-jump)
;  37068
;> (decompile-rom)
; ... do a bit, crash.
; (inspect *program*)

(defun load-game (name)
  (setf *program* (make-hash-table) *asm-map* (make-hash-table))
  (load-rom (app-path (format nil "roms/~A.nes" name))))

(defun now ()
  (current-instruction (nes-cpu *nes*)))

(defvar *program* (make-hash-table)
  "A hash-table with function names as keys to asm block values.")

(defvar *asm-map* (make-hash-table)
  "A hash-table with memory addresses as keys and function names as values.")

(defun node-id ()
  (intern (format nil "FN-~a" (gensym)) :keyword))

(defun next-pc ()
  (with-accessors ((cpu nes-cpu)) *nes*
    (let ((opcode (get-byte (cpu-pc cpu))))
      (incf (cpu-pc cpu))
      (ecase opcode
        (#x00 ; brk
         (6502::stack-push-word (6502:wrap-word (1+ (cpu-pc cpu))) cpu)
         (get-word #xfffe))
        (#x4c ; jmp, absolute
         (6502:absolute cpu))
        (#x6c ; jmp, indirect
         (6502:indirect cpu))
        (#x20 ; jsr
         (6502::stack-push-word (6502:wrap-word (1+ (cpu-pc cpu))) cpu)
         (6502:absolute cpu))
        (#x40 ; rti
         (6502::stack-pop-word cpu))
        (#x60 ; rts
         (1+ (6502::stack-pop-word cpu)))))))

(defun maybe-jump ()
  ;; TODO: We probably want jump-to-self, rts, and rti to signal
  ;; "end-of-function". How are we going to restore higher-level structure?
  ;; What about jsr, jmp, brk?
  (with-accessors ((cpu nes-cpu)) *nes*
    (let ((length (first (disasm-ins (cpu-pc cpu))))
          (orig (cpu-pc cpu))
          (dest (next-pc)))
      (cond ((= dest orig) ; branch-to-self, waiting for an NMI.
             (6502:nmi cpu))
            ((gethash dest *asm-map*) ; we've been there before.
             (incf (cpu-pc cpu) (1- length)))
            (t ; let's check it out!
             (setf (cpu-pc cpu) dest))))))

(defun get-basic-block ()
  (with-accessors ((cpu nes-cpu)) *nes*
    (loop for (len asm) = (disasm-ins (cpu-pc cpu) #'sexpify-instruction)
       collect asm until (member (first asm) '(:jmp :jsr :brk :rti :rts))
       do (incf (cpu-pc cpu) len))))

(defun process-block (code pc)
  (let ((name (node-id)))
    (setf (gethash name *program*) code
          (gethash pc *asm-map*) name))
  (maybe-jump))

(defun decompile-rom ()
  ;; TODO: Currently dying because JumpEngine sets up an address to jump to
  ;; and we don't simulate that, wind up in a field of 0s. (i.e. brk)
  "Decompile the current binary."
  (loop for pc = (cpu-pc (nes-cpu *nes*))
     for code = (get-basic-block)
     do (process-block code pc))
  *program*)

(defun rename-node (old new)
  "Rename a node in the *PROGRAM*."
  ; TODO: Update *asm-map* table too. Store index in *program* too?
  (setf (gethash new *program*) (gethash old *program*))
  (remhash old *program*))
