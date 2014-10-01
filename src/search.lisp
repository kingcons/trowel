(in-package :trowel)

;; TODO: Build a query DSL?
;; i.e. find-first, find-all, etc. (find-first *rom* '((lda _) (sta _)))

(defmacro disasm-until (start opcode &key (style :print))
  "Disassemble memory from START until OPCODE is reached.
OPCODE may be an opcode byte (e.g. #x10) or mnemonic (e.g. 'bpl)
or a list of bytes or mnemonics. Style may be either :print or :list."
  ;; TODO: Factor out etypecase repetition. Combinators?
  (let ((styler (if (eq style :print)
                    'print-instruction
                    'sexpify-instruction)))
    `(loop with index = ,start
        for (step result) = (cl-6502::disasm-ins index ',styler)
        do (incf index step)
        when ,(eq style :list) collect result
        until ,(etypecase opcode
                 (fixnum `(= (get-byte index) ',opcode))
                 (symbol `(let ((op-meta (aref *opcode-meta* (get-byte index))))
                            (eq (first op-meta) ',opcode)))
                 (list (etypecase (first opcode)
                         (fixnum `(member (get-byte index) ',opcode))
                         (symbol `(let ((op-meta (aref *opcode-meta* (get-byte index))))
                                    (member (first op-meta) ,opcode))))))
        finally (format t "stop: ~4,'0x" index))))
