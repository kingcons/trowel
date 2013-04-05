(defpackage :trowel
  (:use :cl)
  (:import-from :famiclom-conf #:app-path)
  (:import-from :famiclom #:load-rom
                          #:nes-cpu
                          #:nes-mapper
                          #:get-byte
                          #:get-word
                          #:reset
                          #:*nes*)
  (:import-from :6502 #:cpu-pc
                      #:disasm
                      #:disasm-ins
                      #:get-instruction
                      #:sexpify-instruction
                      #:current-instruction)
  (:import-from :romreader #:rom-metadata)
  (:import-from :alexandria #:if-let)
  (:import-from :graph-utils #:make-graph #:add-node #:add-edge))