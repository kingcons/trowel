#!/bin/sh
sbcl --eval "(ql:quickload '(trowel sb-introspect cl-api))" \
     --eval "(cl-api:api-gen :trowel \"docs/trowel.html\")" \
     --eval "(progn (terpri) (sb-ext:quit))"
