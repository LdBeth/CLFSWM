#! /bin/sh

clisp load.lisp "(print 'toto) (print (+ 2 2))" "(leave-frame)" " quit  "
#cmucl -load load.lisp "(print 'toto)" "(print (+ 2 2))" "(leave-frame)" "quit"
#sbcl --load load.lisp "(print 'toto)" "(print (+ 2 2))" "(leave-frame)" "quit"
#ccl --load load.lisp -- "(print 'toto)" "(print (+ 2 2))" "(leave-frame)" "quit"
#/tmp/local/bin/clfswm-client "(print 'toto)" "(print 'toto) (print (+ 2 2))" "(leave-frame)" "quit"
