;;;; Transcript contracts, separate from executable ABCL/R2 session fixtures.
;;;; :observe is an observational baseline; :defect must reproduce a known
;;;; failure signature. Neither is a language-compatibility success.
(defparameter *suites*
  '(("original-tests"
     ("future-raw" :observe t :timeout 5 :required ("A
B
C
D
E
F
G
"))
     ("reify-args" :observe t :timeout 5 :required ("ORIGINAL-PASS my-if" "ORIGINAL-PASS ef" "ORIGINAL-PASS interrupt" "ORIGINAL-PASS no-interrupt") :forbidden ("ORIGINAL-FAIL"))
     ("reify-first-excerpt" :observe t :timeout 5 :required ("ORIGINAL-PASS my-if-excerpt" "ORIGINAL-PASS ef-excerpt") :forbidden ("ORIGINAL-FAIL"))
     ("inherit" :observe t :timeout 5 :required ("ORIGINAL-PASS bird" "ORIGINAL-PASS emu-flight" "ORIGINAL-PASS emu-feathers") :forbidden ("ORIGINAL-FAIL"))
     ("user-script" :observe t :timeout 5 :required ("ORIGINAL-PASS before" "ORIGINAL-PASS audience-before" "ORIGINAL-PASS after" "ORIGINAL-PASS audience-after") :forbidden ("ORIGINAL-FAIL"))
     ("meta-meta" :observe t :timeout 5 :required ("ORIGINAL-PASS twice") :forbidden ("ORIGINAL-FAIL"))
     ("fault-tolerant" :observe t :timeout 5 :required ("Count is 10.") :forbidden ("ORIGINAL-FAIL"))
     ("express-original" :observe t :timeout 5 :required ("ORIGINAL-PASS express-finished") :forbidden ("ORIGINAL-FAIL"))
     ("pcontrol-fib" :observe t :timeout 5 :cwd "@OUT@/examples/pcontrol/" :required ("ORIGINAL-PASS priority-fib" "ORIGINAL-PASS fifo-fib" "Fibonacci(4) = (:ANS 3)") :forbidden ("ORIGINAL-FAIL"))
     ("time-warp" :observe t :timeout 5 :cwd "@OUT@/examples/time-warp/" :required ("ORIGINAL-END time-warp") :forbidden ("ORIGINAL-FAIL")))
    ("parser-tests"
     ("session" :prefix "PARSER" :count 16 :timeout 15
      :required ("PARSER-END" "A
B
C
D
E
F
G
" "PARSER-PASS same-ast" "PARSER-PASS reply-context" "PARSER-PASS fixnum-rejected" "PARSER-PASS values-type-rejected" "PARSER-PASS missing-operand" "PARSER-PASS extra-operand" "PARSER-PASS single-evaluation" "PARSER-PASS lexical-binding" "PARSER-PASS conditional-lazy" "PARSER-PASS host-cond" "PARSER-PASS provide-symbol" "PARSER-PASS provide-string" "PARSER-PASS provide-character-rejected" "PARSER-PASS provide-number-rejected" "PARSER-PASS provide-eql" "PARSER-PASS reply-forward")))
    ("serialization-tests"
     ("session" :prefix "SERIAL" :count 13 :timeout 20 :required ("SERIAL-END") :forbidden ("No return messages!")))
    ("tutorial-semantics"
     ("checks" :prefix "SEMANTIC" :count 6 :timeout 15 :required ("SEMANTIC-END")))
    ("reading-tests"
     ("session" :prefix "READING" :count 40 :required ("READING-END")))
    ("group-tests"
     ("mcp" :prefix "GROUP" :count 7 :required ("GROUP-END") :forbidden ("No return messages!"))
     ("tower" :prefix "GROUP" :count 8 :required ("GROUP-END") :forbidden ("No return messages!"))
     ("retry" :prefix "GROUP" :count 7 :required ("GROUP-END") :forbidden ("No return messages!"))
     ("timewarp-app" :prefix "GROUP" :count 5 :cwd "@OUT@/examples/time-warp/" :required ("GROUP-END") :forbidden ("No return messages!"))
     ("timewarp-eval" :prefix "GROUP" :count 6 :cwd "@OUT@/examples/time-warp/" :required ("GROUP-END") :forbidden ("No return messages!"))
     ("pcontrol" :prefix "GROUP" :count 13 :cwd "@OUT@/examples/pcontrol/" :required ("GROUP-END") :forbidden ("No return messages!"))
     ("heavy" :prefix "GROUP" :count 7 :required ("GROUP-END") :forbidden ("No return messages!"))
     ("heavy-old" :prefix "GROUP" :count 7 :required ("GROUP-END") :forbidden ("No return messages!"))
     ("report" :prefix "GROUP" :count 7 :required ("GROUP-END") :forbidden ("No return messages!"))
     ("express" :prefix "GROUP" :count 7 :required ("GROUP-END") :forbidden ("No return messages!"))
     ("express-old" :prefix "GROUP" :count 7 :required ("GROUP-END") :forbidden ("No return messages!"))
     ("interrupt" :prefix "GROUP" :count 7 :required ("GROUP-END") :forbidden ("No return messages!")))
    ("reflection-tests"
     ("inherit-group" :prefix "REFLECTION" :count 18 :required ("REFLECTION-END") :forbidden ("No return messages!"))
     ("dispatch-new" :prefix "CACHE" :count 19 :required ("CACHE-END") :forbidden ("No return messages!"))
     ("dispatch-old" :prefix "CACHE" :count 19 :required ("CACHE-END") :forbidden ("No return messages!"))
     ("script" :prefix "REFLECTION" :count 11 :required ("REFLECTION-END") :forbidden ("No return messages!"))
     ("mcp" :prefix "REFLECTION" :count 4 :required ("REFLECTION-END") :forbidden ("No return messages!"))
     ("tower" :prefix "REFLECTION" :count 3 :required ("REFLECTION-END" "Meta-meta receievs message" "Meta-gen receievs message") :forbidden ("No return messages!")))
    ("guard-tests"
     ("counter" :prefix "GUARD" :count 11 :required ("GUARD-END") :forbidden ("No return messages!"))
     ("extensions" :prefix "GUARD" :count 10 :required ("GUARD-END") :forbidden ("No return messages!"))
     ("unmatched" :prefix "GUARD" :count 3 :required ("GUARD-END" "cannot handle the message :UNKNOWN") :forbidden ("No return messages!")))
    ("heavy-old-tests"
     ("legacy" :prefix "LEGACY" :count 10 :required ("LEGACY-END" "cannot handle message :LEGACY-UNKNOWN, ignores it.") :forbidden ("No return messages!"))
     ("current" :prefix "LEGACY" :count 10 :required ("LEGACY-END" "cannot handle the message :LEGACY-UNKNOWN") :forbidden ("No return messages!")))
    ("pcontrol-tests"
     ("queue" :prefix "PC" :count 5 :cwd "@OUT@" :required ("PC-END") :forbidden ("No return messages!"))
     ("fib" :prefix "PC" :count 4 :cwd "@OUT@" :required ("PC-END") :forbidden ("No return messages!") :counts (("Fibonacci(4) = (:ANS 3)" 2) ("Fibonacci(6) = (:ANS 8)" 2)))
     ("qsort" :prefix "PC" :count 3 :cwd "@OUT@" :required ("PC-END") :forbidden ("No return messages!")))
    ("timewarp-tests"
     ("histories" :prefix "TW" :count 12 :timeout 30 :cwd "@OUT@" :required ("TW-END") :forbidden ("No return messages!"))
     ("wash" :prefix "TW" :count 4 :timeout 30 :cwd "@OUT@" :required ("TW-END") :forbidden ("No return messages!") :counts ((":WASHER 0 :CAR 1 :TIME 1" 2) (":WASHER 1 :CAR 2 :TIME 2" 2) (":WASHER 2 :CAR 3 :TIME 3" 2)))
     ("rollback" :prefix "TW" :count 12 :timeout 30 :cwd "@OUT@" :required ("TW-END" ":ANTI-MESSAGE (:VALUE 10)") :forbidden ("No return messages!") :counts ((" back to 0 by " 2) ("Cancelling " 1)))
     ("rollback-fifo" :prefix "TW" :count 12 :timeout 30 :cwd "@OUT@" :required ("TW-END" ":ANTI-MESSAGE (:VALUE 10)") :forbidden ("No return messages!") :counts ((" back to 0 by " 2) ("Cancelling " 1))))
    ("timewarp-variants"
     ("historical-NR-wash" :defect t :timeout 10 :cwd "@OUT@/historical/" :required ("cannot handle the message (#<FUNCTION" "No return messages!"))
     ("historical-NS-wash" :defect t :timeout 10 :cwd "@OUT@/historical/" :required ("Illegal expression (:SCRIPT" "No return messages!"))
     ("standard" :prefix "VARIANT" :count 15 :cwd "@OUT@" :required ("VARIANT-END") :forbidden ("No return messages!"))
     ("NR" :prefix "VARIANT" :count 15 :cwd "@OUT@" :required ("VARIANT-END") :forbidden ("No return messages!"))
     ("NS" :prefix "VARIANT" :count 15 :cwd "@OUT@" :required ("VARIANT-END") :forbidden ("No return messages!"))
     ("NR-wash" :prefix "VARIANT" :count 4 :timeout 15 :cwd "@OUT@" :required ("VARIANT-END" "RESULT=(:FIN") :forbidden ("No return messages!" "cannot handle the message" "Illegal expression") :counts ((":WASHER 0 :CAR 1 :TIME 1" 1) (":WASHER 1 :CAR 2 :TIME 2" 1) (":WASHER 2 :CAR 3 :TIME 3" 1)))
     ("NS-wash" :prefix "VARIANT" :count 4 :timeout 15 :cwd "@OUT@" :required ("VARIANT-END" "RESULT=(:FIN") :forbidden ("No return messages!" "cannot handle the message" "Illegal expression") :counts ((":WASHER 0 :CAR 1 :TIME 1" 1) (":WASHER 1 :CAR 2 :TIME 2" 1) (":WASHER 2 :CAR 3 :TIME 3" 1))))
    ("retry-tests"
     ("retry" :prefix "RETRY" :count 6 :required ("RETRY-END") :forbidden ("No return messages!") :counts (("Count is 10." 2) ("Count is 20." 1))))
    ("break-tests"
     ("break" :prefix "BREAK" :count 6 :required ("BREAK-END" "Z=123" "Y=123" "(X)" "MISSING-BREAK-BINDING is unbound." "Commands:" "probe") :forbidden ("No return messages!")))
    ("express-tests"
     ("express/active-nonatomic" :prefix "EXPRESS" :count 5 :required ("EXPRESS-END") :forbidden ("No return messages!") :counts (("Tick of the clock is inspected by an express message." 42) ("The clock got an order to stop." 2) ("The clock is stopped." 2)))
     ("express/active-lock" :defect t :required ("EXPRESS-TRACE grab" "EXPRESS-TRACE lock-arrival" "No return messages!" "Message=:GRAB" "EXPRESS-LOCK-BLOCKED-END") :forbidden ("EXPRESS-FAIL" ";An error is detected." "Unhandled"))
     ("express/queued-normal" :prefix "EXPRESS" :count 10 :required ("EXPRESS-END") :forbidden ("No return messages!"))
     ("express/queued-express" :prefix "EXPRESS" :count 10 :required ("EXPRESS-END") :forbidden ("No return messages!"))
     ("express/queued-mixed" :prefix "EXPRESS" :count 12 :required ("EXPRESS-END") :forbidden ("No return messages!"))
     ("express/dispatch" :prefix "EXPRESS" :count 5 :required ("EXPRESS-END") :forbidden ("No return messages!"))
     ("express-old/dispatch" :prefix "EXPRESS" :count 5 :required ("EXPRESS-END") :forbidden ("No return messages!")))
    ("interrupt-tests"
     ("session" :prefix "INTERRUPT" :count 13 :required ("INTERRUPT-END" "Being interrupted..." "Context is caught." "Interruption is ignored.") :forbidden ("No return messages!")))
    ("multicast-tests"
     ("original" :prefix "MULTICAST" :count 6 :required ("MULTICAST-END") :forbidden ("No return messages!"))
     ("clients" :prefix "MULTICAST" :count 13 :required ("MULTICAST-END") :forbidden ("No return messages!"))
     ("collector" :prefix "MULTICAST" :count 5 :required ("MULTICAST-END") :forbidden ("No return messages!")))
    ("copy-tests"
     ("basic" :prefix "COPY" :count 10 :required ("COPY-END") :forbidden ("No return messages!"))
     ("original" :prefix "COPY" :count 2 :required ("COPY-END" "Fib(5)=") :forbidden ("No return messages!"))
     ("queue" :prefix "COPY" :count 8 :required ("COPY-END") :forbidden ("No return messages!"))
     ("options" :prefix "COPY" :count 9 :required ("COPY-END") :forbidden ("No return messages!"))
     ("state" :prefix "COPY" :count 23 :required ("COPY-END") :forbidden ("No return messages!")))
    ("watch-tests"
     ("object" :prefix "WATCH" :count 13 :required ("WATCH-END") :forbidden ("No return messages!"))
     ("group" :prefix "WATCH" :count 7 :required ("WATCH-END") :forbidden ("No return messages!"))
     ("toplevel" :prefix "WATCH" :count 7 :required ("WATCH-END") :forbidden ("No return messages!"))
     ("management" :prefix "WATCH" :count 4 :required ("WATCH-END") :forbidden ("No return messages!"))
     ("watching" :prefix "WATCH" :count 7 :required ("WATCH-END") :forbidden ("No return messages!"))
     ("reporter" :prefix "WATCH" :count 3 :required ("WATCH-END" "DEFAULT-PROBE from" "WATCH-FILTER :FILTER-PROBE") :forbidden ("No return messages!")))
    ("report-tests"
     ("formatter" :prefix "REPORT" :count 10 :required ("REPORT-END") :forbidden ("No return messages!"))
     ("basic" :prefix "REPORT" :count 14 :required ("REPORT-END") :forbidden ("No return messages!"))
     ("filters" :prefix "REPORT" :count 18 :required ("REPORT-END" "from :SENDER-MARKER (reply=:REPLY-MARKER)") :forbidden ("No return messages!" "from :REPLY-MARKER (reply=:SENDER-MARKER)"))
     ("management" :prefix "REPORT" :count 8 :required ("REPORT-END") :forbidden ("No return messages!"))
     ("lifecycle" :prefix "REPORT" :count 5 :required ("REPORT-END") :forbidden ("No return messages!"))
     ("unmatched" :prefix "REPORT" :count 4 :required ("REPORT-END" "cannot handle the message :UNMATCHED-PROBE") :forbidden ("No return messages!")))
    ("host-event-tests"
     ("direct" :prefix "EVENT" :count 13 :required ("EVENT-END" "[Signal (:RESTORED-DEFAULT) is ignored]") :forbidden ("No return messages!"))
     ("slot" :prefix "EVENT" :count 12 :required ("EVENT-END") :forbidden ("No return messages!")))
    ("loading-tests"
     ("cache" :prefix "LOADING" :count 14 :timeout 30 :required ("LOADING-END" "cache.lsp") :forbidden ("No return messages!"))
     ("modules" :prefix "LOADING" :count 13 :required ("LOADING-END") :forbidden ("No return messages!"))
     ("context" :prefix "LOADING" :count 8 :required ("LOADING-END") :forbidden ("No return messages!")))
    ("tutorial-tests"
     ("01-basics" :prefix "CHECK" :count 13 :timeout 25)
     ("02-accounts" :prefix "CHECK" :count 8 :timeout 25)
     ("03-jobs" :prefix "CHECK" :count 3 :timeout 25)
     ("04-futures" :prefix "CHECK" :count 4 :timeout 25)
     ("05-reflection" :prefix "CHECK" :count 12 :timeout 25)
     ("06-express" :defect t :timeout 25
      :required ("No return messages!" "Message=:GRAB" "EXPRESS-BLOCKED-END")
      :forbidden ("CHECK-PASS" "CHECK-FAIL" ";An error is detected."))
     ("07-solutions" :prefix "CHECK" :count 6 :timeout 25))))
