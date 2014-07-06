#!/usr/local/bin/csi -s

;; labrea - an optimising compiler for brainf--k.
;; https://github.com/jdpage/labrea
;;
;; This is a rewrite of the tarpit compiler (https://github.com/jdpage/tarpit),
;; which only does some trivial optimisations and only compiles for IA-32 ELF.
;;
;; This version supports both IA-32 and AMD64, and outputs assembly instead of
;; machine code, meaning that it can be compiled and linked into binary formats
;; other than ELF.

(require-extension srfi-1)
(require-extension srfi-13)
(require-extension srfi-28)
(require-extension srfi-37)
(require-extension matchable)

;; stage 0: read in
;;
;; Before any stages, read in the code. This produces a list of operations as
;; specified in the file.
;;
;; source | ir
;; <      | 'look-cell-left
;; >      | 'look-cell-right
;; +      | 'increment-current-cell
;; -      | 'decrement-current-cell
;; ,      | 'read-char-into-current-cell
;; .      | 'write-char-from-current-cell
;; [      | 'jump-forward-if-current-cell-zero
;; ]      | 'jump-backward-unless-current-cell-zero
;; else   | #f

(define (read-source)
  (filter (lambda (x) x)
          (let loop ()
            (let ([c (read-char)])
              (if (eof-object? c)
                '()
                (cons
                  (match c
                         [#\< `(look-cell-left)]
                         [#\> `(look-cell-right)]
                         [#\+ `(increment-current-cell)]
                         [#\- `(decrement-current-cell)]
                         [#\, `(read-char-into-current-cell)]
                         [#\. `(write-char-from-current-cell)]
                         [#\[ `(jump-forward-if-current-cell-zero)]
                         [#\] `(jump-backward-unless-current-cell-zero)]
                         [_ #f])
                  (loop)))))))

;; stage 1: collect loops
;;
;; this is a rewrite-only stage, collecting pairs of jf/z and jb/nz into loops. 
;;
;; this is the only stage where an error can be thrown, if unmatched brackets
;; exist.
;;
;; This stage introduces these symbols:
;;
;; 'loop-until-current-cell-zero
;;
;; And eliminates these symbols:
;;
;; 'jump-forward-if-current-cell-zero
;; 'jump-backward-unless-current-cell-zero

(define (push-value groups value)
  (let ([current-group (car groups)]
        [other-groups (cdr groups)])
    (cons (cons value current-group)
          other-groups)))

(define (pop-group groups)
  (let ([closing-group (car groups)]
        [other-groups (cdr groups)])
    (push-value other-groups
                `(loop-until-current-cell-zero ,(reverse closing-group)))))

(define (new-group groups)
  (cons '() groups))

(define (collect-loops ir)
  (let loop ([remaining ir]
             [groups '(())])
    (match remaining
           [(`(jump-forward-if-current-cell-zero) . rem)
            (loop rem (new-group groups))]
           [(`(jump-backward-unless-current-cell-zero) . rem)
            (if (> (length groups) 1)
              (loop rem (pop-group groups))
              (error "unmatched ] in source"))]
           [(i . rem)
            (loop rem (push-value groups i))]
           [()
            (if (= (length groups) 1)
              (reverse (car groups))
              (error "unmatched [ in source"))])))

(define (ir-for-each ir func)
  (let loop ([remaining ir])
    (match remaining
           [((control-command (body ...) . args) . rem)
            (cons `(,control-command ,(loop body) ,@args)
                  (loop rem))]
           [(sym . rem)
            (cons (func sym) (loop rem))]
           [()
            '()])))

;; stage 1b: remove empty loops
;;
;; Delete loops with empty bodies. 
;;
;; This stage introduces no symbols and eliminates no symbols.

(define (eliminate-empty-loops ir)
  (let loop ([remaining ir])
    (match remaining
           [(`(loop-until-current-cell-zero ()) . rem)
            (loop rem)]
           [(sym . rem)
            (cons sym (loop rem))]
           [()
            '()])))

;; stage 2: condense arithmetic
;;
;; Collect sequences of 'increment-current-cell and 'decrement-current-cell into
;; appropriate adds and subtracts.
;;
;; This stage introduces these symbols:
;;
;; 'add-to-current-cell
;;
;; And eliminates these symbols:
;;
;; 'increment-current-cell
;; 'decrement-current-cell

(define (condense-arithmetic ir)
  (let loop ([remaining ir]
             [counter 0])
    (match remaining
           [(`(increment-current-cell) . rem)
            (loop rem (+ counter 1))]
           [(`(decrement-current-cell) . rem)
            (loop rem (- counter 1))]
           [_
             (=> cancel-match)
             (if (zero? counter)
               (cancel-match)
               (cons `(add-to-current-cell ,counter)
                     (loop remaining 0)))]
           [((control-command (body ...) . args) . rem)
            (cons `(,control-command ,(loop body 0) ,@args)
                  (loop rem 0))]
           [(sym . rem)
            (cons sym (loop rem 0))]
           [()
            '()])))

;; stage 2b: modulus arithmetic
;; 
;; Brainf--k only knows about one-byte values, so we can make all arithmetic
;; be mod 2^8
;;
;; This stage introduces no symbols and eliminates no symbols.

(define (modulus-arithmetic ir)
  (ir-for-each ir
               (match-lambda
                 [`(add-to-current-cell ,x)
                   `(add-to-current-cell ,(remainder x 256))]
                 [x x])))

;; stage 3: condense memory offsets
;; 
;; Collect sequences of 'look-cell-left and 'look-cell-right
;;
;; This stage introduces these symbols:
;;
;; 'look-cell-offset
;;
;; And eliminates these symbols:
;;
;; 'look-cell-left
;; 'look-cell-right

(define (condense-memory-offsets ir)
  (let loop ([remaining ir]
             [counter 0])
    (match remaining
           [(`(look-cell-left) . rem)
            (loop rem (- counter 1))]
           [(`(look-cell-right) . rem)
            (loop rem (+ counter 1))]
           [_
             (=> cancel-match)
             (if (zero? counter)
               (cancel-match)
               (cons `(look-cell-offset ,counter)
                     (loop remaining 0)))]
           [((control-command (body ...) . args) . rem)
            (cons `(,control-command ,(loop body 0) ,@args)
                  (loop rem 0))]
           [(sym . rem)
            (cons sym (loop rem 0))]
           [()
            '()])))

;; stage 4: change cell accesses to be current-relative
;;
;; After this stage, the current cell pointer is only changed if necessary.
;; Specifically, the pointer changes are moved as far right as possible, to the
;; ends of loops or just before they begin.
;;
;; This stage introduces these symbols:
;;
;; 'add-to-offset-cell
;; 'read-char-into-offset-cell
;; 'write-char-from-offset-cell
;;
;; And eliminates these symbols
;;
;; 'add-to-current-cell
;; 'read-char-into-current-cell
;; 'write-char-from-current-cell

(define (reduce-cell-pointer-changes ir)
  (let loop ([remaining ir]
             [offset 0])
    (match remaining
           ;; collect offsets
           [(`(look-cell-offset ,off) . rem)
            (loop rem (+ offset off))]

           ;; write them out before controls and at end
           [((control-command (body ...) . args) . rem)
            (let* ([cc `((,control-command
                           ,(reduce-cell-pointer-changes body)
                           ,@args))]
                   [ops (if (zero? offset)
                          cc
                          (cons `(look-cell-offset ,offset) cc))])
              (append ops (loop rem 0)))]
           [()
            (if (zero? offset)
              `()
              `((look-cell-offset ,offset)))]

           ;; replace -current-cell with -offset-cell
           [(`(add-to-current-cell ,value) . rem)
            (cons `(add-to-offset-cell ,offset ,value)
                  (loop rem offset))]
           [(`(read-char-into-current-cell) . rem)
            (cons `(read-char-into-offset-cell ,offset)
                  (loop rem offset))]
           [(`(write-char-from-current-cell) . rem)
            (cons `(write-char-from-offset-cell ,offset)
                  (loop rem offset))]

           ;; pass-through
           [(sym . rem)
            (cons sym (loop rem 0))])))

;; stage 4b: convert loop/current-cell to loop/offset-cell
;;
;; After this stage, loops which do not change the current cell are changed to 
;; use cell offsets instead of the current cell. Changes to the current cell are
;; then distributed over the loop. These reductions are alternated until both
;; reach a fixed point.
;;
;; This stage introduces these symbols:
;;
;; 'loop-until-offset-cell-zero
;;
;; And eliminates no symbols.

(define (changes-current-cell? ir)
  (let loop ([remaining ir])
    (match remaining
           [((or ('look-cell-offset . _)
                 ('loop-until-current-cell-zero . _))
             . rem)
            #t]
           [(sym . rem)
            (loop rem)]
           [()
            #f])))

(define (convert-position-independent-loops ir)
  (let loop ([remaining ir])
    (match remaining 
           [(`(loop-until-current-cell-zero ,(body ...)) . rem)
            (let ([opti-body (convert-position-independent-loops body)])
              (if (changes-current-cell? opti-body)
                (cons `(loop-until-current-cell-zero ,opti-body)
                      (loop rem))
                (cons `(loop-until-offset-cell-zero ,opti-body 0)
                      (loop rem))))]
           [(sym . rem)
            (cons sym (loop rem))]
           [()
            '()])))

(define (change-offsets ir off)
  (match ir
         [`(loop-until-offset-cell-zero ,(body ...) ,offset)
           `(loop-until-offset-cell-zero
              ,(distribute-offset-changes body)
              ,(+ offset off))]
         [`(add-to-offset-cell ,offset ,value)
           `(add-to-offset-cell ,(+ offset off) ,value)]
         [`(read-char-into-offset-cell ,offset)
           `(read-char-into-offset-cell ,(+ offset off))]
         [`(write-char-from-offset-cell ,offset)
           `(write-char-from-offset-cell ,(+ offset off))]
         [x x]))

(define (distribute-offset-changes ir)
  (let loop ([remaining ir]
             [acc 0])
    (match remaining
           [(('look-cell-offset off)
             ('loop-until-offset-cell-zero . rst)
              . rem)
            (cons (change-offsets `(loop-until-offset-cell-zero ,@rst)
                                  (+ off acc))
                  (loop rem (+ off acc)))]
           [(`(look-cell-offset ,off) . rem)
            (if (zero? (+ off acc))
              (loop rem 0)
              (cons `(look-cell-offset ,(+ off acc))
                    (loop rem 0)))]
           [(ir-item . rem)
            (cons (change-offsets ir-item acc)
                  (loop rem 0))]
           [()
            (if (zero? acc)
              `()
              `((look-cell-offset ,acc)))])))

(define (run-optimisation-sequence ir opts)
  (fold (lambda (f x) (f x)) ir opts))

(define (run-to-fixed-point ir opts)
  (let loop ([old-ir ir])
    (let ([new-ir (run-optimisation-sequence old-ir opts)])
      (if (equal? new-ir old-ir)
        new-ir
        (loop new-ir)))))

(define (distribute-cell-changes-right ir)
  (run-to-fixed-point
    ir (list
         convert-position-independent-loops
         distribute-offset-changes)))

;; stage 5a: condense cell reads
;;
;; Collect sequences of reads into a single read
;;
;; This stage introduces these symbols:
;;
;; 'read-chars-into-consecutive-cells
;;
;; And eliminates these symbols:
;;
;; 'read-char-into-current-cell

;; stage 5b: condense cell writes
;;
;; Collect sequences of writes into a single write
;;
;; This stage introduces these symbols
;;
;; 'write-chars-from-consecutive-cells

;; stage ω: code generation
;;
;; Walks the code tree and generates list-structured assembly code.

(define (get-syscall-sysv call)
  (match call
         ['sys-exit 1]
         ['sys-read 3]
         ['sys-write 4]))

(define get-syscall-xnu32 get-syscall-sysv)

(define (get-syscall-xnu64 call)
  (+ #x2000000 (get-syscall-xnu32 call)))

(define (generate-code-sysv-ia32 get-syscall ir) ir)

(define (generate-loop-sysv-amd64 lblnum cell ir)
  `((cmp ,cell 0)
    (label ,(string-append "begin" lblnum))
    (je ,(string-append "end" lblnum))
    ,@ir
    (jmp ,(string-append "begin" lblnum))
    (label ,(string-append "end" lblnum))))

(define (generate-code-sysv-amd64 get-syscall ir)
  `((global "start")
    (section "text")
    (symbol "start")
    (mov (register rax) (symbol qword "arena"))
    ,@(let ([counter 0])
        (let loop ([remaining ir])
          (match remaining
                 [(`(loop-until-current-cell-zero ,(body ...))
                     . rem)
                   (set! counter (+ counter 1))
                   (let ([lbl (number->string counter)])
                     `(,@(generate-loop-sysv-amd64
                           lbl `(deref byte (register rax)) (loop body))
                        ,@(loop rem)))]
                 [(`(loop-until-offset-cell-zero ,(body ...) ,offset)
                    . rem)
                  (set! counter (+ counter 1))
                  (let ([lbl (number->string counter)])
                    `(,@(generate-loop-sysv-amd64
                          lbl `(deref byte (+ (register rax) ,offset))
                          (loop body))
                       ,@(loop rem)))]
                 [(`(add-to-offset-cell ,offset ,value)
                    . rem)
                  `((add (deref byte (+ (register rax) ,offset)) ,value)
                    ,@(loop rem))]
                 [(`(look-cell-offset ,delta)
                    . rem)
                  `((add (register rax) ,delta)
                    ,@(loop rem))]
                 [(`(write-char-from-offset-cell ,offset)
                    . rem)
                  `((mov (register rsi) (register rax))
                    (add (register rsi) ,offset)
                    (mov (register rax) ,(get-syscall 'sys-write))
                    (mov (register rdi) 1) ; stdout
                    (mov (register rdx) 1)
                    (syscall)
                    (mov (register rax) (register rsi))
                    (sub (register rax) ,offset)
                    ,@(loop rem))]
                 [(`(read-char-into-offset-cell ,offset)
                    . rem)
                  `((mov (register rsi) (register rax))
                    (add (register rsi) ,offset)
                    (mov (register rax) ,(get-syscall 'sys-read))
                    (mov (register rdi) 0) ; stdin
                    (mov (register rdx) 1)
                    (syscall)
                    (mov (register rax) (register rsi))
                    (sub (register rax) ,offset)
                    ,@(loop rem))]
                 [() '()])))
    (mov (register rax) ,(get-syscall 'sys-exit))
    (mov (register rdi) 0)
    (syscall)
    (section "data")
    (label "arena")
    (block 0 30000)))



;; stage ω+1: assembly output
;;
;; writes list-structured assembly code out to a file.

;; outputs code suitable for NASM or YASM.
(define (output-code-nasm asm)
  (for-each
    (lambda (line)
      (display
        (match line
               [`(global ,sym)
                 (format "global ~a" sym)]
               [`(section ,sec)
                 (format "\nsection .~a" sec)]
               [`(label ,lbl)
                 (format "~a:" lbl)]
               [`(symbol ,sym)
                 (format "~a:" sym)]
               [`(block ,byte ,size)
                 (format "  times ~a db ~a" size byte)]
               [`(,op ,addy)
                 (format "  ~a ~a" op (output-loc-nasm addy))]
               [`(,op ,dest ,src)
                 (format "  ~a ~a, ~a" op
                         (output-loc-nasm dest)
                         (output-loc-nasm src))]
               [`(,op)
                 (format "  ~a" op)]
               [x x]))
      (newline))
    asm))

(define (output-loc-nasm loc)
  (match loc
         [`(register ,reg)
           (format "~a" reg)]
         [`(deref ,size ,e)
           (format "~a [~a]" size (output-loc-nasm e))]
         [`(+ ,l ,r)
           (format "~a + ~a"
                   (output-loc-nasm l)
                   (output-loc-nasm r))]
         [(? number?)
          (format "~a" loc)]
         [((and size
               (or 'byte
                   'word
                   'dword
                   'qword)) value)
          (format "~a ~a" size value)]
         [`(symbol ,size ,sym)
           (format "~a ~a" size sym)]
         [x
           (format "~a" x)]))

(define (output-code-gas asm)
  (for-each
    (lambda (line)
      (display
        (match line
               [`(global ,sym)
                 (format ".globl ~a" sym)]
               [`(section ,sec)
                 (format ".~a" sec)]
               [`(label ,lbl)
                 (format "~a:" lbl)]
               [`(symbol ,sym)
                 (format "~a:" sym)]
               [`(block ,byte ,size)
                 (format ".fill ~a, 1, ~a" size byte)]
               [`(,op ,addy)
                 (format "  ~a ~a" op (output-loc-gas addy))]
               [`(,op ,dest ,src)
                 (format "  ~a~a ~a, ~a" op
                         (or (gas-size-suffix src)
                             (gas-size-suffix dest))
                         (output-loc-gas src)
                         (output-loc-gas dest))]
               [`(,op)
                 (format "  ~a" op)]))
      (newline))
    asm))

(define (output-loc-gas loc)
  (match loc
         [`(register ,reg)
           (format "%~a" reg)]
         [`(deref ,size (+ ,l ,r))
           (format "~a(~a)" r
                   (output-loc-gas l))]
         [`(deref ,size ,e)
           (format "(~a)" (output-loc-gas e))]
         [(? number?)
          (format "$~a" loc)]
         [`(,size ,value)
           (output-loc-gas value)]
         [`(symbol ,size ,sym)
          (format "~a@GOTPCREL(%rip)" sym)]
         [x
           (format "~a" x)]))

(define (gas-size-suffix loc)
  (match loc
         [`(register ,reg)
           (match (string->list (symbol->string reg))
                  [(#\r . _)
                   #\q]
                  [(#\e . _)
                   #\d]
                  [(_ #\x)
                   #\w]
                  [_
                    #\b])]
         [(or ('deref size . _)
              ('symbol size . _)
              (size _))
           (car (string->list (symbol->string size)))]
         [_
           #f]))

;; compilation

(define (select-code-generator)
  (lambda (ir)
    (generate-code-sysv-amd64 get-syscall-xnu64 ir)))

(define *output-code-proc* output-code-nasm)

(define (output-code ir)
  (*output-code-proc* ir))

(define option-help
  (option
    `(#\h "help") #f #f
    (lambda _
      (display "usage: labrea [-t platform] [-f format] [-o output] input")
      (newline)
      (display "  -h  --help    show this text") (newline)
      (newline)
      (display "  -f  --format  select output format:") (newline)
      (display "                  nasm (NASM/YASM syntax, default)") (newline)
      (display "                  yasm (same as nasm)") (newline)
      (display "                  intel (same as nasm)") (newline)
      (display "                  gas  (GNU Assembler syntax)") (newline)
      (newline)
      (display "  -t  --target  select target platform:") (newline)
      (display "                  xnu-amd64  (Mac OS X)") (newline)
      (display "                  sysv-ia32  (SysV IA-32)") (newline)
      (display "                  sysv-amd64 (SysV AMD64)") (newline)
      (display "                  sysv-arm6  (SysV ARMv6)") (newline)
      (newline)
      (display "  -o  --output  output to file") (newline)
      (newline)
      (exit))))

(define option-format
  (option
    `(#\f "format") #t #f
    (lambda (opt name arg seeds)
      (match arg
             [(or "nasm"
                  "yasm"
                  "intel")
              (set! *output-code-proc* output-code-nasm)]
             ["gas"
              (set! *output-code-proc* output-code-gas)]
             [_
               (error (format "unrecognized format ~a" arg))])
      seeds)))

(define option-target
  (option
    `(#\t "target") #t #f
    (lambda (opt name arg seeds)
      (match arg
             ["xnu-amd64"
              (set! select-code-generator
                (lambda ()
                  (lambda (ir)
                    (generate-code-sysv-amd64 get-syscall-xnu64 ir))))]
             ["sysv-amd64"
              (set! select-code-generator
                (lambda ()
                  (lambda (ir)
                    (generate-code-sysv-amd64 get-syscall-sysv ir))))]
             [_
               (error (format "unsupported target ~a" arg))])
      seeds)))

(define option-output
  (option
    `(#\o "output") #t #f
    (lambda (opt name arg seeds)
      (set! output-code
        (lambda (ir)
          (with-output-to-file
            arg
            (lambda ()
              (*output-code-proc* ir)))))
      seeds)))

(define (compile)
  (run-optimisation-sequence
    (read-source)
    (list
      collect-loops                     ; stage 1
      eliminate-empty-loops             ;   stage 1b
      condense-arithmetic               ; stage 2
      modulus-arithmetic                ;   stage 2b
      condense-memory-offsets           ; stage 3
      reduce-cell-pointer-changes       ; stage 4
      distribute-cell-changes-right     ;   stage 4b
      (select-code-generator)           ; stage ω
      ; pp)))
      output-code)))

(define ops
  (reverse
    (args-fold
      (command-line-arguments)
      (list
        option-help
        option-format
        option-target
        option-output)
      (lambda (opt name arg seeds)
        (error (format "unrecognised option ~a" opt)))
      cons '())))

(if (zero? (length ops))
  (error "no input files specified"))
(for-each
  (lambda (x)
    (with-input-from-file x compile))
  ops)
