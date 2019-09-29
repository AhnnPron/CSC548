;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |HW #3.5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define myProgram ; array/list of instructions
  '(
    (addi t1 zero 1)
    (sw t1 8192)
    (lw t2 8196) ; value 1 is stored in t2
    (sw t1 8192)
    (lw t3 8196) ; value 2 is stored in t3

    (slt t4 t2 t3) ; t4 will be set to a 1 if t2 is smaller than t3, otherwise t4 will hold a zero
    (beq t4 zero T3Smaller) ; if t4 is equal to zero, then t3 is the smaller value
    (add t1 t2 1) ; store into t1 the value of t2
    (add t2 t3 zero) ; store into t2 the value of t3
    (j Loop)
    (add t1 t3 1) 
    (T3Smaller)
    (Loop)
    (beq t1 t2 Done)
    (sw t1 8200)
    (addi t1 t1 1)
    (j Loop)
    (Done)
  ))
(mypzyCPU myProgram)

(define mipzyFindLabel
  (lambda (prog label pos)
    (if (eq? (caar prog) label) ; if the label is the one we're looking for
        (set! programCounter pos) ; update our position 
        (mipzyFindLabel (cdr prog) label (+ pos 1))))) ; check the next bucket for the label

(define programCounter 0) ; keeps track of the next line in the program

(define mipzyGetInstruction
  (lambda (prog)
    (list-ref prog programCounter))) ;list-ref is a scheme function

(define mipzyCPU
  (lambda (prog)
    (if (= (length prog) programCounter) ;have we run out of instructions to execute?
        (list register-file data-memory)
        (begin ; <-- block statement
          (executeInstruction (mipzyGetInstruction prog)) ;execute this instruction
          (mipzyCPU prog)))))


(define executeInstruction
  (lambda (instruction)
    (cond
      ((eq? (car instruction) 'lw)
       (lw (cadr instruction) (caddr instruction))) ; (lw t1 5004)
      ;
      ((eq? (car instruction) 'sw)
       (if (number? (caddr instruction))
           (sw (cadr instruction) (caddr instruction))
           (sw (cadr instruction) (getRegValue (caddr instruction)))))
      ;
      ((eq? (car instruction) 'add)
       (add (cadr instruction) (caddr instruction) (cadddr instruction)))
      ;
      ((eq? (car instruction) 'addi)
       (addi (cadr instruction) (caddr instruction) (cadddr instruction)))
      ;
      ((eq? (car instruction) 'sub)
       (sub (cadr instruction) (caddr instruction) (cadddr instruction)))
      ;
      ((eq? (car instruction) 'mul)
       (mul (cadr instruction) (caddr instruction) (cadddr instruction)))
      ;
      ((eq? (car instruction) 'beq)
       (if (= (getRegValue (cadr instruction)) ; if the value of the register at the cadr is equal to the value at the register at the caddr
              (getRegValue (caddr instruction))) ; (beq t1, t2, label) ; t1 = t2
           (mipzyFindLabel myProgram (cadddr instruction) 0) ;find the location of the label in the code and update the program counter to 0
           (set! programCounter (+ programCounter 1))))
      ;
      ((eq? (car instruction) 'bne)
       (if (not(= (getRegValue (cadr instruction)) ; if the value of the register at the cadr is NOT equal to the value at the register at the caddr
              (getRegValue (caddr instruction)))) ; (bne t1, t2, label) ; t1 != t2
           (mipzyFindLabel myProgram (cadddr instruction) 0) ;find the label, update the program counter
           (set! programCounter (+ programCounter 1))))
      ;
      ((eq? (car instruction) 'slt)
       (if (< (getRegValue (caddr instruction)) ; if $b is less than $c
              (getRegValue (cadddr instruction)))
           (setRegValue (cadr instruction) 1) ; set $a's value at 1
           (setRegValue (cadr instruction) 0))) ; else set $a's value at 0
      ;
      ((eq? (car instruction) 'jal)
       (begin
         (setRegValue 'ra (+ programCounter 1))
       (mipzyFindLabel myProgram (cadr instruction) 0)))
      ;
      ((eq? (car instruction) 'j)
       (mipzyFindLabel myProgram (cadr instruction) 0)) ; find the location of the label in the code and update the program counter to 0
      ;
      ((eq? (car instruction) 'jr)
       (set! programCounter (getRegValue (cadr instruction))))
      ;
      (else ; we're looking at a label
       (if (= (length (cdr instruction)) 0) ; if the stuff after the label is non-existant 
           (set! programCounter (+ programCounter 1)) ; update the program counter so that it operates on the next line
           (executeInstruction (cdr instruction))))))) ; 
                     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;HW #2.5 Helpers
(define getRegValueHelper ; (getRegisterValue 't2) --> 0
  (lambda (regName register-file)
    (if (null? register-file)
        #f
        (if (eq? (caar register-file) regName)
        (cadar register-file)
        (getRegValueHelper regName (cdr register-file))))))

;allows us to get the value of a register
(define getRegValue
  (lambda (regName)
    (getRegValueHelper regName register-file)))

(define get2ListValHelper ; (getRegisterValue 't2) --> 0
  (lambda (name file)
    (if (null? file)
        #f
        (if (eq? (caar file) name)
        (cadar register-file)
        (get2ListValHelper name (cdr file))))))

;allows us to get the value of a register
(define get2ListValue
  (lambda (name)
    (get2ListValHelper name register-file)))

(define setRegValueHelper
  (lambda (regName newVal regFile)
    (if (null? regFile)
        '()
        (if (eq? (caar regFile) regName)
            (append (list (list regName newVal)) ;create a new 2-list with the register name and the new value and append it to the cdr of the old list
                  (cdr regFile))
            (cons (car regFile) (setRegValueHelper regName newVal (cdr regFile))))))) ; we need to use cons because we're using recursion

(define set2ListValHelper
  (lambda (name newVal file)
    (if (null? file)
        '()
        (if (eq? (caar file) name)
            (cadr (list
              (set! programCounter (+ programCounter 1))
              (append (list (list name newVal)) ;create a new 2-list with the register name and the new value and append it to the cdr of the old list
                      (cdr file))))
            (cons (car file) (set2ListValHelper name newVal (cdr file))))))) ; we need to use cons because we're using recursion
 
(define setRegValue
  (lambda (regName newVal)
    (set! register-file(setRegValueHelper regName newVal register-file))))

(define getDmValue
  (lambda (memLocation)
    (get2ListValHelper memLocation data-memory))) 

(define setDmVal
  (lambda (memAddress newVal)
    (begin
      (if (= memAddress 8200)
         (map display (list newVal "\n"))
          #f)
          (set! data-memory(set2ListValHelper memAddress newVal data-memory)))))

;Load
(define lw
  (lambda (regName memLocation)
    (setRegValue regName (getDmValue memLocation))))

;Store
(define sw
  (lambda (regName memLocation)
    (if (and (= memLocation 8192)(= (getRegValue regName) 1))
        (setDmVal 8196 (read))
        (setDmVal memLocation (getRegValue regName)))))

;addi
(define addi
  (lambda (destinationRegName sourceRegName immediate)
    (setRegValue destinationRegName (+ (getRegValue sourceRegName) immediate))))
;(addi 't2 't1 15)

;add
(define add
  (lambda (destinationRegName sourceRegName1 sourceRegName2)
    (setRegValue destinationRegName (+ (getRegValue sourceRegName1) (getRegValue sourceRegName2)))))
; (add 't3 't1 't2)

;subtract
(define sub
  (lambda (destinationRegName sourceRegName1 sourceRegName2)
    (setRegValue destinationRegName (- (getRegValue sourceRegName1) (getRegValue sourceRegName2)))))

;multiply
(define mul
  (lambda (destinationRegName sourceRegName1 sourceRegName2)
    (setRegValue destinationRegName (* (getRegValue sourceRegName1) (getRegValue sourceRegName2)))))

(define register-file
  '((zero 0)
    (t0 0)
    (t1 0)
    (t2 0)
    (t3 0)
    (t4 0)
    (t5 60)
    (t6 0)
    (ra 0)
    ))

(define data-memory
  '((5000 0)
    (5004 0)
    (5008 0)
    (5012 0)
    (5016 0)
    (5020 0)
    (5024 0)
    (8192 0)
    (8196 0)
    ))