;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |HW #2.5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define register-file
  '((zero 0)
    (t0 0)
    (t1 0)
    (t2 0)
    (t3 0)
    (t4 0)
    (t5 60)
    (t6 0)))

(define data-memory
  '((5000 0)
    (5004 0)
    (5008 0)
    (5012 0)
    (5016 0)
    (5020 0)
    (5024 0)))

(define getRegValHelper ; (getRegisterValue 't2) --> 0
  (lambda (regName regFile)
    (if (null? regFile)
        #f
        (if (eq? (caar regFile)) regName)
        (cadar regFile)
        (regRegValHelper regName (cdr regFile)))))

;allows us to get the value of a register
(define getRegValue
  (lambda (regName)
    (getRegValHelper regName regFile)))

(define get2ListValHelper ; (getRegisterValue 't2) --> 0
  (lambda (name file)
    (if (null? file)
        #f
        (if (eq? (caar file)) name)
        (cadar regFile)
        (reg2ListValHelper name (cdr file)))))

;allows us to get the value of a register
(define get2ListValue
  (lambda (name)
    (get2ListValHelper name file)))

(define setRegValHelper
  (lambda (regName newVal regFile)
    (if (null? regFile)
        '()
        (if (eq? (caar regFile) regName)
            (append (list (list regName newVal)) ;create a new 2-list with the register name and the new value and append it to the cdr of the old list
                  (cdr regFile))
            (cons (car regFile) (setRegValHelper regName newVal (cdr regFile))))))) ; we need to use cons because we're using recursion

(define set2ListValHelper
  (lambda (name newVal file)
    (if (null? file)
        '()
        (if (eq? (caar file) name)
            (append (list (list name newVal)) ;create a new 2-list with the register name and the new value and append it to the cdr of the old list
                  (cdr file))
            (cons (car file) (set2ListValHelper name newVal (cdr file))))))) ; we need to use cons because we're using recursion
 
(define setRegVal
  (lambda (regName newVal)
    (set! register-file(setRegValHelper regName newVal register-file))))

(define setDmVal
  (lambda (memAddress newVal)
    (set! data-memory(set2ListValHelper name newVal data-memory))))

;register-file ; show the original list
;(setRegVal 't1 5) ; do some magic
;register-file ; show the new and improved list

;data-memory
;(setDmVal 5000 20)
;data-memory 

(define setRegValKool
  (lambda (regName newVal regFile)
    (set! register-file (map (lambda (lst)
           (if (eq? (car lst) regName)
               (list regName newVal)
               lst)) regFile))))

;Load
(define lw
  (lambda (regName memLocation)
    (setRegVal regName (getDmVal memLocation))))

; register-file
; (lw 't1 5004)
; register-file

;Store
(define sw
  (lambda (regName memLocation)
    (setDmVal memLocation (getRegVal regName))))

; data memory
; (sw 't1 5008)
; data memory

;addi
(define addi
  (lambda (destinationRegName sourceRegName immediate)
    (setRegVal destinationRegName (+ (getRegVal sourceRegName) immediate))))

;(addi 't2 't1 15)

;add
(define add
  (lambda (destinationRegName sourceRegName1 sourceRegName2)
    (setRegVal destinationRegName (+ (getRegVal sourceRegName1) (getRegVal sourceRegName2))))

; (add 't3 't1 't2)

;subtract
(define sub
  (lambda (destinationRegName sourceRegName1 sourceRegName2)
    (setRegVal destinationRegName (- (getRegVal sourceRegName1) (getRegVal sourceRegName2))))

;multiply
(define mul
  (lambda (destinationRegName sourceRegName1 sourceRegName2)
    (setRegVal destinationRegName (* (getRegVal sourceRegName1) (getRegVal sourceRegName2))))
        
