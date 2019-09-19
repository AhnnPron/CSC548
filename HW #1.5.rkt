;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |HW #1.5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;function that converts decimal to binary
(define decimalToBinaryHelper
  (lambda (n)
    (if (= n 0)
        '()
        (cons (modulo n 2)
              (decimalToBinaryHelper (quotient n 2))))))
(define decimalToBinary
  (lambda (n)
    (reverse (decimalToBinaryHelper n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define myReverse ;home made reverse function
  (lambda (lst)
    (if (null? lst)
        '()
        (append (myReverse (cdr lst)) (list (car lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;function that converts binary to decimal
(define binaryToDecimalHelper 
  (lambda (binNum placeVal)
    (if (null? binNum)
        0
        (+ (* (car binNum) placeVal)
           (binaryToDecimalHelper (cdr binNum) (* placeVal 2))))))

(define binaryToDecimal
  (lambda (binNum)
    (binaryToDecimalHelper (myReverse binNum) 1)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;function that converts decimal to hex and vise versa

(define indexOfHelper
  (lambda (lst searchChar currPos)
    (if (null? lst)
        -1
        (if (eq? (car lst) searchChar)
            currPos
            (indexOfHelper (cdr lst) searchChar (+ currPos 1))))))

(define indexOf
  (lambda (lst searchChar)
    (indexOfHelper lst searchChar 0)))

(define charAt
  (lambda (lst pos)
    (if (= pos 0)
        (car lst)
        (charAt (cdr lst) (- pos 1)))))

(define mapHex2Dec
  (lambda (hextet)
    (indexOf '(0 1 2 3 4 5 6 7 8 9 A B C D E F) hextet)))

(define mapDec2Hex
  (lambda (digit)
    (charAt '(0 1 2 3 4 5 6 7 8 9 A B C D E F) digit)))

(define hexToDecimalHelper 
  (lambda (hex placeVal)
    (if (null? hex)
        0
        (+ (* (car hex) placeVal)
           (hexToDecimalHelper (cdr hex) (* placeVal 2))))))

(define hexToDecimal
  (lambda (hex)
    (hexToDecimalHelper (myReverse hex) 1)))

(define decimalToHexHelper 
  (lambda (n)
    (if (= n 0)
        '()
        (cons(mapDec2Hex (modulo n 16))
             (decimalToHexHelper (quotient n 16))))))
     
(define decimalToHex
  (lambda (n)
    (myReverse (decimalToHexHelper n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;twos complement

(define flipBitsCoolKidz
  (lambda (binNum)
    (map (lambda (x) (if (= x 0) 1 0)) binNum))) ;map says, apply this new function to every value of binNum

(define flipBits
  (lambda (binNum)
    (if (null? binNum)
        '()
        (if (= (car binNum) 1) ;essentially doing the same thing as map
             (cons 0 (flipBits (cdr binNum)))
             (cons 1 (flipBits (cdr binNum)))))))

(define twosComp
  (lambda (binNum)
    (decimalToBinary
     (+ (binaryToDecimal (flipBits binNum)) 1))))

(define twosComplementHelper
  (lambda (binNum carry adder)
    (if (null? binNum)
        '()
        (cond
          ((= (+ (+ (car binNum) carry) adder) 3)
           (cons 1 (twosComplementHelper (cdr binNum) 1 0)))
          ((= (+ (+ (car binNum) carry) adder) 2)
           (cons 0 (twosComplementHelper (cdr binNum) 1 0)))
          ((= (+ (+ (car binNum) carry) adder) 1)
           (cons 1 (twosComplementHelper (cdr binNum) 0 0)))
          (else (cons 0 (twosComplementHelper (cdr binNum) 0 0)))))))

(define twosComplement
  (lambda (binNum)
    (myReverse (twosComplementHelper
                (myReverse (flipBits binNum)) 0 1))))

                                                  ;;        
                
               
