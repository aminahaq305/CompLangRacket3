#lang racket
;Amina Haq | Project 3 | Playing Games with Racket

;Define faces and suits
;-----------------------------
(define faces '(2 3 4 5 6 7 8 9 10 J Q K A)) ;define the list of faces for cards in deck
(define suits '(♣ ♦ ♥ ♠)) ;define the list of suits for cards in deck


;Make the deck
;-----------------------------
;create the deck by creating a list of dotted pairs for each face and suit combination
(define (make-deck)
  (for*/list ([x faces]
              [y suits])
    (cons x y)))

;Deal cards to player and dealer
;--------------------------------
;take the top two cards and add them to the hand
(define deal!
  (lambda (deck)
    (set! thedeck (cddr thedeck)) ;reset the deck to not have the top two cards
    (cons (car deck) (cons (cadr deck) '())))) ;return a list containing the top two cards in the deck

;Show a hand
;-----------------------------
(define show-hand
  (lambda (hand how disc) ;hand is the hand to be displayed, how is "Full" or "Part" and description defines whose hand is being displayed
    (cond ((equal? 'Part how) (printf "~a ~s" disc (cons '***** (cdr hand)))) ;if it is part, do not display the first card
          ((equal? 'Full how) (printf "~a ~s" disc hand)) ;if it is full display the entire hand
          (else (printf "error"))))) ;if any other how is entered, display an error

;Randomly shuffle the deck
;-----------------------------
(define shuffledeck
  (lambda (list)
    (letrec ((shuffle
              (lambda (in out n)
                (if (= n 0) (cons (car in) (shuffledeck (append (cdr in) out))) ;keep calling suffledeck until the list is empty
                    (shuffle (cdr in) (cons (car in) out) (- n 1)))))) ;shuffle the cards in groups
      (if (null? list) ;if list is empty return an empty list
          '()
          ;if list if not empty call the shuffle function with the deck, an empty list,
          ;and a random number between 0 to 52
          (shuffle list '() (random (length list)))))))

;Evaluate the best possible score for a hand
;--------------------------------------------
(define eval-hand (lambda (hand)
                    (letrec(( numOfAce (lambda (deck) ;calculates he number of aces in a hand
                                         (letrec(( loop (lambda (list result)
                                                          (cond ((null? list) result)
                                                                ((equal? (caar list) 'A) (loop (cdr list) (+ 1 result)))
                                                                (else (loop (cdr list) result)))))) ;return the number of aces in a hand
                                           (loop deck 0)))))
                      (let ((AceNum (numOfAce hand)) ;intially number of Aces are calculated using numOfAce
                            (score 0) ;initial score is zero
                            (otherScore 0)) ;initial alternate score is zero
                        (letrec ((checkBest
                                  (lambda ();if the score with Ace as value 11 results in total score higher than 21,
                                  ;return otherScore with Ace as value 1
                                  (if (>= 21 score) score
                                      otherScore))))
                          ;the adjustScore function accounts for the number of Aces and procudes the best possible score
                          (letrec ((adjustScore (lambda ()
                                                  (if (>= AceNum 1)
                                                      (cond ((= AceNum 1) (begin (set! score (+ 11 score))
                                                                                 ;score accounts for Ace as having value 11
                                                                                 (set! otherScore (+ 1 otherScore))
                                                                                 ;otherScore accounts for Ace as having value 1
                                                                                 (checkBest)))
                                                            ;checkBest checks which score is in favor of the player
                                                            ((= AceNum 2) (begin (set! score (+ 12 score))
                                                                                 (set! otherScore (+ 2 otherScore))
                                                                                 (checkBest)))
                                                            ((= AceNum 3) (begin (set! score (+ 13 score))
                                                                                 (set! otherScore (+ 3 otherScore))
                                                                                 (checkBest)))
                                                            ((= AceNum 4) (begin (set! score (+ 14 score))
                                                                                 (set! otherScore (+ 4 otherScore))
                                                                                 (checkBest)))
                                                            ;if the number of aces in a hand are greater than 4 return an error
                                                            (else "Error: more than 4 aces in deck!"))
                                                      score))))
                            ;the checkFaces function checks if the face is A, KQJ, or numbered
                            (letrec ((checkFaces
                                      (lambda (cards)
                                        (cond ((null? cards) (adjustScore)) ;when all cards are checked, call the adjustScore function
                                              ((eq? (caar cards) 'A) (checkFaces (cdr cards)))   
                                              ((or (eq? (caar cards) 'K)
                                                   (eq? (caar cards) 'Q)
                                                   (eq? (caar cards) 'J)) (begin (set! score (+ score 10))
                                                                                 ;if it is KQJ it sets score to 10, and otherScore
                                                                                 ;(alternate score) to 10 aswell
                                                                                 (set! otherScore (+ otherScore 10))
                                                                                 (checkFaces (cdr cards))))
                                              ;if it is numbered it sets score to its value, and otherScore
                                              ;(alternate score) to its value aswell
                                              (else (begin (set! score (+ (caar cards) score))
                                                           (set! otherScore (+ (caar cards) otherScore))
                                                           (checkFaces (cdr cards))))))))
                              (checkFaces hand))))))))

;Define hit!
;------------
(define-syntax-rule (hit! x y)
  (let ([tmp (car x)]) ;store the top card in the deck in tmp variable
    (set! y (cons tmp y)) ;change the hand to contain the top card in the deck
    (set! x (cdr x)))) ;change the deck to not contain the top card

;Player turn function
;---------------------
(define (playerturn)
  (newline)
  (display "Hit or stand? (enter hit or stand)")
  (newline)
  (let ((answer (read)))
    (cond ((eq? answer 'hit) 
           (begin (hit! thedeck playerhand)
                  (show-hand playerhand 'Full "You have: ")
                  (cond ((> (eval-hand playerhand) 21) (begin (newline) (set! playerquota 0) (printf "Bust: you lose! Your total winnings: ~a" playerquota) ))
                        ((< (eval-hand playerhand) 21) (begin (newline)
                                                              (playerturn)))
                        (else (playerturn)))))
          ((eq? answer 'stand)
           (cond ((and (equal? (eval-hand playerhand) 21) (= 2 (length playerhand)))
                  (begin 
                         (set! playerquota (+ playerquota (* playerquota 1.5)))
                         (printf "Blackjack: you won! Your total winnings: ~a" playerquota)))
                 ((= (eval-hand playerhand) 21) (dealerturn))
                 (else (dealerturn))))
          (else "fail"))))

;Dealer turn function
;---------------------
(define (dealerturn)
  (newline)
  (show-hand dealerhand 'Full "The dealer has: ")
  (newline)
  (cond ((> (eval-hand dealerhand) 21)
         (begin (set! playerquota (* playerquota 2))
                (printf "Dealer Busts: you win! Your total winnings: ~a" playerquota)))
        ((and (= 21 (eval-hand dealerhand)) (= 21 (eval-hand playerhand))) "Push!")
        ((or (= 21 (eval-hand dealerhand)) 
             (> (eval-hand dealerhand) (eval-hand playerhand))) "Dealer won: you lose.")
        (else (< (eval-hand dealerhand) (eval-hand playerhand)) (begin (hit! thedeck dealerhand)
                                                                       (display "Dealer Hits")
                                                                       (dealerturn)))))

;Main game play code
;---------------------
(define thedeck (make-deck))
(set! thedeck (shuffledeck thedeck))
(define playerhand (deal! thedeck))
(define dealerhand (deal! thedeck))
(display "Let's play blackjack!")
(newline)
(show-hand dealerhand 'Part "The dealer has: ")
(newline)
(display "Enter your bet: ")
(define playerquota (read))
(newline)
(show-hand playerhand 'Full "You have: ")
(newline)
(playerturn)
