;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname click1.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; clicker game with tick, mouse, and draw
;;  - upgrade ui
;;  - upgrades

;; DIFFICULTY CONSTANTS 
(define BASE-COST 10) ; lower for easier/less time (recommended)
(define AMOUNT-OF-UPGRADES 20) ; minus 2 for click, minus 1 for auto due to build-list 
(define AUTOTICK 7)  ; adjusts the tick multiplier, higher for easier 
(define WIN 1000)    ; higher for harder/more time 


;; UPGRADE-STRUCT
;; An Upgrade is a Struct (make-upgrade (Number Number)
(define-struct upgrade (level cost))
(define uclick (make-upgrade 2 100))
(define uauto (make-upgrade 1 200))

;; MENU-STRUCT
;; A Menu is a Struct (make-menu [Listof Upgrade] [Listof Upgrade])
(define-struct menu (click auto))

;; WORLD-STRUCT
;; see below, line 66

;; A [Listof Upgrade] is one of:
;; - empty
;; (cons Upgrade [Listof Upgrade])

;; LIST-OF-UPGRADE
;; lou* : Number Number [Listof Upgrade]
;; (lou* x start-level lou-option) : abstraction
(define (lou* x start-level lou-option)
  (filter (lambda (x)
            (> (upgrade-level x) start-level))(lou-option x)))

;; lou-click-draft : Number -> [Listof Upgrade]
;; (lou-click-draft x) : uses build-list to construct a [Listof Upgrade]
(define (lou-click-draft x)
  (build-list AMOUNT-OF-UPGRADES
              (lambda (x)(make-upgrade x (* x x BASE-COST)))))

;; lou-click : Number -> [Listof Upgrade]
;; (lou-click x) : uses filter and build-list to construct a [Listof Upgrade]
(define (lou-click x)
  (lou* x 1 lou-click-draft))

;; lou-auto-draft : Number -> [Listof Upgrade]
;; (lou-auto-draf x) : see above
(define (lou-auto-draft x)
  (build-list AMOUNT-OF-UPGRADES
              (lambda (x)(make-upgrade x (* x BASE-COST)))))

;; lou-auto : Number -> [Listof Upgrade]
;; (lou-auto x) : see above
(define (lou-auto x)
  (lou* x 0 lou-auto-draft))

(define lou-click1 (lou-click AMOUNT-OF-UPGRADES))
(define lou-auto1 (lou-auto AMOUNT-OF-UPGRADES))

;; WORLD-STRUCT
;; A World is a struct (make-world Number Number Number Menu)
(define-struct world (click auto bank menu))
(define world1 (make-world 1 0 0 (make-menu lou-click1 lou-auto1)))


;; UPGRADES
;; purchase-upgrade : World -> World
;; (purchase-upgrade w) : if the bank has enough for the cost of an upgrade, it will upgrade the level
;; , deduct the cost from the bank, and remove the upgrade from the list
(define (purchase-click-upgrade w)
  (cond [(empty? (menu-click (world-menu w))) w]
        [(>= (world-bank w)(upgrade-cost (first (menu-click (world-menu w))))) 
         (make-world (upgrade-level (first (menu-click (world-menu w))))
                     (world-auto w)
                     (- (world-bank w) (upgrade-cost (first (menu-click (world-menu w)))))
                     (make-menu (rest (menu-click (world-menu w)))
                                (menu-auto (world-menu w))))]
         [else w]))

(define check-purchase1 (make-world 1 0 30 (make-menu (list (make-upgrade 2 30)) (list))))
(check-expect (purchase-click-upgrade check-purchase1) (make-world 2 0 0 (make-menu empty empty)))

;; purchase-auto-upgrade : World -> World
;; (purchase-auto-upgrade w) : see above
(define (purchase-auto-upgrade w)
  (cond [(empty? (menu-auto (world-menu w))) w]
        [(>= (world-bank w)(upgrade-cost (first (menu-auto (world-menu w)))))
         (make-world (world-click w)
                     (upgrade-level (first (menu-auto (world-menu w))))
                     (- (world-bank w)(upgrade-cost (first (menu-auto (world-menu w)))))
                     (make-menu (menu-click (world-menu w))
                                (rest (menu-auto (world-menu w)))))]
        [else w]))

(check-expect (purchase-auto-upgrade (make-world 1 0 30 (make-menu empty (list (make-upgrade 1 30)))))
              (make-world 1 1 0 (make-menu empty empty)))

;; ON-MOUSE
;; upgrade-click : World Number Number MouseEvent -> World
;; (upgrade-click w x y me) : if the mouse is within posn range of click button and mouse event button is
;; "button-down", this will relay to purchase-click-upgrade
(define (upgrade-click w x y me)
  (if (and (and (>= x (+ WIDTH QWIDTH))(<= x (- (+ WIDTH WIDTH) QWIDTH)))
           (and (>= y HHEIGHT)(<= y (+ HHEIGHT UBOXH)))
           (string=? me "button-down"))
      (purchase-click-upgrade w)
      w))

(check-expect (upgrade-click (make-world 1 0 30 (make-menu (list (make-upgrade 2 30)) empty)) 330 (- (+ UBOXH HHEIGHT) 20) "button-down")
              (make-world 2 0 0 (make-menu empty empty)))

;; upgrade-auto : World Number Number MouseEvent -> World
;; (upgrade-auto w x y me) : see above
(define (upgrade-auto w x y me)
  (if (and (and (>= x (+ WIDTH QWIDTH))(<= x (- (+ WIDTH WIDTH) QWIDTH)))
           (and (>= y (+ HHEIGHT UBOXH))(<= y (+ HHEIGHT UBOXH UBOXH)))
           (string=? me "button-down"))
      (purchase-auto-upgrade w)
      w))

(check-expect (upgrade-auto (make-world 1 0 30 (make-menu empty (list (make-upgrade 1 30)))) 330 (- (+ UBOXH UBOXH HHEIGHT) 20) "button-down")
              (make-world 1 1 0 (make-menu empty empty)))

;; upgrade-click-auto-or-bank : World Number Number MouseEvent -> World
;; (upgrade-click-or-auto-or-bank w x y me)
(define (upgrade-click-or-auto-or-bank w x y me)
  (cond [(and (and (>= x (+ WIDTH QWIDTH))(<= x (- (+ WIDTH WIDTH) QWIDTH)))
              (and (>= y (- HHEIGHT UBOXH))(<= y HHEIGHT))
              (string=? me "button-down"))
         (purchase-click-upgrade w)]
        [(and (and (>= x (+ WIDTH QWIDTH))(<= x (- (+ WIDTH WIDTH) QWIDTH)))
           (and (>= y HHEIGHT)(<= y (+ HHEIGHT UBOXH)))
           (string=? me "button-down"))
         (purchase-auto-upgrade w)]
        [(string=? me "button-down")
         (make-world (world-click w)(world-auto w)(+ (world-click w)(world-bank w))(world-menu w))]
        [else w]))

;; DIMENSIONS
(define WIDTH 250)
(define HEIGHT 250)
(define UFONT 30)
(define UBOXH 50)
(define HWIDTH (/ WIDTH 2))
(define QWIDTH (/ WIDTH 4))
(define HHEIGHT (/ HEIGHT 2))

;; UI
(define UI (overlay/xy (rectangle WIDTH HEIGHT "outline" "black")
                            WIDTH 0
                            (place-images (list (above (overlay (text "click" UFONT "purple")
                                                                (rectangle HWIDTH UBOXH "solid" "green"))
                                                       (overlay (text "auto" UFONT "green")
                                                                (rectangle HWIDTH UBOXH "solid" "purple")))
                                                (text "upgrades" UFONT "blue"))
                                          (list (make-posn HWIDTH HHEIGHT)
                                                (make-posn HWIDTH 40))
                                          (rectangle WIDTH HEIGHT "outline" "black"))))

;; to-draw : World -> Image
;; (to-draw w) : renders world
(define (draw w)
  (place-image (above (text "bank" 18 "goldenrod")
                      (if (>= (world-bank w) WIN)
                          (text "YOU WON!" 40 "RED")
                          (text (number->string (world-bank w)) 12 "black")))
               HWIDTH
               (/ HEIGHT 3)
               (place-image (above (text (string-append "c: " (number->string (world-click w))) 12 "purple")
                                   (text (string-append "a: " (number->string (world-auto w))) 12 "green"))
                            HWIDTH
                            (/ HEIGHT 1.4)                 
                            UI)))

;; tock : World -> World
;; (tock w) : adds world-auto level to bank each second
(define (tock w)
  (make-world (world-click w)(world-auto w)(+ (* AUTOTICK (world-auto w))(world-bank w))(world-menu w)))

;; click : foundational "button-down" mouse event, now unused
#;(define (click w x y me)
  (if (string=? "button-down" me)
      (make-world (world-click w)(world-auto w)(+ (world-click w)(world-bank w))(world-menu w))
      w))

;; you-win : World -> ???
;; (you-win w) : if the world-bank is at WIN amount, this terminates the game
(define (you-win w)
  (> (world-bank w) WIN))

(big-bang world1
          [on-tick tock 1]
          [to-draw draw]
          [on-mouse upgrade-click-or-auto-or-bank]
          [stop-when you-win])

