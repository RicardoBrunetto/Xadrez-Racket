#lang racket

(provide (all-defined-out))

(require (lib "graphics.ss" "graphics"))
(define preto (make-rgb 0.4 0.4 0.4))
(define branco (make-rgb 0.255 0.255 0.255))
(define verde (make-rgb 0.4 0.14 0.25))

(struct peca (id tipo cor) #:transparent #:mutable)
;Estrutura de posição do tabuleiro
;Contém uma coordenada (x -> linha) e (y -> coluna)
;Contém uma peça do tipo struct peca
(struct pos (x y bkg [peca #:mutable]) #:transparent)

;Peças Brancas
(define pB0 (peca 0 "peao" "branco"))
(define pB1 (peca 1 "peao" "branco"))
(define pB2 (peca 2 "peao" "branco"))
(define pB3 (peca 3 "peao" "branco"))
(define pB4 (peca 4 "peao" "branco"))
(define pB5 (peca 5 "peao" "branco"))
(define pB6 (peca 6 "peao" "branco"))
(define pB7 (peca 7 "peao" "branco"))

(define tB0 (peca 8 "torre" "branco"))
(define tB1 (peca 9 "torre" "branco"))

(define cB0 (peca 10 "cavalo" "branco"))
(define cB1 (peca 11 "cavalo" "branco"))

(define bB0 (peca 12 "bispo" "branco"))
(define bB1 (peca 13 "bispo" "branco"))

(define qB0 (peca 14 "rainha" "branco"))
(define kB0 (peca 15 "rei" "branco"))

;Peças Pretas
(define pP0 (peca 16 "peao" "preto"))
(define pP1 (peca 17 "peao" "preto"))
(define pP2 (peca 18 "peao" "preto"))
(define pP3 (peca 19 "peao" "preto"))
(define pP4 (peca 20 "peao" "preto"))
(define pP5 (peca 21 "peao" "preto"))
(define pP6 (peca 22 "peao" "preto"))
(define pP7 (peca 23 "peao" "preto"))

(define tP0 (peca 24 "torre" "preto"))
(define tP1 (peca 25 "torre" "preto"))

(define cP0 (peca 26 "cavalo" "preto"))
(define cP1 (peca 27 "cavalo" "preto"))

(define bP0 (peca 28 "bispo" "preto"))
(define bP1 (peca 29 "bispo" "preto"))

(define qP0 (peca 30 "rainha" "preto"))
(define kP0 (peca 31 "rei" "preto"))

;Configuração preto do Tabuleiro
(define A8 (pos 0 0 branco tP0))    (define B8 (pos 0 1 preto cP0))     (define C8 (pos 0 2 branco bP0))    (define D8 (pos 0 3 preto qP0))     (define E8 (pos 0 4 branco kP0))    (define F8 (pos 0 5 preto bP1))     (define G8 (pos 0 6 branco cP1))    (define H8 (pos 0 7 preto tP1))
(define A7 (pos 1 0 preto pP0))     (define B7 (pos 1 1 branco pP1))    (define C7 (pos 1 2 preto pP2))     (define D7 (pos 1 3 branco pP3))    (define E7 (pos 1 4 preto pP4))     (define F7 (pos 1 5 branco pP5))    (define G7 (pos 1 6 preto pP6))     (define H7 (pos 1 7 branco pP7))
(define A6 (pos 2 0 branco empty))  (define B6 (pos 2 1 preto empty))   (define C6 (pos 2 2 branco empty))  (define D6 (pos 2 3 preto empty))   (define E6 (pos 2 4 branco empty))  (define F6 (pos 2 5 preto empty))   (define G6 (pos 2 6 branco empty))  (define H6 (pos 2 7 preto empty))
(define A5 (pos 3 0 preto empty))   (define B5 (pos 3 1 branco empty))  (define C5 (pos 3 2 preto empty))   (define D5 (pos 3 3 branco empty))  (define E5 (pos 3 4 preto empty))   (define F5 (pos 3 5 branco empty))  (define G5 (pos 3 6 preto empty))   (define H5 (pos 3 7 branco empty))
(define A4 (pos 4 0 branco empty))  (define B4 (pos 4 1 preto empty))   (define C4 (pos 4 2 branco empty))  (define D4 (pos 4 3 preto empty))   (define E4 (pos 4 4 branco empty))  (define F4 (pos 4 5 preto empty))   (define G4 (pos 4 6 branco empty))  (define H4 (pos 4 7 preto empty))
(define A3 (pos 5 0 preto empty))   (define B3 (pos 5 1 branco empty))  (define C3 (pos 5 2 preto empty))   (define D3 (pos 5 3 branco empty))  (define E3 (pos 5 4 preto empty))   (define F3 (pos 5 5 branco empty))  (define G3 (pos 5 6 preto empty))   (define H3 (pos 5 7 branco empty))
(define A2 (pos 6 0 branco pB0))    (define B2 (pos 6 1 preto pB1))     (define C2 (pos 6 2 branco pB2))    (define D2 (pos 6 3 preto pB3))     (define E2 (pos 6 4 branco pB4))    (define F2 (pos 6 5 preto pB5))     (define G2 (pos 6 6 branco pB6))    (define H2 (pos 6 7 preto pB7))
(define A1 (pos 7 0 preto tB0))     (define B1 (pos 7 1 branco cB0))    (define C1 (pos 7 2 preto bB0))     (define D1 (pos 7 3 branco qB0))    (define E1 (pos 7 4 preto kB0))     (define F1 (pos 7 5 branco bB1))    (define G1 (pos 7 6 preto cB1))     (define H1 (pos 7 7 branco tB1))
