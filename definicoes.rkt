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
(struct pos (x y destinavel [peca #:mutable]) #:transparent)

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
(define kB0 (peca 15 "rei"    "branco"))

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
(define kP0 (peca 31 "rei"    "preto"))

;Configuração do Tabuleiro
(define A8 (pos 0 0 #f tP0))    (define B8 (pos 0 1 #f cP0))    (define C8 (pos 0 2 #f bP0))    (define D8 (pos 0 3 #f qP0))    (define E8 (pos 0 4 #f kP0))    (define F8 (pos 0 5 #f bP1))    (define G8 (pos 0 6 #f cP1))    (define H8 (pos 0 7 #f tP1))
(define A7 (pos 1 0 #f pP0))    (define B7 (pos 1 1 #f pP1))    (define C7 (pos 1 2 #f pP2))    (define D7 (pos 1 3 #f pP3))    (define E7 (pos 1 4 #f pP4))    (define F7 (pos 1 5 #f pP5))    (define G7 (pos 1 6 #f pP6))    (define H7 (pos 1 7 #f pP7))
(define A6 (pos 2 0 #f empty))  (define B6 (pos 2 1 #f empty))  (define C6 (pos 2 2 #f empty))  (define D6 (pos 2 3 #f empty))  (define E6 (pos 2 4 #f empty))  (define F6 (pos 2 5 #f empty))  (define G6 (pos 2 6 #f empty))  (define H6 (pos 2 7 #f empty))
(define A5 (pos 3 0 #f empty))  (define B5 (pos 3 1 #f empty))  (define C5 (pos 3 2 #f empty))  (define D5 (pos 3 3 #f empty))  (define E5 (pos 3 4 #f empty))  (define F5 (pos 3 5 #f empty))  (define G5 (pos 3 6 #f empty))  (define H5 (pos 3 7 #f empty))
(define A4 (pos 4 0 #f empty))  (define B4 (pos 4 1 #f empty))  (define C4 (pos 4 2 #f empty))  (define D4 (pos 4 3 #f empty))  (define E4 (pos 4 4 #f empty))  (define F4 (pos 4 5 #f empty))  (define G4 (pos 4 6 #f empty))  (define H4 (pos 4 7 #f empty))
(define A3 (pos 5 0 #f empty))  (define B3 (pos 5 1 #f empty))  (define C3 (pos 5 2 #f empty))  (define D3 (pos 5 3 #f empty))  (define E3 (pos 5 4 #f empty))  (define F3 (pos 5 5 #f empty))  (define G3 (pos 5 6 #f empty))  (define H3 (pos 5 7 #f empty))
(define A2 (pos 6 0 #f pB0))    (define B2 (pos 6 1 #f pB1))    (define C2 (pos 6 2 #f pB2))    (define D2 (pos 6 3 #f pB3))    (define E2 (pos 6 4 #f pB4))    (define F2 (pos 6 5 #f pB5))    (define G2 (pos 6 6 #f pB6))    (define H2 (pos 6 7 #f pB7))
(define A1 (pos 7 0 #f tB0))    (define B1 (pos 7 1 #f cB0))    (define C1 (pos 7 2 #f bB0))    (define D1 (pos 7 3 #f qB0))    (define E1 (pos 7 4 #f kB0))    (define F1 (pos 7 5 #f bB1))    (define G1 (pos 7 6 #f cB1))    (define H1 (pos 7 7 #f tB1))

;CAVALO
;(define A8 (pos 0 0 #f cB1))    (define B8 (pos 0 1 #f cP0))     (define C8 (pos 0 2 #f bP0))    (define D8 (pos 0 3 #f qP0))     (define E8 (pos 0 4 #f kP0))    (define F8 (pos 0 5 #f bP1))     (define G8 (pos 0 6 #f cP1))    (define H8 (pos 0 7 #f tP1))
;(define A7 (pos 1 0 #f pP0))     (define B7 (pos 1 1 #f pP1))    (define C7 (pos 1 2 #f pP2))     (define D7 (pos 1 3 #f pP3))    (define E7 (pos 1 4 #f pP4))     (define F7 (pos 1 5 #f pP5))    (define G7 (pos 1 6 #f pP6))     (define H7 (pos 1 7 #f pP7))
;(define A6 (pos 2 0 #f empty))  (define B6 (pos 2 1 #f empty))   (define C6 (pos 2 2 #f empty))  (define D6 (pos 2 3 #f empty))   (define E6 (pos 2 4 #f empty))  (define F6 (pos 2 5 #f empty))   (define G6 (pos 2 6 #f cB0))  (define H6 (pos 2 7 #f empty))
;(define A5 (pos 3 0 #f empty))   (define B5 (pos 3 1 #f empty))  (define C5 (pos 3 2 #f empty))   (define D5 (pos 3 3 #f empty))  (define E5 (pos 3 4 #f empty))   (define F5 (pos 3 5 #f empty))  (define G5 (pos 3 6 #f empty))   (define H5 (pos 3 7 #f empty))
;(define A4 (pos 4 0 #f empty))  (define B4 (pos 4 1 #f empty))   (define C4 (pos 4 2 #f empty))  (define D4 (pos 4 3 #f empty))   (define E4 (pos 4 4 #f empty))  (define F4 (pos 4 5 #f empty))   (define G4 (pos 4 6 #f empty))  (define H4 (pos 4 7 #f empty))
;(define A3 (pos 5 0 #f empty))   (define B3 (pos 5 1 #f empty))  (define C3 (pos 5 2 #f cB0))   (define D3 (pos 5 3 #f empty))  (define E3 (pos 5 4 #f empty))   (define F3 (pos 5 5 #f empty))  (define G3 (pos 5 6 #f empty))   (define H3 (pos 5 7 #f empty))
;(define A2 (pos 6 0 #f pB0))    (define B2 (pos 6 1 #f cB1))     (define C2 (pos 6 2 #f pB2))    (define D2 (pos 6 3 #f pB3))     (define E2 (pos 6 4 #f pB4))    (define F2 (pos 6 5 #f pB5))     (define G2 (pos 6 6 #f pB6))    (define H2 (pos 6 7 #f pB7))
;(define A1 (pos 7 0 #f tB0))     (define B1 (pos 7 1 #f cB0))    (define C1 (pos 7 2 #f bB0))     (define D1 (pos 7 3 #f qB0))    (define E1 (pos 7 4 #f kB0))     (define F1 (pos 7 5 #f bB1))    (define G1 (pos 7 6 #f cB1))     (define H1 (pos 7 7 #f tB1))

;BISPO
;(define A8 (pos 0 0 #f tP0))    (define B8 (pos 0 1 #f cP0))     (define C8 (pos 0 2 #f bP0))    (define D8 (pos 0 3 #f qP0))     (define E8 (pos 0 4 #f kP0))    (define F8 (pos 0 5 #f bP1))     (define G8 (pos 0 6 #f cP1))    (define H8 (pos 0 7 #f tP1))
;(define A7 (pos 1 0 #f pP0))     (define B7 (pos 1 1 #f pP1))    (define C7 (pos 1 2 #f pP2))     (define D7 (pos 1 3 #f pP3))    (define E7 (pos 1 4 #f pP4))     (define F7 (pos 1 5 #f pP5))    (define G7 (pos 1 6 #f bP0))     (define H7 (pos 1 7 #f pP7))
;(define A6 (pos 2 0 #f empty))  (define B6 (pos 2 1 #f empty))   (define C6 (pos 2 2 #f empty))  (define D6 (pos 2 3 #f empty))   (define E6 (pos 2 4 #f empty))  (define F6 (pos 2 5 #f empty))   (define G6 (pos 2 6 #f empty))  (define H6 (pos 2 7 #f empty))
;(define A5 (pos 3 0 #f empty))   (define B5 (pos 3 1 #f empty))  (define C5 (pos 3 2 #f bB0))   (define D5 (pos 3 3 #f empty))  (define E5 (pos 3 4 #f empty))   (define F5 (pos 3 5 #f empty))  (define G5 (pos 3 6 #f empty))   (define H5 (pos 3 7 #f empty))
;(define A4 (pos 4 0 #f empty))  (define B4 (pos 4 1 #f empty))   (define C4 (pos 4 2 #f empty))  (define D4 (pos 4 3 #f empty))   (define E4 (pos 4 4 #f empty))  (define F4 (pos 4 5 #f empty))   (define G4 (pos 4 6 #f empty))  (define H4 (pos 4 7 #f empty))
;(define A3 (pos 5 0 #f empty))   (define B3 (pos 5 1 #f empty))  (define C3 (pos 5 2 #f empty))   (define D3 (pos 5 3 #f empty))  (define E3 (pos 5 4 #f empty))   (define F3 (pos 5 5 #f empty))  (define G3 (pos 5 6 #f bB0))   (define H3 (pos 5 7 #f empty))
;(define A2 (pos 6 0 #f pB0))    (define B2 (pos 6 1 #f pB1))     (define C2 (pos 6 2 #f pB2))    (define D2 (pos 6 3 #f pB3))     (define E2 (pos 6 4 #f pB4))    (define F2 (pos 6 5 #f pB5))     (define G2 (pos 6 6 #f pB6))    (define H2 (pos 6 7 #f pB7))
;(define A1 (pos 7 0 #f tB0))     (define B1 (pos 7 1 #f cB0))    (define C1 (pos 7 2 #f bB0))     (define D1 (pos 7 3 #f qB0))    (define E1 (pos 7 4 #f kB0))     (define F1 (pos 7 5 #f bB1))    (define G1 (pos 7 6 #f cB1))     (define H1 (pos 7 7 #f tB1))

;Torre
;(define A8 (pos 0 0 #f tP0))    (define B8 (pos 0 1 #f cP0))     (define C8 (pos 0 2 #f bP0))    (define D8 (pos 0 3 #f qP0))     (define E8 (pos 0 4 #f kP0))    (define F8 (pos 0 5 #f bP1))     (define G8 (pos 0 6 #f cP1))    (define H8 (pos 0 7 #f tP1))
;(define A7 (pos 1 0 #f pP0))     (define B7 (pos 1 1 #f pP1))    (define C7 (pos 1 2 #f pP2))     (define D7 (pos 1 3 #f pP3))    (define E7 (pos 1 4 #f pP4))     (define F7 (pos 1 5 #f pP5))    (define G7 (pos 1 6 #f pP6))     (define H7 (pos 1 7 #f pP7))
;(define A6 (pos 2 0 #f empty))  (define B6 (pos 2 1 #f empty))   (define C6 (pos 2 2 #f empty))  (define D6 (pos 2 3 #f empty))   (define E6 (pos 2 4 #f empty))  (define F6 (pos 2 5 #f tP1))   (define G6 (pos 2 6 #f empty))  (define H6 (pos 2 7 #f empty))
;(define A5 (pos 3 0 #f empty))   (define B5 (pos 3 1 #f empty))  (define C5 (pos 3 2 #f empty))   (define D5 (pos 3 3 #f empty))  (define E5 (pos 3 4 #f empty))   (define F5 (pos 3 5 #f empty))  (define G5 (pos 3 6 #f empty))   (define H5 (pos 3 7 #f empty))
;(define A4 (pos 4 0 #f empty))  (define B4 (pos 4 1 #f empty))   (define C4 (pos 4 2 #f empty))  (define D4 (pos 4 3 #f empty))   (define E4 (pos 4 4 #f empty))  (define F4 (pos 4 5 #f empty))   (define G4 (pos 4 6 #f empty))  (define H4 (pos 4 7 #f empty))
;(define A3 (pos 5 0 #f empty))   (define B3 (pos 5 1 #f empty))  (define C3 (pos 5 2 #f tB0))   (define D3 (pos 5 3 #f empty))  (define E3 (pos 5 4 #f empty))   (define F3 (pos 5 5 #f empty))  (define G3 (pos 5 6 #f empty))   (define H3 (pos 5 7 #f empty))
;(define A2 (pos 6 0 #f pB0))    (define B2 (pos 6 1 #f pB1))     (define C2 (pos 6 2 #f pB2))    (define D2 (pos 6 3 #f pB3))     (define E2 (pos 6 4 #f pB4))    (define F2 (pos 6 5 #f pB5))     (define G2 (pos 6 6 #f pB6))    (define H2 (pos 6 7 #f pB7))
;(define A1 (pos 7 0 #f tB0))     (define B1 (pos 7 1 #f cB0))    (define C1 (pos 7 2 #f bB0))     (define D1 (pos 7 3 #f qB0))    (define E1 (pos 7 4 #f kB0))     (define F1 (pos 7 5 #f bB1))    (define G1 (pos 7 6 #f cB1))     (define H1 (pos 7 7 #f tB1))
