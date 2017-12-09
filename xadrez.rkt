;+----------------------------------------------+
;|  Paradigma de Programação Lógica e Funcional |
;|                    Xadrez                    |
;|                                              |
;|      Ricardo Henrique Brunetto - RA94182     |
;|                Dezembro/2017                 |
;|                Maringá - PR                  |
;+----------------------------------------------+

#lang racket
;+----------------------------+
;|  Importação de Bibliotecas |
;+----------------------------+
(require math)
(require math/matrix)
(require rackunit)
(require rackunit/text-ui)
;(require alexis/util/struct)
;+-------------------------------+
;|  Importação de Módulos Locais |
;+-------------------------------+
(require "definicoes.rkt")

;Definições e Constantes
;(open-graphics)
;(define janela (open-viewport "Xadrez" 660 660))
(define u 0) ;Intercalar o tabuleiro com as cores
(define h 0) ;Posição inicial do eixo x
(define v 0) ;Posição inicial de eixo y

(define tabuleiro (mutable-array #[#[A8 B8 C8 D8 E8 F8 G8 H8]
                                   #[A7 B7 C7 D7 E7 F7 G7 H7]
                                   #[A6 B6 C6 D6 E6 F6 G6 H6]
                                   #[A5 B5 C5 D5 E5 F5 G5 H5]
                                   #[A4 B4 C4 D4 E4 F4 G4 H4]
                                   #[A3 B3 C3 D3 E3 F3 G3 H3]
                                   #[A2 B2 C2 D2 E2 F2 G2 H2]
                                   #[A1 B1 C1 D1 E1 F1 G1 H1]]))
;Concatena as listas aninhadas vazias
(define (remove-empty L)
  (cond
    [(empty? L) L]
    [(list? (first L)) (if (empty? (first L))
                   (remove-empty (rest L))
                   (append (first L) (remove-empty (rest L))))]
   [else (cons (first L) (remove-empty (rest L)))]
  )
)


;Retorna um novo tabuleiro onde p1 assumiu o lugar de p2 e p2 está fora do jogo
;(define-struct-updaters pos)
(define (mover-peca p1 p2)
  (set! p2 (struct-copy pos p2[peca (pos-peca p1)]))
  (set! p1 (struct-copy pos p1[peca empty]))
  (array-set! tabuleiro (vector (pos-x p1) (pos-y p1)) p1)
  (array-set! tabuleiro (vector (pos-x p2) (pos-y p2)) p2)
)

;+-------------------------------------------+
;|Cálculo das Posições Possíveis de cada Peça|
;+-------------------------------------------+

(define (get-pos-valida-tabuleiro x y)
  (cond
    [(and (and(> x -1) (< x 8)) (and(> y -1) (< y 8)))
     (array-ref tabuleiro (vector x y))]
    [else empty]
  )
)

(define Lf-Cavalo (list (list (λ(x)(sub1 x)) (λ(y)(+ y 2)))
                        (list (λ(x)(add1 x)) (λ(y)(+ y 2)))
                        (list (λ(x)(+ x 2)) (λ(y)(add1 y)))
                        (list (λ(x)(- x 2)) (λ(y)(add1 y)))
                        (list (λ(x)(- x 2)) (λ(y)(sub1 y)))
                        (list (λ(x)(+ x 2)) (λ(y)(sub1 y)))
                        (list (λ(x)(add1 x)) (λ(y)(- y 2)))
                        (list (λ(x)(sub1 x)) (λ(y)(- y 2)))))

(define Lf-Bispo (list (list (λ(x)(sub1 x)) (λ(y)(add1 y)))
                        (list (λ(x)(sub1 x)) (λ(y)(sub1 y)))
                        (list (λ(x)(add1 x)) (λ(y)(add1 y)))
                        (list (λ(x)(add1 x)) (λ(y)(sub1 y)))))
;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posC, o cavalo pode se movimentar.
(define get-cavalo-possibilidades-tests
  (test-suite
   "get-cavalo-possibilidades tests"
   (check-match (get-cavalo-possibilidades C3) (list B1 A2 A4 B5 D5 E4 E2 D1))
   (check-match (get-cavalo-possibilidades A8) (list B6 C7))
   (check-match (get-cavalo-possibilidades H4) (list G6 F5 F3 G2))
   (check-match (get-cavalo-possibilidades B2) (list A4 C4 D3 D1))
   ))
;Corpo do código
(define (get-cavalo-possibilidades posC)
  (define (get-cavalo-pos-interno Lfc Lp)
    (cond
      [(empty? Lfc) Lp]
      [else (get-cavalo-pos-interno (rest Lfc)
             (cons (get-pos-valida-tabuleiro ((first (first Lfc )) (pos-x posC)) ((second (first Lfc)) (pos-y posC))) Lp)
            )]
      )
  )
  (remove-empty (get-cavalo-pos-interno Lf-Cavalo empty)) ;chamado para remover os empty's
)

(define (get-bispo-possibilidades posB)
  (define (get-bispo-pos-interno f p Lb)
    (cond
      [(empty? p) Lb]
      [else (get-bispo-pos-interno f (get-pos-valida-tabuleiro ((first f) (pos-x p)) ((second f) (pos-y p)))
                                      (cons p Lb))]
    )
  )
  (define (dist-bispo-pos-interno Lfb Lp)
    (cond
      [(empty? Lfb) Lp]
      [else (dist-bispo-pos-interno (rest Lfb)
             (append (get-bispo-pos-interno (first Lfb) (get-pos-valida-tabuleiro ((first (first Lfb)) (pos-x posB)) ((second (first Lfb)) (pos-y posB))) empty) Lp)
            )]
      )
  )
  (remove-empty (dist-bispo-pos-interno Lf-Bispo empty)) ;chamado para remover os empty's
)
;(define (get-possibilidades pos1))

;(define make-display
;  (for ([v (in-range 11 651 80)])
;    (if (equal? u 0)
;      (for ([h (in-range 11 570 160)])
;        ((draw-solid-rectangle janela) (make-posn h v) 79 79 verde)
;        (display u)
;        (set! u (+ u 1))
;      )
;      (for ([h (in-range 91 651 160)])
;        ((draw-solid-rectangle janela) (make-posn h v) 79 79 branco)
;        (set! u (- u 1))
;      )
;  )
;))
;;;;;;;;;;;;;;;;;;;;
;; Funções para auxiliar nos testes

;; Teste ... -> Void
;; Executa um conjunto de testes.
(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executa-testes get-cavalo-possibilidades-tests)
