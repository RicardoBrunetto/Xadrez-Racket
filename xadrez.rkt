;+---------------------------------------------+
;| Paradigma de Programação Lógica e Funcional |
;|                   Xadrez                    |
;|                                             |
;|     Ricardo Henrique Brunetto - RA94182     |
;|               Dezembro/2017                 |
;|               Maringá - PR                  |
;+---------------------------------------------+

#lang racket

;+--------------------------------------------+
;|          Importação de Bibliotecas         |
;+--------------------------------------------+
(require math)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(require math/matrix)
(require rackunit)
(require rackunit/text-ui)
;(require alexis/util/struct)

;+--------------------------------------------+
;|        Importação de Módulos Locais        |
;+--------------------------------------------+
(require "definicoes.rkt")

;+--------------------------------------------+
;|                 Definições                 |
;+--------------------------------------------+
;(open-graphics)
;(define janela (open-viewport "Xadrez" 660 660))
(define u 0) ;Intercalar o tabuleiro com as cores
(define h 0) ;Posição inicial do eixo x
(define v 0) ;Posição inicial de eixo y
(define select 0) ;Variável para controlar os cliques (selecionar origem = 0 / selecionar destino = 1)

;Definição do Tabuleiro (configuração inicial no arquivo definicoes.rkt)
(define tabuleiro (mutable-array #[#[A8 B8 C8 D8 E8 F8 G8 H8]
                                   #[A7 B7 C7 D7 E7 F7 G7 H7]
                                   #[A6 B6 C6 D6 E6 F6 G6 H6]
                                   #[A5 B5 C5 D5 E5 F5 G5 H5]
                                   #[A4 B4 C4 D4 E4 F4 G4 H4]
                                   #[A3 B3 C3 D3 E3 F3 G3 H3]
                                   #[A2 B2 C2 D2 E2 F2 G2 H2]
                                   #[A1 B1 C1 D1 E1 F1 G1 H1]]))
; Lista de funções para as possibilidades de locomoção do Cavalo
(define Lf-Cavalo (list (list (λ(x)(sub1 x))  (λ(y)(+ y 2)))
                        (list (λ(x)(add1 x))  (λ(y)(+ y 2)))
                        (list (λ(x)(+ x 2))   (λ(y)(add1 y)))
                        (list (λ(x)(- x 2))   (λ(y)(add1 y)))
                        (list (λ(x)(- x 2))   (λ(y)(sub1 y)))
                        (list (λ(x)(+ x 2))   (λ(y)(sub1 y)))
                        (list (λ(x)(add1 x))  (λ(y)(- y 2)))
                        (list (λ(x)(sub1 x))  (λ(y)(- y 2)))))
; Lista de funções para as possibilidades de locomoção do Bispo
(define Lf-Bispo  (list (list (λ(x)(sub1 x))  (λ(y)(add1 y)))
                        (list (λ(x)(sub1 x))  (λ(y)(sub1 y)))
                        (list (λ(x)(add1 x))  (λ(y)(add1 y)))
                        (list (λ(x)(add1 x))  (λ(y)(sub1 y)))))
; Lista de funções para as possibilidades de locomoção da Torre
(define Lf-Torre  (list (list (λ(x)(sub1 x))  (λ(y)y))
                        (list (λ(x)(add1 x))  (λ(y)y))
                        (list (λ(x)x)         (λ(y)(add1 y)))
                        (list (λ(x)x)         (λ(y)(sub1 y)))))
; Lista de funções para as possibilidades de locomoção do Peao
(define Lf-Peao   (list (list (λ(x)(sub1 x))  (λ(y)(add1 y)))
                        (list (λ(x)(add1 x))  (λ(y)(add1 y)))
                        (list (λ(x)x)         (λ(y)(add1 y)))))
; Lista de funções para as possibilidades de locomoção do Rei
(define Lf-Rei    (list (list (λ(x)(sub1 x))  (λ(y)y))
                        (list (λ(x)(sub1 x))  (λ(y)(add1 y)))
                        (list (λ(x)(sub1 x))  (λ(y)(sub1 y)))
                        (list (λ(x)x)         (λ(y)(add1 y)))
                        (list (λ(x)x)         (λ(y)(sub1 y)))
                        (list (λ(x)(add1 x))  (λ(y)(sub1 y)))
                        (list (λ(x)(add1 x))  (λ(y)y))
                        (list (λ(x)(add1 x))  (λ(y)(add1 y)))))

;+--------------------------------------------+
;|              Funções Auxiliares            |
;+--------------------------------------------+

;Lista -> Lista
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

;Posicao -> Boolean
;Verifica se uma peça tem a propriedade destinável como #t
(define (destinavel? posX)
  (cond
    [(empty? posX) #f]
    [else (pos-destinavel posX)]
  )
)

;+--------------------------------------------+
;|            Manipulação das Peças           |
;+--------------------------------------------+

;Peca -> Tabuleiro
;Retorna um novo tabuleiro onde p1 assumiu o lugar de p2 e p2 está fora do jogo
(define (mover-peca p1 p2)
  (set! p2 (struct-copy pos p2[peca (pos-peca p1)]))
  (set! p1 (struct-copy pos p1[peca empty]))
  (array-set! tabuleiro (vector (pos-x p1) (pos-y p1)) p1)
  (array-set! tabuleiro (vector (pos-x p2) (pos-y p2)) p2)
)

;Verifica se uma posição está dentro dos limites do tabuleiro: se sim, retorna a Peça na posição; se não, retorna empty.
(define (get-pos-valida-tabuleiro x y)
  (cond
    [(and (and(> x -1) (< x 8)) (and(> y -1) (< y 8)))
     (array-ref tabuleiro (vector x y))]
    [else empty]
  )
)

(define (peca-cor-igual? pos1 pos2) (equal? (peca-cor (pos-peca pos1)) (peca-cor (pos-peca pos2))))
(define (peca-cor-diferente? pos1 pos2) (not (peca-cor-igual? pos1 pos2)))
(define (validar-pos-cavalo-rei pos1 posC)
  (cond
    [(empty? pos1) empty]
    [(empty? (pos-peca pos1)) pos1]
    [(peca-cor-igual? pos1 posC) empty]
    [else pos1]
))
(define (validar-pos-peao pos1 posP)
  (cond
    [(empty? pos1) empty]
    [else
      (cond
        [(equal? (pos-x pos1) (pos-x posP)) (if (empty? (pos-peca pos1)) pos1 empty)]
        [else (if (empty? (pos-peca pos1)) empty (if (peca-cor-diferente? pos1 posP) pos1 empty))]
)]))

(define (get-unitario-possibilidades posX fval Lfuncoes)
  (define (get-unitario-pos-interno Lf Lp)
    (cond
      [(empty? Lf) Lp]
      [else (get-unitario-pos-interno (rest Lf)
             (cons (fval (get-pos-valida-tabuleiro ((first (first Lf )) (pos-x posX)) ((second (first Lf)) (pos-y posX))) posX) Lp)
            )]
      )
  )
  (remove-empty (get-unitario-pos-interno Lfuncoes empty)) ;chamado para remover os empty's
)

(define (get-recursivo-possibilidades posX Lfuncoes)
  (define (get-recursivo-pos-interno f p Lp)
    (cond
      [(empty? p) Lp]
      [(empty? (pos-peca p)) (get-recursivo-pos-interno f (get-pos-valida-tabuleiro ((first f) (pos-x p)) ((second f) (pos-y p)))
                             (cons p Lp))] ;Adiciona a peça e passa para a próxima posição
      [(peca-cor-diferente? p posX) (get-recursivo-pos-interno f empty (cons p Lp)) ] ;Adiciona a peça e retorna (não há como seguir)
      [else Lp]
    )
  )
  (define (dist-recursivo-pos-interno Lf Lp)
    (cond
      [(empty? Lf) Lp]
      [else (dist-recursivo-pos-interno (rest Lf)
             (append (get-recursivo-pos-interno (first Lf)
                     (get-pos-valida-tabuleiro ((first (first Lf)) (pos-x posX)) ((second (first Lf)) (pos-y posX))) empty) Lp)
            )]
      )
  )
  (remove-empty (dist-recursivo-pos-interno Lfuncoes empty)) ;chamado para remover os empty's
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posC, o Cavalo pode se movimentar.
(define get-cavalo-possibilidades-tests
  (test-suite
   "get-cavalo-possibilidades tests"
   (check-match (get-cavalo-possibilidades C3) (list A4 B5 D5 E4))
   (check-match (get-cavalo-possibilidades G6) (list H8 H4 E7 E5 F8 F4))
   (check-match (get-cavalo-possibilidades A8) (list B6 C7))
   (check-match (get-cavalo-possibilidades B2) (list A4 C4 D3))
   ))
;Corpo do código
(define (get-cavalo-possibilidades posC)
  (get-unitario-possibilidades posC validar-pos-cavalo-rei Lf-Cavalo)
)
(define (get-rei-possibilidades posR)
  (get-unitario-possibilidades posR validar-pos-cavalo-rei Lf-Rei)
)
(define (get-peao-possibilidades posP)
  (get-unitario-possibilidades posP validar-pos-peao Lf-Peao)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posB, o Bispo pode se movimentar.
(define get-bispo-possibilidades-tests
  (test-suite
   "get-bispo-possibilidades tests"
   (check-match (get-bispo-possibilidades C1) empty)
   (check-match (get-bispo-possibilidades G7) (list H6 G6 E5 D4 C3 B2))
   (check-match (get-bispo-possibilidades G3) (list H4 F4 E5 D6 C7))
   (check-match (get-bispo-possibilidades C5) (list B6 A7 B4 A3 D6 D4 E7 E3))
   ))
;Corpo do Código
(define (get-bispo-possibilidades posB)
  (get-recursivo-possibilidades posB Lf-Bispo)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posT, a Torre pode se movimentar.
(define get-torre-possibilidades-tests
  (test-suite
    "get-torre-possibilidades tests"
    (check-match (get-torre-possibilidades A1) empty)
    (check-match (get-torre-possibilidades C3) (list A3 B3 D3 E3 F3 G3 H3 C4 C5 C6 C7))
    (check-match (get-torre-possibilidades F6) (list H4 F4 E5 D6 C7))
    (check-match (get-torre-possibilidades C5) (list B6 A7 B4 A3 D6 D4 E7 E3))
    ))
;Corpo do Código
(define (get-torre-possibilidades posT)
  (get-recursivo-possibilidades posT Lf-Torre)
)

(define (get-rainha-possibilidades posR)
  (remove-empty (append (get-bispo-possibilidades posR) (get-torre-possibilidades posR)))
)



;+--------------------------------------------+
;|              Interface Gráfica             |
;+--------------------------------------------+
;Posicao -> Cor
;Retorna a cor de fundo que uma determinada posição posX deve ter no tabuleiro
(define (get-background-pos posX)
  (if (even? (+ (pos-x posX) (pos-y posX))) branco preto)
)

;Lista -> void
;Altera a a propriedade destinavel das peças em Lp para vf
(define (change-selecao-pos Lp vf)
  (cond
    [(empty? Lp) Lp]
    [else (
      void
      (let ([posR (first Lp)])
      (set! posR (struct-copy pos posR[destinavel vf]))
      (array-set! tabuleiro (vector (pos-x posR) (pos-y posR)) posR) empty)
      (reset-selecao-pos (rest Lp))
      )]
))

;Lista -> void
;Altera a a propriedade destinavel das peças em Lp para #f
(define (reset-selecao-pos Lp)
  (change-selecao-pos Lp #f)
)

;Lista -> void
;Altera a a propriedade destinavel das peças em Lp para #t
(define (make-selecao-pos Lp)
  (change-selecao-pos Lp #t)
)


(define tamanho-quadrado 100)
(define (make-celula cor)
  (square tamanho-quadrado "outline" cor)
)
(define layout (empty-scene (* 8 tamanho-quadrado) (* 8 tamanho-quadrado)))

(define (generate-layout Lpar)
  (define (generate-layout-interno Lp Lpos)
    (cond
      [(empty? Lp) Lpos]
      [else
       (let ([posX (get-pos-valida-tabuleiro (first (first Lp)) (second (first Lp)))])
       (if (destinavel? posX)
           (generate-layout-interno  (rest Lp) (cons (make-celula verde) Lpos))
           (if(even? (+(pos-x posX) (pos-y posX)))
              (generate-layout-interno  (rest Lp) (cons (make-celula branco) Lpos))
              (generate-layout-interno  (rest Lp) (cons (make-celula preto) Lpos)))
           ))])
  )
  (generate-layout-interno Lpar empty)
)

(define (generate-posn Lpar-xy)
  (define (generate-posn-interno Lp Lpos)
    (cond
      [(empty? Lp) Lpos]
      [else (generate-posn-interno (rest Lp)
            (cons (make-posn (+ (/ tamanho-quadrado 2) (* tamanho-quadrado (second (first Lp)))) (+ (/ tamanho-quadrado 2) (* tamanho-quadrado (first (first Lp))))) Lpos))]
    )
  )
  (generate-posn-interno Lpar-xy empty)
)

(define (desenhar-gui w)
  (define Lpar-xy (cartesian-product (range 8) (range 8)))
  (place-images
   (generate-layout-interno Lpar-xy)
   (generate-posn Lpar-xy)
   layout
  )
)

(big-bang
  0
  (to-draw desenhar-gui)
)

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

;+--------------------------------------------+
;|             Execução de Testes             |
;+--------------------------------------------+

;; Teste ... -> Void
;; Executa um conjunto de testes.
;(define (executa-testes . testes)
;  (run-tests (test-suite "Todos os testes" testes))
;  (void))

; Chama a função para executar os testes.
;(executa-testes
;  get-cavalo-possibilidades-tests
;  get-bispo-possibilidades-tests
;  get-torre-possibilidades-tests
;)
