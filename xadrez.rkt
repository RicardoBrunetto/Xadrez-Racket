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

;+--------------------------------------------+
;|        Importação de Módulos Locais        |
;+--------------------------------------------+
(require "definicoes.rkt")

;+--------------------------------------------+
;|                 Definições                 |
;+--------------------------------------------+
(define ranking empty)
(define jogadorIA empty) ;Define quem é o jogador IA
(define jogadorHumano branco) ;Define quem é o jogador Humano
(define select 0) ;Variável para controlar os cliques (selecionar origem = 0 / selecionar destino = 1)
(define jogador-atual branco) ;Define quem é o jogador atual
(define possibilidades-temporarias empty) ;Lista de possibilidades de locomoção temporárias
(define posicao-origem empty) ;Posição de origem (de onde um jogador deseja fazer o movimento)
(define king-is-dead 0) ;Variável que atesta que ambos reis estão vivos (0) ou algum está morto (1)
(define pts-branco 0) ;Placar do jogador branco
(define pts-preto 0) ;Placar do jogador preto
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
; Lista de funções para as possibilidades de locomoção do Peao Preto
(define Lf-Peao-P (list (list (λ(x)(add1 x))  (λ(y)(add1 y)))
                        (list (λ(x)(add1 x))  (λ(y)y))
                        (list (λ(x)(add1 x))  (λ(y)(sub1 y)))))
; Lista de funções para as possibilidades de locomoção do Peao Branco
(define Lf-Peao-B (list (list (λ(x)(sub1 x))  (λ(y)(add1 y)))
                        (list (λ(x)(sub1 x))  (λ(y)y))
                        (list (λ(x)(sub1 x))  (λ(y)(sub1 y)))))
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
;Verifica se posX tem a propriedade destinável como #t
(define (destinavel? posX)
  (cond
    [(empty? posX) #f]
    [else (pos-destinavel posX)]
  )
)

;void -> cor
;Altera a vez de quem está jogando, retornando-a.
(define (change-player)
  (if (equal? jogador-atual branco)
      (set! jogador-atual preto)
      (set! jogador-atual branco))
  jogador-atual
)

;Posicao -> number
;Retorna o valor de pontos da peça contida na posição posX
(define (get-pontos-pos posX)
  (cond
    [(empty? posX) -1]
    [(empty? (pos-peca posX)) 0]
    [(empty? (pos-peca posX)) 0]
    [else
      (let ([tipo (peca-tipo (pos-peca posX))])
        (cond
          [(equal? tipo "peao") 1]
          [(equal? tipo "torre") 3]
          [(equal? tipo "cavalo") 5]
          [(equal? tipo "bispo") 7]
          [(equal? tipo "rainha") 10]
          [(equal? tipo "rei") 49]
   ))])
)

;Posicao Posicao -> Posicao
;Retorna a Posição de maior valor entre posX e posY
(define (get-maior-pos posX posY)
  (if (> (get-pontos-pos posX) (get-pontos-pos posY)) posX posY)
)

;+--------------------------------------------+
;|           Inteligência Artificial          |
;+--------------------------------------------+

;jogada -> jogada
;Simula uma jogada da IA de acordo com as regras do jogo
(define (jogada-IA ws)
  (define tabinterno (jogada-tab ws)) ;Tabuleiro interno a ser manipulado

  ;Internamente, a ideia é encontrar uma posição de origem partindo de um endereço aleatório
  (define (selecionar-Origem)
    (define (percorrerTabuleiro x y)
      (define posEncontrada (get-pos-valida-tabuleiro x y tabinterno))
      (cond
        [(empty? (pos-peca posEncontrada)) ;Caso a posição encontrada não seja válida (não tem peça), tenta com números aleatórios novamente
          (percorrerTabuleiro (random 0 8 (current-pseudo-random-generator)) (random 0 8 (current-pseudo-random-generator)))]
        [else ;Caso tenha encontrado uma válida, confere se é do jogador
          (if (equal? (peca-cor (pos-peca posEncontrada)) jogador-atual)
              (if (empty? (get-possibilidades-peca posEncontrada)) ;Não há como mover a peça
                  (percorrerTabuleiro (random 0 8 (current-pseudo-random-generator)) (random 0 8 (current-pseudo-random-generator)))
                  posEncontrada
              )
              (percorrerTabuleiro (random 0 8 (current-pseudo-random-generator)) (random 0 8 (current-pseudo-random-generator)))
      )])
    )
    (percorrerTabuleiro (random 0 8 (current-pseudo-random-generator)) (random 0 8 (current-pseudo-random-generator)))
  )

  ;Encontra uma posição-alvo dada uma origem posX através de sua Lista de Possibilidades
  (define (get-destino posX)
    (define (get-destino-interno maiorPos Lp)
      (cond
        [(empty? Lp) maiorPos]
        [else (get-destino-interno (get-maior-pos (first Lp) maiorPos) (rest Lp))]
      )
    )
    (get-destino-interno empty (get-possibilidades-peca posX))
  )
  (define posOrigem (selecionar-Origem))
  (make-jogada (mover-peca posOrigem (get-destino posOrigem) tabinterno) (change-player) (check-king) pts-branco pts-preto)
  ;(make-jogada tabinterno jogador-atual kinginterno (jogada-ptsB ws) ptsinterno)
)

;+--------------------------------------------+
;|            Manipulação das Peças           |
;+--------------------------------------------+

;Peca Peca Jogada -> Tabuleiro
;Retorna um novo tabuleiro onde p1 assumiu o lugar de p2 e p2 está fora da Jogada
(define (mover-peca p1 p2 tabinterno)
  (void
   (cond
     [(empty? (pos-peca p2)) void]
     [else
      (if (equal? (peca-tipo (pos-peca p2)) "rei")
          (set! king-is-dead 1)
          (if(equal? jogador-atual branco)
            (set! pts-branco (+ pts-branco (get-pontos-pos p2)))
            (set! pts-preto (+ pts-preto (get-pontos-pos p2)))
          )
          )])
   (set! p2 (struct-copy pos p2[peca (pos-peca p1)] [destinavel #f]))
   (set! p1 (struct-copy pos p1[peca empty]))
   (set! select 0)
   (array-set! tabinterno (vector (pos-x p2) (pos-y p2)) p2)
   (array-set! tabinterno (vector (pos-x p1) (pos-y p1)) p1))
  (reset-selecao-pos possibilidades-temporarias tabinterno)
)

;Verifica se uma posição está dentro dos limites do tabuleiro: se sim, retorna a Peça na posição; se não, retorna empty.
(define (get-pos-valida-tabuleiro x y tab)
  (cond
    [(and (and(> x -1) (< x 8)) (and(> y -1) (< y 8)))
     (array-ref tab (vector x y))]
    [else empty]
  )
)

;Posicao Posicao -> Boolean
;Verifica se as peças nas posições pos1 e pos2 são iguais
(define (peca-cor-igual? pos1 pos2) (equal? (peca-cor (pos-peca pos1)) (peca-cor (pos-peca pos2))))

;Posicao Posicao -> Boolean
;Verifica se as peças nas posições pos1 e pos2 são diferntes
(define (peca-cor-diferente? pos1 pos2) (not (peca-cor-igual? pos1 pos2)))

;Posicao Posicao -> Posicao
;Função de validade do Cavalo e do Rei. Retorna pos1 caso seja permitido que o Cavalo ou Rei (em posC) chegue em pos1. Retorna empty caso contrário.
(define (validar-pos-cavalo-rei pos1 posC)
  (cond
    [(empty? pos1) empty]
    [(empty? (pos-peca pos1)) pos1]
    [(peca-cor-igual? pos1 posC) empty]
    [else pos1]
))
;Posicao Posicao -> Posicao
;Função de validade do Peão. Retorna pos1 caso seja permitido que o Peão (em posP) chegue em pos1. Retorna empty caso contrário.
(define (validar-pos-peao pos1 posP)
  (cond
    [(empty? pos1) empty]
    [else
      (cond
        [(equal? (pos-y pos1) (pos-y posP)) (if (empty? (pos-peca pos1)) pos1 empty)]
        [else (if (empty? (pos-peca pos1)) empty (if (peca-cor-diferente? pos1 posP) pos1 empty))]
)]))

;Posicao Funcao Lista[Funcao] -> Lista[Posicao]
;Função que retorna a lista de posições que determinada peça em posX pode ter como destino.
;Aplica uma única vez (unitário) as funções em Lfuncoes para cada possibilidade. Valida a possibilidade com a função fval.
(define (get-unitario-possibilidades posX fval Lfuncoes)
  (define (get-unitario-pos-interno Lf Lp)
    (cond
      [(empty? Lf) Lp]
      [else (get-unitario-pos-interno (rest Lf)
             (cons (fval (get-pos-valida-tabuleiro ((first (first Lf )) (pos-x posX)) ((second (first Lf)) (pos-y posX)) tabuleiro) posX) Lp)
            )]
      )
  )
  (remove-empty (get-unitario-pos-interno Lfuncoes empty)) ;chamado para remover os empty's
)

;Posicao Funcao Lista[Funcao] -> Lista[Posicao]
;Função que retorna a lista de posições que determinada peça em posX pode ter como destino.
;Aplica recursivamente as funções em Lfuncoes para cada possibilidade.
(define (get-recursivo-possibilidades posX Lfuncoes)
  (define (get-recursivo-pos-interno f p Lp)
    (cond
      [(empty? p) Lp]
      [(empty? (pos-peca p)) (get-recursivo-pos-interno f (get-pos-valida-tabuleiro ((first f) (pos-x p)) ((second f) (pos-y p)) tabuleiro)
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
                     (get-pos-valida-tabuleiro ((first (first Lf)) (pos-x posX)) ((second (first Lf)) (pos-y posX)) tabuleiro) empty) Lp)
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

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posR, o Rei pode se movimentar.
(define (get-rei-possibilidades posR)
  (get-unitario-possibilidades posR validar-pos-cavalo-rei Lf-Rei)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posP, o Peao Branco pode se movimentar.
(define (get-peaob-possibilidades posP)
  (get-unitario-possibilidades posP validar-pos-peao Lf-Peao-B)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posP, o Peao Preto pode se movimentar.
(define (get-peaop-possibilidades posP)
  (get-unitario-possibilidades posP validar-pos-peao Lf-Peao-P)
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

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posR, a Rainha pode se movimentar.
(define (get-rainha-possibilidades posR)
  (remove-empty (append (get-bispo-possibilidades posR) (get-torre-possibilidades posR)))
)

;Posicao -> Lista[Posicao]
;Devolve uma lista de posições para onde, partindo de posX, a peça contida pode se movimentar.
(define (get-possibilidades-peca posX)
  (cond
    [(empty? posX) empty]
    [(empty? (pos-peca posX)) empty]
    [else
      (let ([tipo (peca-tipo (pos-peca posX))])
        (cond
          [(equal? tipo "peao") (if(equal? (peca-cor (pos-peca posX)) branco)(get-peaob-possibilidades posX)(get-peaop-possibilidades posX))]
          [(equal? tipo "torre") (get-torre-possibilidades posX)]
          [(equal? tipo "cavalo") (get-cavalo-possibilidades posX)]
          [(equal? tipo "bispo") (get-bispo-possibilidades posX)]
          [(equal? tipo "rainha") (get-rainha-possibilidades posX)]
          [(equal? tipo "rei") (get-rei-possibilidades posX)]
      ))]
  )
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
(define (change-selecao-pos Lp vf tab)
  (cond
    [(empty? Lp) tab]
    [else (and
      (void
      (let ([posR (get-pos-valida-tabuleiro (pos-x (first Lp)) (pos-y (first Lp)) tab)])
      (set! posR (struct-copy pos posR[destinavel vf]))
      (array-set! tab (vector (pos-x posR) (pos-y posR)) posR)))
      (change-selecao-pos (rest Lp) vf tab))
   ]
))

;Lista -> void
;Altera a a propriedade destinavel das peças em Lp para #f
(define (reset-selecao-pos Lp tab)
  (change-selecao-pos Lp #f tab)
)

;Lista -> void
;Altera a a propriedade destinavel das peças em Lp para #t
(define (make-selecao-pos Lp tab)
  (change-selecao-pos Lp #t tab)
)

;Peca -> image
;Retorna a imagem de centro (primeiro plano) da célula
(define (get-image pec)
  (cond
    [(empty? pec) empty-image]
    [else (peca-imagem pec)]
  )
)

;Color Posicao -> image
;Retorna a imagem (de fundo - que será de cor cor - e de primeiro plano) da peca em posX
(define (make-celula cor posX)
  (if (destinavel? posX) ;Se for destinável, adiciona um ponto verde ao centro
    (underlay/align "center" "center"
      (square tamanho-quadrado "solid" cor)
      (get-image (pos-peca posX))
      (circle 10 "solid" verde)
    )
    (underlay/align "center" "center"
      (square tamanho-quadrado "solid" cor)
      (get-image (pos-peca posX))
    )
  )
)

;void -> Lista[image]
;Usa a lista de pares ordenados em Lpar-xy e retorna a imagem que deve preencher cada célula no tabuleiro
(define (generate-layout w)
  (define (generate-layout-interno Lp Lpos)
    (cond
      [(empty? Lp) Lpos]
      [else
       (define posX (get-pos-valida-tabuleiro (first (first Lp)) (second (first Lp)) (jogada-tab w)))
       (generate-layout-interno  (rest Lp) (cons (make-celula (get-background-pos posX) posX) Lpos))
      ]
  ))
  (append (generate-layout-interno Lpar-xy empty) (list (bottom-bar (jogada-ptsB w) (jogada-ptsP w) (jogada-jogador w))))
)

;void -> Lista[posn]
;Usa a lista de pares ordenados em Lpar-xy e retorna as posições que cada célula deve ocupar no tabuleiro
(define (generate-posn)
  (define (generate-posn-interno Lp Lpos)
    (cond
      [(empty? Lp) Lpos]
      [else (generate-posn-interno (rest Lp)
            (cons (make-posn (+ (/ tamanho-quadrado 2) (* tamanho-quadrado (second (first Lp)))) (+ (/ tamanho-quadrado 2) (* tamanho-quadrado (first (first Lp))))) Lpos))]
    )
  )
  (append (generate-posn-interno Lpar-xy empty) (list (make-posn (/ largura-bottombar 2) (+ (/ altura-bottombar 2) lado-tabuleiro))))
)

;number number -> Pos
;Retorna uma Posicao do tabuleiro em x e y
(define (get-pos-at x y)
  (if (or (>= x 8) (>= y 8)) empty
  (array-ref tabuleiro (vector x y)))
)

;mutable-array jogador number number number -> jogada (world)
;Cria uma jogada (world) com os atributos
(define (make-jogada newTabuleiro newJogador newKing newPb newPp)
  (jogada newTabuleiro newJogador newKing newPb newPp)
)

;void -> number
;Retorna o jogador atual
(define (get-jogador)
  jogador-atual
)

;void -> number
;Retorna a variável de vitória atual
(define (check-king)
  king-is-dead)

;jogada number number string -> void
;Manipula um evento de mouse (MouseEvent) nas coordenadas x y
(define (mouse-handler ws x y event)
  (cond [(and (not (empty? jogadorIA))(equal? (jogada-jogador ws) jogadorIA)) ws]
        [(string=? event "button-down")
         (let ([posClicada (get-pos-at (quotient y 100) (quotient x 100))])
           (if (empty? posClicada) ws
           (if (zero? select)
           ;Jogador selecionando a origem
             (if (empty? (pos-peca posClicada))
                 ws
                 (if(equal? (peca-cor (pos-peca posClicada)) (jogada-jogador ws))
                  (and (void
                  (set! possibilidades-temporarias
                        (get-possibilidades-peca posClicada))
                  (if (empty? possibilidades-temporarias)
                    (set! possibilidades-temporarias empty)
                    (void
                      (set! posicao-origem posClicada)
                      (make-selecao-pos possibilidades-temporarias (jogada-tab ws))
                      (set! select 1)
                  ))) ws) ws)
            )
           ; Jogador selecionando o destino
             (if (memf (lambda (x) (and (equal? (pos-x posClicada) (pos-x x)) (equal? (pos-y posClicada) (pos-y x)))) possibilidades-temporarias)
                 (make-jogada (mover-peca posicao-origem posClicada (jogada-tab ws)) (change-player) (check-king) pts-branco pts-preto);Posição válida
                 ws;Posição inválida
             )
           )))
        ]
        [else ws])
  )

;jogada -> image
;Desenha a interface gráfica do usuário
(define (desenhar-gui w)
  (if (zero? (jogada-king w))
    (place-images
     (generate-layout w)
     (generate-posn)
     layout
    )
    (make-end-screen w))
)

(define (vez-da-IA w)
  (if (equal? (jogada-jogador w) jogadorIA)
    (jogada-IA w)
    w
  )
)

(define (start-new-game)
  (big-bang (make-jogada tabuleiro jogador-atual king-is-dead pts-branco pts-preto)
    (to-draw desenhar-gui)
    (on-mouse mouse-handler)
    (on-tick vez-da-IA)
    (name "Xadrez"))
)

(start-new-game)

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
