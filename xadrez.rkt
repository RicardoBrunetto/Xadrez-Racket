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
(require 2htdp/universe)
(require 2htdp/image)
(require (prefix-in gui: racket/gui))
(require lang/posn)
(require math)
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
(define jogadorIA1 empty) ;Define quem é o jogador IA 1
(define jogadorIA2 empty) ;Define quem é o jogador IA 2
(define jogadorHumano1 empty) ;Define quem é o jogador Humano 1
(define jogadorHumano2 empty) ;Define quem é o jogador Humano 2
(define nomeJogador1 "Humano1") ;Define o nome do jogador humano 1
(define nomeJogador2 "Humano2") ;Define o nome do jogador humano 2
(define select 0) ;Variável para controlar os cliques (selecionar origem = 0 / selecionar destino = 1)
(define movimentos 0) ;Variável para contar os movimentos
(define jogador-atual empty) ;Define quem é o jogador atual
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
;Definição de como o jogo deve começar
(define jogada-inicial empty)
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
(define remove-empty-tests
  (test-suite
   "remove-empty tests"
   (check-match (remove-empty (list empty (list empty))) (list empty))
   (check-match (remove-empty (list 1 2 3 empty)) (list 1 2 3))
   (check-match (remove-empty (list 5 6)) (list 5 6))
   ))
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

;void -> jogador
;Encontra a vez de quem está jogará, retornando-o.
(define (change-player)
  (cond
    [(and (not (empty? jogadorHumano1))(equal? (jogador-nome jogador-atual) (jogador-nome jogadorHumano1))) ;JH1
      (if (empty? jogadorIA1) jogadorHumano2 jogadorIA1)
    ]
    [(and (not (empty? jogadorIA1))(equal? (jogador-nome jogador-atual) (jogador-nome jogadorIA1))) ;JIA1
      (if (empty? jogadorIA2) jogadorHumano1 jogadorIA2)
    ]
    [(and (not (empty? jogadorHumano2))(equal? (jogador-nome jogador-atual) (jogador-nome jogadorHumano2))) ;JH2
      jogadorHumano1
    ]
    [(and (not (empty? jogadorIA2))(equal? (jogador-nome jogador-atual) (jogador-nome jogadorIA2))) ;JIA2
      jogadorIA1
    ]
)
)

;jogador -> string
;Retorna o placar do jogador
(define (get-placar-string j1)
  (if (equal? (jogador-cor j1) branco)
      (string-append (jogador-nome j1) " " (number->string pts-branco))
      (string-append (number->string pts-preto) " " (jogador-nome j1)))
)

;jogada number -> image
;Gera uma bottom-bar com as informações
(define (bottom-bar partida movimentos)
  (define j1 (if (equal? branco (jogador-cor (jogada-jogador partida))) (jogada-jogador partida) (change-player)))
  (define j2 (if (equal? j1 (jogada-jogador partida)) (change-player) (jogada-jogador partida)))
  (underlay/align "right" "bottom"
    (underlay/align "left" "bottom"
      (underlay/align "center" "center"
        (rectangle largura-bottombar altura-bottombar "solid" btn-bar-color)
        (text (string-append (get-placar-string j1) " x " (get-placar-string j2)) 24 "white")
      )
      (text (string-append " Vez do jogador " (jogador-nome j1)) 18 "white")
    )
    (text (string-append (number->string movimentos) " Movimentos") 18 "white")
  )
)

;jogada number -> image
;Cria a tela de fim de jogo
(define (make-end-screen partida movimentos)
    (define j1 (if (equal? branco (jogador-cor (jogada-jogador partida))) (jogada-jogador partida) (change-player)))
    (define j2 (if (equal? j1 (jogada-jogador partida)) (change-player) (jogada-jogador partida)))
    (underlay/align "center" "bottom"
      (underlay/align "center" "top"
          (underlay/align "center" "center"
            (rectangle lado-tabuleiro (+ lado-tabuleiro altura-bottombar) "solid" "black")
            pic-gameover)
          (text/font "Tecle [ENTER] para jogar novamente"
                30 branco "Gill Sans" 'swiss 'normal 'bold #f)
      )
      (above
        (text/font (string-append "\n\nVitória do Jogador")
              15 branco "Gill Sans" 'swiss 'normal 'bold #f)
        (text/font  (jogador-nome (jogada-jogador partida))
              30 branco "Gill Sans" 'swiss 'normal 'bold #f)
        (text/font (string-append  "\n\n" (number->string movimentos) " movimentos\n\n")
              15 branco "Gill Sans" 'swiss 'normal 'bold #f)
        (text/font (string-append " \nPontuação final")
              15 branco "Gill Sans" 'swiss 'normal 'bold #f)
        (text/font (string-append (get-placar-string j1) " x " (get-placar-string j2))
              30 branco "Gill Sans" 'swiss 'normal 'bold #f)
      )
    )
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
          (if (equal? (peca-cor (pos-peca posEncontrada)) (jogador-cor jogador-atual))
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
  (make-jogada (mover-peca posOrigem (get-destino posOrigem) tabinterno) jogador-atual (check-king) pts-branco pts-preto)
)

;+--------------------------------------------+
;|            Manipulação das Peças           |
;+--------------------------------------------+

;peca peca mutable-array -> jogada
;Retorna um novo tabuleiro onde p1 assumiu o lugar de p2 e p2 está fora da jogada
(define (mover-peca p1 p2 tabinterno)
  (if (and (empty? jogadorIA2) (equal? jogador-atual jogadorIA1)) (sleep 1) (sleep 0))
  (void
   (cond
     [(empty? (pos-peca p2)) void]
     [else
      (if(equal? (jogador-cor jogador-atual) branco)
             (set! pts-branco (+ pts-branco (get-pontos-pos p2)))
             (set! pts-preto (+ pts-preto (get-pontos-pos p2))))
      (if(equal? (peca-tipo (pos-peca p2)) "rei")
             (void (set! king-is-dead 1) (set! jogador-atual (change-player))) void)
     ])
   (set! movimentos (add1 movimentos))
   (set! p2 (struct-copy pos p2[peca (pos-peca p1)] [destinavel #f]))
   (set! p1 (struct-copy pos p1[peca empty]))
   (set! select 0)
   (set! jogador-atual (change-player))
   (array-set! tabinterno (vector (pos-x p2) (pos-y p2)) p2)
   (array-set! tabinterno (vector (pos-x p1) (pos-y p1)) p1))
  (reset-selecao-pos possibilidades-temporarias tabinterno)
)

;number number mutable-array -> Posição
;Verifica se uma posição está dentro dos limites do tabuleiro: se sim, retorna a Peça na posição; se não, retorna empty.
(define get-pos-valida-tabuleiro-tests
  (test-suite
   "get-pos-valida-tabuleiro tests"
   (check-match (get-pos-valida-tabuleiro 0 0 tabuleiro) A8)
   (check-match (get-pos-valida-tabuleiro 5 6 tabuleiro) G3)
   (check-match (get-pos-valida-tabuleiro 1 -1 tabuleiro) empty)
   (check-match (get-pos-valida-tabuleiro 800 151 tabuleiro) empty)
   ))
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
   (check-match (get-cavalo-possibilidades B6) (list A8 C8 D7 D5 C4 A4))
   (check-match (get-cavalo-possibilidades G3) (list F5 H5 E4 E2 F1 H1))
   (check-match (get-cavalo-possibilidades G8) (list F6 H6))
   ))
(define (get-cavalo-possibilidades posC)
  (get-unitario-possibilidades posC validar-pos-cavalo-rei Lf-Cavalo)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posR, o Rei pode se movimentar.
(define get-rei-possibilidades-tests
  (test-suite
   "get-rei-possibilidades tests"
   (check-match (get-rei-possibilidades E1) (list F1 E2 D2 D1))
   (check-match (get-rei-possibilidades E8) empty)
   ))
(define (get-rei-possibilidades posR)
  (get-unitario-possibilidades posR validar-pos-cavalo-rei Lf-Rei)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posP, o Peao Branco pode se movimentar.
(define get-peaob-possibilidades-tests
  (test-suite
   "get-peaob-possibilidades tests"
   (check-match (get-peaob-possibilidades F4) (list E5))
   (check-match (get-peaob-possibilidades C6) (list B7 D7))
   (check-match (get-peaob-possibilidades C2) empty)
   ))
(define (get-peaob-possibilidades posP)
  (get-unitario-possibilidades posP validar-pos-peao Lf-Peao-B)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posP, o Peao Preto pode se movimentar.
(define get-peaop-possibilidades-tests
  (test-suite
   "get-peaop-possibilidades tests"
   (check-match (get-peaop-possibilidades C7) (list B6))
   (check-match (get-peaop-possibilidades F7) (list E6 F6 G6))
   (check-match (get-peaop-possibilidades E7) empty)
   ))
(define (get-peaop-possibilidades posP)
  (get-unitario-possibilidades posP validar-pos-peao Lf-Peao-P)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posB, o Bispo pode se movimentar.
(define get-bispo-possibilidades-tests
  (test-suite
   "get-bispo-possibilidades tests"
   (check-match (get-bispo-possibilidades B4) (list A5 A3 C5 D6 E7))
   (check-match (get-bispo-possibilidades G6) (list H7 F5 E4 D3 F7 H5))
   (check-match (get-bispo-possibilidades F8) empty)
   ))
(define (get-bispo-possibilidades posB)
  (get-recursivo-possibilidades posB Lf-Bispo)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posT, a Torre pode se movimentar.
(define get-torre-possibilidades-tests
  (test-suite
    "get-torre-possibilidades tests"
    (check-match (get-torre-possibilidades E6) (list E7 E5 E4 E3 E2 D6 F6))
    (check-match (get-torre-possibilidades C3) (list C4 C5 A3 B3 D3 E3 F3))
    (check-match (get-torre-possibilidades H8) empty)
    ))
(define (get-torre-possibilidades posT)
  (get-recursivo-possibilidades posT Lf-Torre)
)

;Posição -> Lista[Posição]
;Devolve uma lista de posições para onde, partindo de posR, a Rainha pode se movimentar.
(define get-rainha-possibilidades-tests
  (test-suite
    "get-rainha-possibilidades tests"
    (check-match (get-rainha-possibilidades D4) (list D6 D5 D3 D2 D1 C5 E3 G7 F6 E5 C4 E4 D7))
    (check-match (get-rainha-possibilidades D8) empty)
    ))
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

;Lista -> mutable-array
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

;Lista -> mutable-array
;Altera a a propriedade destinavel das peças em Lp para #f
(define (reset-selecao-pos Lp tab)
  (change-selecao-pos Lp #f tab)
)

;Lista -> mutable-array
;Altera a a propriedade destinavel das peças em Lp para #t
(define (make-selecao-pos Lp tab)
  (change-selecao-pos Lp #t tab)
)

;peca -> image
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
  (append (generate-layout-interno Lpar-xy empty) (list (bottom-bar w movimentos)))
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

;jogada -> jogada
;Reseta a jogada w para o estado inicial (jogada-inicial)
(define (set-jogada-as-inicial w)
  (set! tabuleiro (mutable-array-copy (jogada-tab jogada-inicial)))
  (set! jogador-atual (jogada-jogador jogada-inicial))
  (set! king-is-dead (jogada-king jogada-inicial))
  (set! pts-branco (jogada-ptsB jogada-inicial))
  (set! pts-preto (jogada-ptsP jogada-inicial))
  (set! movimentos 0)
  (make-jogada tabuleiro jogador-atual king-is-dead pts-branco pts-preto)
)

;number number -> Pos
;Retorna uma Posicao do tabuleiro em x e y
(define (get-pos-at x y)
  (if (or (>= x 8) (>= y 8)) empty
  (array-ref tabuleiro (vector x y)))
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

;jogada number number string -> jogada
;Manipula um evento de mouse (MouseEvent) nas coordenadas x y
(define (mouse-handler ws x y event)
  (cond [(and (not (empty? jogadorIA1))(equal? (jogador-nome (jogada-jogador ws)) (jogador-nome jogadorIA1))) ws]
        [(string=? event "button-down")
         (let ([posClicada (get-pos-at (quotient y 100) (quotient x 100))])
           (if (empty? posClicada) ws
           (if (zero? select)
           ;Jogador selecionando a origem
             (if (empty? (pos-peca posClicada))
                 ws
                 (if(equal? (peca-cor (pos-peca posClicada)) (jogador-cor (jogada-jogador ws)))
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
             (if (memf (λ(x)(and (equal? (pos-x posClicada) (pos-x x)) (equal? (pos-y posClicada) (pos-y x)))) possibilidades-temporarias)
                 (make-jogada (mover-peca posicao-origem posClicada (jogada-tab ws)) jogador-atual (check-king) pts-branco pts-preto);Posição válida
                 ws;Posição inválida
             )
           )))
        ]
        [else ws])
  )

;jogada string -> jogada
(define (key-handler w key)
 (cond
   [(key=? key "\r")
    (cond [(equal? (jogada-king w) 1) (set-jogada-as-inicial w)]
          [else w])
   ]
   [else w]
 )
)

;jogada -> image
;Desenha a interface gráfica do usuário
(define (desenhar-gui w)
  (if (empty? jogadorHumano1) (sleep 1) (sleep 0))
  (if (zero? (jogada-king w))
    (place-images
     (generate-layout w)
     (generate-posn)
     layout
    )
    (make-end-screen w movimentos)
  )
)

;jogada -> jogada
;Caso a IA esteja habilitada, faz uma jogada
(define (vez-da-IA w)
  (if (zero? (jogada-king w))
    (if (empty? jogadorIA1) w
    (if (equal? (jogador-nome (jogada-jogador w)) (jogador-nome jogadorIA1))
      (jogada-IA w)
      (if (empty? jogadorIA2)
        w
        (jogada-IA w)
    )))
    w
  )
)

;void -> universe
;Inicia um novo jogo
(define (start-new-game)
  (big-bang (make-jogada tabuleiro jogador-atual king-is-dead pts-branco pts-preto)
    (to-draw  desenhar-gui)
    (on-mouse mouse-handler)
    (on-tick  vez-da-IA)
    (on-key   key-handler)
    (name     "Xadrez")
  )
)

;void -> universe
;Inicia um novo jogo entre duas IA's
(define (start-new-ia-ia-game)
  (set! jogadorIA1  (jogador "Android" branco))
  (set! jogadorIA2  (jogador "iOS" preto ))
  (set! jogador-atual jogadorIA1)
  (set! jogada-inicial (make-jogada (mutable-array-copy tabuleiro) jogador-atual king-is-dead pts-branco pts-preto))
  (start-new-game)
)

;void -> universe
;Inicia um novo jogo entre duas IA's
(define (start-new-h-ia-game)
  (set! jogadorHumano1 (jogador nomeJogador1   branco))
  (set! jogadorIA1     (jogador "Computador"   preto ))
  (set! jogador-atual jogadorHumano1)
  (set! jogada-inicial (make-jogada (mutable-array-copy tabuleiro) jogador-atual king-is-dead pts-branco pts-preto))
  (start-new-game)
)

;void -> universe
;Inicia um novo jogo entre dois humanos
(define (start-new-h-h-game)
  (set! jogadorHumano1 (jogador nomeJogador1   branco))
  (set! jogadorHumano2 (jogador nomeJogador2   preto ))
  (set! jogador-atual jogadorHumano1)
  (set! jogada-inicial (make-jogada (mutable-array-copy tabuleiro) jogador-atual king-is-dead pts-branco pts-preto))
  (start-new-game)
)

;+--------------------------------------------+
;|                Tela Inicial                |
;+--------------------------------------------+
(define game-mode empty) ; Variável que define o tipo de jogo

(define frame (new gui:frame% [label "Configurações Iniciais"]))
(new gui:message% [parent frame] [label "Escolha o modo de jogo"])
(define panel (new gui:horizontal-panel% [parent frame] [alignment '(center center)]))
(new gui:button% [parent panel] [label "Humano vs Humano"]
        [callback (lambda (button event)  (send frame show #f) (set! game-mode 0) (send frame-jogador1 show #t) #t #f)])
(new gui:button% [parent panel] [label "Humano vs Computador"]
        [callback (lambda (button event)  (send frame show #f) (set! game-mode 1) (send frame-jogador1 show #t) #t #f)])
(new gui:button% [parent panel] [label "Computador vs Computador"]
        [callback (lambda (button event)  (send frame show #f) (start-new-ia-ia-game) #f #t )])
(send frame show #t)

(define frame-jogador1 (new gui:frame% [label "Nome dos Jogadores"]))
(define panel-jogador1 (new gui:horizontal-panel% [parent frame-jogador1] [alignment '(center center)]))
(new gui:message% [parent frame-jogador1] [label "Informe o nome do Jogador 1 (Peças Brancas)"])
(define jogador1txt (new gui:text-field% [parent panel-jogador1] [label "Jogador 1"]))
(new gui:button% [parent panel-jogador1] [label "Ok"]
                                [callback (lambda (button event)
                                          (set! nomeJogador1 (send jogador1txt get-value))
                                          (if (zero? game-mode)
                                            (send frame-jogador2 show #t)
                                            (start-new-h-ia-game)
                                          )
                                          (send frame-jogador1 show #f)
                                          #t #f)])

(define frame-jogador2 (new gui:frame% [label "Nome dos Jogadores"]))
(define panel-jogador2 (new gui:horizontal-panel% [parent frame-jogador2] [alignment '(center center)]))
(new gui:message% [parent frame-jogador2]
                      [label "Informe o nome do Jogador2 (Peças Pretas)"])
(define jogador2txt (new gui:text-field% [parent panel-jogador2] [label "Jogador 2"]))
(new gui:button% [parent panel-jogador2] [label "Ok"]
                                [callback (lambda (button event)  (set! nomeJogador2 (send jogador2txt get-value)) (send frame-jogador2 show #f) (start-new-h-h-game) #t #f)])

;+--------------------------------------------+
;|             Execução de Testes             |
;+--------------------------------------------+

;; Teste ... -> Void
;; Executa um conjunto de testes.
;(define (executa-testes . testes)
;  (run-tests (test-suite "Todos os testes" testes))
;  (void))
;
;;; Chama a função para executar os testes.
;(executa-testes
;  remove-empty-tests
;  get-pos-valida-tabuleiro-tests
;  get-peaop-possibilidades-tests
;  get-peaob-possibilidades-tests
;  get-torre-possibilidades-tests
;  get-cavalo-possibilidades-tests
;  get-bispo-possibilidades-tests
;  get-rainha-possibilidades-tests
;  get-rei-possibilidades-tests
;)
