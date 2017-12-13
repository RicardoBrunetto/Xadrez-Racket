#lang racket

(provide (all-defined-out))

;;+--------------------------------------------+
;|          Importação de Bibliotecas         |
;+--------------------------------------------+
(require 2htdp/image)

;Definições de cores
(define preto (make-color 0 0 0 75)) ;Cor preta
(define branco "white") ;Cor branca
(define verde "MediumSeaGreen") ;Cor verde
(define btn-bar-color "RoyalBlue") ;Cor da barra inferior

;Definições do Tabuleiro
(define Lpar-xy (cartesian-product (range 8) (range 8))) ;Lista de pares ordenados com as posições possíveis
(define tamanho-quadrado 100) ;Tamanho de cada célula
(define lado-tabuleiro (* 8 tamanho-quadrado)) ;Tamanho do Tabuleiro inteiro

;Definições da Barra Inferior
(define altura-bottombar 50) ;Tamanho da barra inferior
(define largura-bottombar lado-tabuleiro) ;Tamanho da barra inferior

;Definições do Layout
(define layout (empty-scene lado-tabuleiro (+ lado-tabuleiro altura-bottombar))) ;Dimensão do Layout (tela do jogo)
(define pic-gameover (bitmap "imagens/game_over.png")) ;Imagem da tela de fim de jogo

;Estrutura que define uma jogada (World)
(struct jogada (tab jogador king ptsB ptsP))

;mutable-array jogador number number number -> jogada (world)
;Cria uma jogada (world) com os atributos
(define (make-jogada newTabuleiro newJogador newKing newPb newPp)
  (jogada newTabuleiro newJogador newKing newPb newPp)
)
;Estrutura que define um jogador
(struct jogador (nome pontos cor))

;color -> string
;Retorna o nome do jogador
(define (get-nome player)
  (cond
    [(equal? player branco) "BRANCO"]
    [(equal? player preto) "PRETO"]
  )
)

;number number jogador number -> image
;Gera uma bottom-bar com as informações
(define (bottom-bar placarB placarP player movimentos)
  (underlay/align "right" "bottom"
    (underlay/align "left" "bottom"
      (underlay/align "center" "center"
        (rectangle largura-bottombar altura-bottombar "solid" btn-bar-color)
        (text (string-append "Brancos " (number->string placarB) " x " (number->string placarP) " Pretos") 24 "white")
      )
      (text (string-append " Vez do jogador " (get-nome player)) 18 "white")
    )
    (text (string-append (number->string movimentos) " Movimentos") 18 "white")
  )
)

;jogada number -> image
;Cria a tela de fim de jogo
(define (make-end-screen partida movimentos)
    (underlay/align "center" "bottom"
      (underlay/align "center" "top"
          (underlay/align "center" "center"
            (rectangle lado-tabuleiro (+ lado-tabuleiro altura-bottombar) "solid" "black")
            pic-gameover)
          (text/font "Tecle [ENTER] para jogar novamente"
                30 branco "Gill Sans" 'swiss 'normal 'bold #f)
      )
      (above
        (text/font (string-append " Vitória do Jogador " (get-nome (jogada-jogador partida)))
              30 branco "Gill Sans" 'swiss 'normal 'bold #f)
        (text/font (string-append  "\n\n" (number->string movimentos) " movimentos\n\n")
              15 branco "Gill Sans" 'swiss 'normal 'bold #f)
        (text/font (string-append " \nPontuação final")
              15 branco "Gill Sans" 'swiss 'normal 'bold #f)
        (text/font (string-append  "BRANCO " (number->string (jogada-ptsB partida)) " x " (number->string (jogada-ptsP partida)) " PRETO\n\n")
              30 branco "Gill Sans" 'swiss 'normal 'bold #f)
      )
    )
)

;Estrutura de peça do tabuleiro
;Contém um id único (natural)
;Contém um tipo (string)
;Contém uma cor (color)
;Contém uma imagem para centro (image)
(struct peca (id tipo cor imagem) #:transparent #:mutable)
;Estrutura de posição do tabuleiro
;Contém uma coordenada (x -> linha (number)) e (y -> coluna (number))
;Contém um booleano para marcar se a posição é um destino
;Contém uma peça do tipo struct peca
(struct pos (x y destinavel peca) #:transparent #:mutable)

;+--------------------------------------------+
;|           Definição das Imagens            |
;+--------------------------------------------+

(define b-peao (bitmap "imagens/peaob.png"))
(define b-torre (bitmap "imagens/torreb.png"))
(define b-cavalo (bitmap "imagens/cavalob.png"))
(define b-bispo (bitmap "imagens/bispob.png"))
(define b-rainha (bitmap "imagens/rainhab.png"))
(define b-rei (bitmap "imagens/reib.png"))

(define p-peao (bitmap "imagens/peaop.png"))
(define p-torre (bitmap "imagens/torrep.png"))
(define p-cavalo (bitmap "imagens/cavalop.png"))
(define p-bispo (bitmap "imagens/bispop.png"))
(define p-rainha (bitmap "imagens/rainhap.png"))
(define p-rei (bitmap "imagens/reip.png"))

;+--------------------------------------------+
;|            Definição das Peças             |
;+--------------------------------------------+

;Peças Brancas
(define pB0 (peca 0 "peao" branco b-peao))
(define pB1 (peca 1 "peao" branco b-peao))
(define pB2 (peca 2 "peao" branco b-peao))
(define pB3 (peca 3 "peao" branco b-peao))
(define pB4 (peca 4 "peao" branco b-peao))
(define pB5 (peca 5 "peao" branco b-peao))
(define pB6 (peca 6 "peao" branco b-peao))
(define pB7 (peca 7 "peao" branco b-peao))

(define tB0 (peca 8 "torre" branco b-torre))
(define tB1 (peca 9 "torre" branco b-torre))

(define cB0 (peca 10 "cavalo" branco b-cavalo))
(define cB1 (peca 11 "cavalo" branco b-cavalo))

(define bB0 (peca 12 "bispo" branco b-bispo))
(define bB1 (peca 13 "bispo" branco b-bispo))

(define qB0 (peca 14 "rainha" branco b-rainha))
(define kB0 (peca 15 "rei"    branco b-rei))

;Peças Pretas
(define pP0 (peca 16 "peao" preto p-peao))
(define pP1 (peca 17 "peao" preto p-peao))
(define pP2 (peca 18 "peao" preto p-peao))
(define pP3 (peca 19 "peao" preto p-peao))
(define pP4 (peca 20 "peao" preto p-peao))
(define pP5 (peca 21 "peao" preto p-peao))
(define pP6 (peca 22 "peao" preto p-peao))
(define pP7 (peca 23 "peao" preto p-peao))

(define tP0 (peca 24 "torre" preto p-torre))
(define tP1 (peca 25 "torre" preto p-torre))

(define cP0 (peca 26 "cavalo" preto p-cavalo))
(define cP1 (peca 27 "cavalo" preto p-cavalo))

(define bP0 (peca 28 "bispo" preto p-bispo))
(define bP1 (peca 29 "bispo" preto p-bispo))

(define qP0 (peca 30 "rainha" preto p-rainha))
(define kP0 (peca 31 "rei"    preto p-rei))


;+--------------------------------------------+
;|           Definição do Tabuleiro           |
;+--------------------------------------------+

;Configuração do Tabuleiro
;(define A8 (pos 0 0 #f tP0))    (define B8 (pos 0 1 #f cP0))    (define C8 (pos 0 2 #f bP0))    (define D8 (pos 0 3 #f qP0))    (define E8 (pos 0 4 #f kP0))    (define F8 (pos 0 5 #f bP1))    (define G8 (pos 0 6 #f cP1))    (define H8 (pos 0 7 #f tP1))
;(define A7 (pos 1 0 #f pP0))    (define B7 (pos 1 1 #f pP1))    (define C7 (pos 1 2 #f pP2))    (define D7 (pos 1 3 #f pP3))    (define E7 (pos 1 4 #f pP4))    (define F7 (pos 1 5 #f pP5))    (define G7 (pos 1 6 #f pP6))    (define H7 (pos 1 7 #f pP7))
;(define A6 (pos 2 0 #f empty))  (define B6 (pos 2 1 #f empty))  (define C6 (pos 2 2 #f empty))  (define D6 (pos 2 3 #f empty))  (define E6 (pos 2 4 #f empty))  (define F6 (pos 2 5 #f empty))  (define G6 (pos 2 6 #f empty))  (define H6 (pos 2 7 #f empty))
;(define A5 (pos 3 0 #f empty))  (define B5 (pos 3 1 #f empty))  (define C5 (pos 3 2 #f empty))  (define D5 (pos 3 3 #f empty))  (define E5 (pos 3 4 #f empty))  (define F5 (pos 3 5 #f empty))  (define G5 (pos 3 6 #f empty))  (define H5 (pos 3 7 #f empty))
;(define A4 (pos 4 0 #f empty))  (define B4 (pos 4 1 #f empty))  (define C4 (pos 4 2 #f empty))  (define D4 (pos 4 3 #f empty))  (define E4 (pos 4 4 #f empty))  (define F4 (pos 4 5 #f empty))  (define G4 (pos 4 6 #f empty))  (define H4 (pos 4 7 #f empty))
;(define A3 (pos 5 0 #f empty))  (define B3 (pos 5 1 #f empty))  (define C3 (pos 5 2 #f empty))  (define D3 (pos 5 3 #f empty))  (define E3 (pos 5 4 #f empty))  (define F3 (pos 5 5 #f empty))  (define G3 (pos 5 6 #f empty))  (define H3 (pos 5 7 #f empty))
;(define A2 (pos 6 0 #f pB0))    (define B2 (pos 6 1 #f pB1))    (define C2 (pos 6 2 #f pB2))    (define D2 (pos 6 3 #f pB3))    (define E2 (pos 6 4 #f pB4))    (define F2 (pos 6 5 #f pB5))    (define G2 (pos 6 6 #f pB6))    (define H2 (pos 6 7 #f pB7))
;(define A1 (pos 7 0 #f tB0))    (define B1 (pos 7 1 #f cB0))    (define C1 (pos 7 2 #f bB0))    (define D1 (pos 7 3 #f qB0))    (define E1 (pos 7 4 #f kB0))    (define F1 (pos 7 5 #f bB1))    (define G1 (pos 7 6 #f cB1))    (define H1 (pos 7 7 #f tB1))

;+--------------------------------------------+
;|           Definições para Testes           |
;+--------------------------------------------+

;Tabuleiro de Testes
(define A8 (pos 0 0 #f tP0))    (define B8 (pos 0 1 #f cP0))    (define C8 (pos 0 2 #f bP0))    (define D8 (pos 0 3 #f qP0))    (define E8 (pos 0 4 #f kP0))    (define F8 (pos 0 5 #f bP1))    (define G8 (pos 0 6 #f cP1))    (define H8 (pos 0 7 #f tP1))
(define A7 (pos 1 0 #f pP0))    (define B7 (pos 1 1 #f pP1))    (define C7 (pos 1 2 #f pP2))    (define D7 (pos 1 3 #f pP3))    (define E7 (pos 1 4 #f pP4))    (define F7 (pos 1 5 #f pP5))    (define G7 (pos 1 6 #f pP6))    (define H7 (pos 1 7 #f pP7))
(define A6 (pos 2 0 #f empty))  (define B6 (pos 2 1 #f cB0))    (define C6 (pos 2 2 #f pB3))    (define D6 (pos 2 3 #f empty))  (define E6 (pos 2 4 #f tB0))    (define F6 (pos 2 5 #f empty))  (define G6 (pos 2 6 #f bB1))    (define H6 (pos 2 7 #f empty))
(define A5 (pos 3 0 #f empty))  (define B5 (pos 3 1 #f empty))  (define C5 (pos 3 2 #f empty))  (define D5 (pos 3 3 #f empty))  (define E5 (pos 3 4 #f empty))  (define F5 (pos 3 5 #f empty))  (define G5 (pos 3 6 #f empty))  (define H5 (pos 3 7 #f empty))
(define A4 (pos 4 0 #f empty))  (define B4 (pos 4 1 #f bB0))    (define C4 (pos 4 2 #f empty))  (define D4 (pos 4 3 #f qB0))    (define E4 (pos 4 4 #f empty))  (define F4 (pos 4 5 #f pB4))    (define G4 (pos 4 6 #f empty))  (define H4 (pos 4 7 #f empty))
(define A3 (pos 5 0 #f empty))  (define B3 (pos 5 1 #f empty))  (define C3 (pos 5 2 #f tB1))    (define D3 (pos 5 3 #f empty))  (define E3 (pos 5 4 #f empty))  (define F3 (pos 5 5 #f empty))  (define G3 (pos 5 6 #f cB1))    (define H3 (pos 5 7 #f empty))
(define A2 (pos 6 0 #f pB0))    (define B2 (pos 6 1 #f pB1))    (define C2 (pos 6 2 #f pB2))    (define D2 (pos 6 3 #f empty))  (define E2 (pos 6 4 #f empty))  (define F2 (pos 6 5 #f pB5))    (define G2 (pos 6 6 #f pB6))    (define H2 (pos 6 7 #f pB7))
(define A1 (pos 7 0 #f empty))  (define B1 (pos 7 1 #f empty))  (define C1 (pos 7 2 #f empty))  (define D1 (pos 7 3 #f empty))  (define E1 (pos 7 4 #f kB0))    (define F1 (pos 7 5 #f empty))  (define G1 (pos 7 6 #f empty))  (define H1 (pos 7 7 #f empty))
