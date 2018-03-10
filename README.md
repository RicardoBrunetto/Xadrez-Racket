# Xadrez em Racket
Este trabalho foi desenvolvido para a disciplina de Paradigma de Programação Lógica e Funcional (Ciência da Computação - UEM) em Dezembro/2017 por Ricardo Henrique Brunetto (ra94182@uem.br)

## Funcionalidade
O programa apresenta um jogo de Xadrez com três modalidades:
- Player vs. Player
- Player vs. CPU
- CPU vs. CPU

Não são permitidos os movimentos especiais que fujam da regra de movimentação clássica das peças (Roque, *en passant*, etc).

## Especificações Tecnológicas
Todo o programa foi escrito em Racket. O arquivo `xadrez.rkt` pode ser perfeitamente executado por linha de comando ou através do ambiente `DrRacket`, visto que não há dependências externas que requeiram complexidade.

## Implementação
O relatório com detalhes de implementação consta [aqui](Relatório/main.pdf).

## Lista de Afazeres
- [x] Movimento das peças
  - [x] Peão
  - [x] Torre
  - [x] Cavalo
  - [x] Bispo
  - [x] Rainha
  - [x] Rei
  - [x] FUNÇÃO DE FILTRO (OBSTÁCULOS)
- [ ] GUI
  - [x] Plot das peças
  - [x] Mouse/Key event handler
  - [x] Plot das possibilidades de movimento    
  - [x] Barra inferior
    - [x] Placar
    - [x] Jogador corrente  
  - [x] Tela final
    - [x] Repetição
  - [ ] Coordenadas
  - [ ] Ranking
  - [x] Sistema de Jogadores
- [x] Testes
  - [x] Documentação de função (cabeçalho)
  - [x] Testes de função
- [x] IA
  - [x] IA vs IA

### Limitações e Sugestões
- Implementar um sistema de Ranking. Segue como sugestão.
- Adicionar as coordenadas no tabuleiro. Segue como sugestão.

## Licença
As imagens utilizadas foram obtidas no Wikipedia, sob licença livre.

Este projeto segue a licença [Creative Commons Attribution-ShareAlike (BY-SA)](https://creativecommons.org/licenses/by-sa/4.0/), que está detalhada no arquivo [`LICENSE.md`](LICENSE.md).
<p align="center">
  <img src="https://licensebuttons.net/l/by-sa/3.0/88x31.png">
</p>
