# **Manual de Utilizador**

## Inteligência Artificial | Projeto 1 - Jogo do Cavalo | André Meseiro 202100225 e Pedro Anjos 202100230

### 1. Objetivos do programa e descrição geral do seu funcionamento

O objetivo do programa é resolver o "Jogo do Cavalo", que decorre num tabuleiro 10x10, em que cada casa contém valores (pontuações) entre 00 e 99, sem repetição, distribuídos de forma aleatória cada vez que se inicia um novo jogo.

As regras de funcionamento adotadas para este problema específico são as seguintes:

* Apenas existe um jogador (cavalo branco);

* O jogo tem início com a colocação do cavalo numa casa à escolha, da 1ª linha do tabuleiro (A1-J1);

* As jogadas são efetuadas através de um movimento de cavalo, usando as regras tradicionais do Xadrez para o cavalo;

* Quando o jogador efetua uma jogada, o valor da casa de onde o jogador saiu é apagado, e a casa torna-se inacessível para o resto do jogo;

* Se a casa escolhida tiver um número com dois dígitos diferentes, por exemplo 24, o seu número simétrico, neste caso 42, é apagado do tabuleiro, ficando a respetiva casa inacessível durante o resto do jogo, não podendo terminar outra jogada na mesma;

* Se o cavalo for colocado numa casa com um número "duplo", por exemplo 33, o jogador pode remover qualquer outro número duplo do tabuleiro, à sua escolha (por *default* remove-se a de maior valor);

* Cada vez que uma casa é visitada, o valor da mesma é somado à pontuação do jogador; a remoção de simétricos e "duplos" não tem pontuação associada;

* O jogador apenas pode efetuar 8 movimentos por jogada e define uma pontuação objetivo a atingir antes de iniciar o jogo;

* O estado final é atingido quando o cavalo chega a uma casa que lhe permite obter uma pontuação igual ou superior ao objetivo definido;

* Se não for possível atingir o objetivo definido, o jogador será informado.

### 2. Utilização do programa (com exemplos)

* Ponto 1

![texto caso não carregue](http://url/to/img.png)

### 3. Informação necessária e produzida - *Input* e *Output* (ecrã/teclado e ficheiros)



### 4. Limitações do programa (do ponto de vista do utilizador)


