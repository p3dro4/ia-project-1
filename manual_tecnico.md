# **Manual Técnico**

## Inteligência Artificial | Projeto 1 - Jogo do Cavalo | André Meseiro 202100225 e Pedro Anjos 202100230

### 1. Algoritmos Implementados

### 1.1. Algoritmo de Procura em Largura - Breadth-First Search (BFS)

```lisp
;; Algoritmo de procura em largura
(defun bfs (no-inicial objetivop funcao-sucessores operadores &optional (nos-expandidos 0) (nos-gerados 0) abertos fechados (tempo-inicial (get-internal-real-time)))
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores e os operadores. Retorna uma lista com os nós que compõem o caminho, ou NIL."
  ; Gera a lista de nós sucessores, gerados pelo nó passado como argumento, através dos operadores
  (cond ((null no-inicial) (error "Nó inicial não pode ser nulo"))
        ; Lista de nós abertos juntamente com os nós fechados
        (t (let* ((sucessores-gerados (remove-if (lambda (suc) (no-existp suc fechados 'bfs)) (funcall funcao-sucessores no-inicial operadores 'bfs)))
                  ; Gera a lista de nós que são solução
                  (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-gerados))))
                  ; Gera a lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos) adicionados
                  (abertos-novo (abertos-bfs abertos sucessores-gerados)))
              (let ((nos-expandidos-novo (1+ nos-expandidos))
                    (nos-gerados-novo (+ nos-gerados (length sucessores-gerados))))
                (cond 
                  ; Verifica se o nó inicial é solução, se for retorna-o
                  ((funcall objetivop no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) (ramificacao-media no-inicial nos-gerados) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                  ; Verifica se a lista de nós abertos é nula, se for retorna NIL (ou se o número de nós gerados for superior a 1900)
                  ((null abertos-novo) (list nil nos-expandidos-novo nos-gerados-novo 0 0 (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                  ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
                  ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) (ramificacao-media (car solucao) nos-gerados-novo) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                  ; Aplica recursividade para continuar a procurar
                  (t (bfs (car abertos-novo) objetivop funcao-sucessores operadores nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados (list no-inicial)) tempo-inicial))
                )
              )
            )
        )
  )
)
```

### 1.2. Algoritmo de Procura em Profundidade - Depth-First Search (DFS)

```lisp
;; Algoritmo de procura em profundidade
(defun dfs (no-inicial objetivop funcao-sucessores operadores profundidade-max &optional (nos-expandidos 0) (nos-gerados 0) abertos fechados (tempo-inicial (get-internal-real-time)))
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores. Retorna uma lista com os nós que compõem o caminho, ou NIL."
  (cond ((null no-inicial) (error "Nó inicial não pode ser nulo"))
        ; Lista de nós abertos juntamente com os nós fechados
        (t (let* ((abertos-fechados (append abertos fechados))
                  ; Lista de nós sucessores gerados pelo nó passado como argumento através dos operadores
                  (sucessores-gerados (remove-if (lambda (suc) (no-existp suc abertos-fechados 'dfs)) (funcall funcao-sucessores no-inicial operadores 'dfs profundidade-max)))
                  ; Lista de nós que são solução
                  (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-gerados))))
                  ; Lista de nós abertos com as profundidades recalculadas
                  (abertos-recalculados (recalcular-profundidade sucessores-gerados abertos))
                  ; Lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos e fechados) adicionados
                  (abertos-novo (abertos-dfs abertos-recalculados sucessores-gerados))
                  ; Lista de nós fechados com as profundidades recalculadas
                  (fechados-recalculados (recalcular-profundidade sucessores-gerados fechados)))
              (let ((nos-expandidos-novo (1+ nos-expandidos))
                    (nos-gerados-novo (+ nos-gerados (length sucessores-gerados))))
                (cond 
                  ; Verifica se o nó inicial é solução, se for retorna-o
                  ((funcall objetivop no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) (ramificacao-media no-inicial nos-gerados) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                  ; Verifica se a lista de nós abertos é nula, se for retorna NIL (ou se o número de nós gerados for superior a 1900)
                  ((null abertos-novo) (list nil nos-expandidos-novo nos-gerados-novo 0 0 (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                  ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
                  ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) (ramificacao-media (car solucao) nos-gerados-novo) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                  ; Aplica recursividade para continuar a procurar
                  (t (dfs (car abertos-novo) objetivop funcao-sucessores operadores profundidade-max nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados-recalculados (list no-inicial)) tempo-inicial))
                )
              )
            )
        )
  )
)
```

### 1.3. Algoritmo de Procura A*

```lisp
;; Algoritmo de procura A*
(defun aestrela (no-inicial objetivop funcao-sucessores funcao-heuristica operadores &optional (nos-expandidos 0) (nos-gerados 0) abertos fechados (tempo-inicial (get-internal-real-time)))
  "Implementação do algoritmo de procura A*. Recebe o nó inicial, o objetivo de pontuação, a função que calcula a heurística, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
  (cond ((null no-inicial) (error "Nó inicial não pode ser nulo"))
        ; Lista de nós sucessores gerados pelo nó passado como argumento através dos operadores
        (t (let* ((sucessores-gerados (remove-if (lambda (suc) (no-existp suc (append abertos fechados) 'aestrela)) (funcall funcao-sucessores no-inicial operadores 'aestrela 0 funcao-heuristica)))
                  ; Lista de nós que são solução
                  (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-gerados))))
                  ; Lista de nós abertos com as profundidades recalculadas
                  (abertos-recalculados (recalcular-profundidade sucessores-gerados abertos))
                  ; Lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos e fechados) adicionados
                  (abertos-novo (colocar-sucessores-em-abertos abertos-recalculados sucessores-gerados))
                  ; Lista de nós fechados com as profundidades recalculadas
                  (fechados-recalculados (recalcular-profundidade sucessores-gerados fechados)))
                  ; TODO: Adicionar nós de fechados, que foram recalculados, a abertos
              (let ((nos-expandidos-novo (1+ nos-expandidos))
                    (nos-gerados-novo (+ nos-gerados (length sucessores-gerados))))
                (cond 
                  ; Verifica se o nó inicial é solução, se for retorna-o
                  ((funcall objetivop no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) (ramificacao-media no-inicial nos-gerados) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                  ; Verifica se a lista de nós abertos é nula, se for retorna NIL
                  ((null abertos-novo) (list nil nos-expandidos-novo nos-gerados-novo 0 0 (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                  ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
                  ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) (ramificacao-media (car solucao) nos-gerados-novo) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                  ; Aplica recursividade para continuar a procurar
                  (t (aestrela-loop (car abertos-novo) objetivop funcao-sucessores funcao-heuristica operadores nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados-recalculados (list no-inicial)) tempo-inicial))
                )
              )
            )
        )
  )
)
```

### 2. Descrição dos objetos que compõem o projeto, incluindo dados e procedimentos

#### Dados

* Nó - Objeto base do projeto; Consiste numa lista que contém o tabuleiro, o custo e o nó pai (antecessor); Neste contexto, é o estado atual do jogo;

* Operador - Consiste numa operação a aplicar a um nó; Neste contexto está relacionado com uma operação intermédia necessária para chegar ao estado pretendido, associado à jogada efetuada;

* Sucessores - Consiste numa lista que contém os nós sucessores de um determinado nó, após a aplicação de um operador; Neste contexto, diz respeito a todas as casas disponíveis para a jogada seguinte;

* Lista de abertos - Consiste numa lista que contém os nós que ainda não foram explorados;

* Lista de fechados - Consiste numa lista que contém os nós que já foram explorados;

* Nó solução - Consiste no nó que contém a lista com os nós que compõem a solução (caminho desde o nó inicial até ao nó solução) do problema.

#### Procedimentos

* Aplicar operador aos nós sucessores do nó atual, que se encontram na lista de abertos, expandindo-os;

* Colocar nó expandido na lista de fechados, verificando se algum dos nós que se encontram nessa lista é solução;

* Repetir os dois passos acima até à lista de abertos estar vazia;

* O programa termina quando encontrar a solução, ou quando a lista de abertos estiver vazia, encontrando ou não a solução.

### 3. Limitações e opções técnicas

Em termos de limitações, a principal que foi encontrada foi o facto do IDE utilizado, LispWorks, na versão grátis, ter um limite de memória definido para os programas.

### 4. Análise critica dos resultados das execuções do programa, transparecendo a compreensão das limitações do projeto

Devido à limitação mencionada no ponto anterior, alguns algoritmos não conseguem encontrar solução para determinados problemas, devido a gerarem demasiados nós, ultrapassando o limite de memória disponível e dando "crash" ao programa.

### 5. Análise comparativa do conjunto de execuções do programa para cada algoritmo e cada problema, permitindo verificar o desempenho de cada algoritmo e das heurísticas

#### Algoritmo BFS

| | Encontrou solução? (Sim/Não e porquê) | Nós expandidos | Nós gerados | Penetrância | Fator de Ramificação Média | Tempo de Execução |
| :--------: | :-: | :-: | :-: | :-: | :-: | :-: |
| Problema A | Sim | 6 | 8 | 0.375 (37.5 %) | 1.578 (157.8%) | 0.010000 seg. |
| Problema B | Sim | 43 | 46 | 0.174 (17.4%) | 1.389 (138.9%) | 0.074000 seg. |
| Problema C | Sim | 32 | 39 | 0.154 (15.4%) | 1.574 (157.4%) | 0.055000 seg. |
| Problema D | Sim | 267 | 279 | 0.043 (4.3%) | 1.452 (145.2%) | 0.682000 seg. |
| Problema E | Não, uma vez que gera demasiados nós, ultrapassando o limite de memória disponível | - | - | - | - | - |
| Problema F |  |  |  |  |  |  |

#### Algoritmo DFS

| | Encontrou solução? (Sim/Não e porquê) | Nós expandidos | Nós gerados | Penetrância | Fator de Ramificação Média | Tempo de Execução |
| :--------: | :-: | :-: | :-: | :-: | :-: | :-: |
| Problema A | Sim | 6 | 7 | 0.429 (42.9%) | 1.488 (148.8%) | 0.009000 seg. |
| Problema B | Sim | 10 | 14 | 0.714 (71.4%) | 1.060 (106.0%) | 0.023000 seg. |
| Problema C | Sim | 8 | 16 | 0.375 (37.5%) | 1.289 (128.9%) | 0.020000 seg. |
| Problema D | Sim | 36 | 46 | 0.283 (28.3%) | 1.170 (117.0%) | 0.106000 seg. |
| Problema E | Não, uma vez que gera demasiados nós, ultrapassando o limite de memória disponível | - | - | - | - | - |
| Problema F |  |  |  |  |  |  |

#### Algoritmo A*

#### Heurísticas

* Heurística Base - Privilegia visitar as casas com o maior número de pontos.
  * Segue a fórmula h(x) = o(x)/m(x), em que:
    * x é o tabuleiro;
    * m(x) é a média por casa dos pontos que constam no tabuleiro x;
    * o(x) é o número de pontos que faltam para atingir o valor definido como objetivo.

### Resultados obtidos com a Heurística Base

| | Encontrou solução? (Sim/Não e porquê) | Nós expandidos | Nós gerados | Penetrância | Fator de Ramificação Média | Tempo de Execução |
| :--------: | :-: | :-: | :-: | :-: | :-: | :-: |
| Problema A | Sim | 4 | 6 | 0.500 (50.0%) | 1.389 (138.9%) | 0.008000 seg. |
| Problema B | Sim | 32 | 39 | 0.205 (20.5%) | 1.352 (135.2%) | 0.105000 seg. |
| Problema C | Sim | 32 | 39 | 0.154 (15.4%) | 1.574 (1157.4%) | 0.87000 seg. |
| Problema D | Sim | 61 | 87 | 0.138 (13.8%) | 1.285 (128.5%) | 1.518000 seg. |
| Problema E |  |  |  |  |  |  |
| Problema F |  |  |  |  |  |  |

### 6. Lista dos requisitos do projeto (listados no enunciado) que não foram implementados

* Implementação do Algoritmo RBFS;

* Implementação do Algoritmo IDA*;

* Implementação do Algoritmo SMA*;
