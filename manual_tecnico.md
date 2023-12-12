# **Manual Técnico**

## Inteligência Artificial | Projeto 1 - Jogo do Cavalo | André Meseiro 202100225 e Pedro Anjos 202100230

### 1. Algoritmo geral, por partes e devidamente comentado

### 1.1. Algoritmo Breadth-First Search Completo

```lisp
;; Algoritmo de procura em largura
(defun bfs (no-inicial objetivo funcao-sucessores operadores &optional abertos fechados (tempo-inicial (get-internal-real-time)))
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
  ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((null no-inicial) (error "Nó inicial não pode ser nulo"))
        ((= (no-profundidade no-inicial) 0)
          (cond ((not (cavalo-colocado-p (no-estado no-inicial)))
                  ; Se o cavalo ainda não foi colocado, geram-se os sucessores do nó inicial
                  (let ((sucessores-no-inicial (funcall funcao-sucessores no-inicial (car operadores) 'bfs)))
                  ; Se não há sucessores, retorna uma lista com informações sobre a execução
                    (cond ((null sucessores-no-inicial) (list nil 1 0 0 0 (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                          (t (bfs (car sucessores-no-inicial) objetivo funcao-sucessores (cdr operadores) (cdr sucessores-no-inicial) (append fechados (list no-inicial)) tempo-inicial))
                    )
                  )
                )
                (t (bfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores (cdr operadores) (length fechados) (1+ (length abertos)) abertos fechados tempo-inicial))
          )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (bfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores operadores (length fechados) (1+ (length abertos)) abertos fechados tempo-inicial))
  )
)

;; Função auxiliar que implementa o algoritmo de procura em largura
(defun bfs-loop (no-inicial objetivop funcao-sucessores operadores nos-expandidos nos-gerados abertos fechados tempo-inicial)
  "Função auxiliar para o algoritmo de procura em largura"
         ; Gera a lista de nós sucessores, gerados pelo nó passado como argumento, através dos operadores
  (let* ((sucessores-gerados (remove-if (lambda (suc) (no-existp suc fechados 'bfs)) (funcall funcao-sucessores no-inicial operadores 'bfs)))
         ; Gera a lista de nós que são solução
         (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-gerados))))
         ; Gera a lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos) adicionados
         (abertos-novo (abertos-bfs abertos sucessores-gerados))
        )
        (let ((nos-expandidos-novo (1+ nos-expandidos))
              (nos-gerados-novo (+ nos-gerados (length sucessores-gerados)))
             )
          (cond 
            ; Verifica se o nó inicial é solução, se for retorna-o
            ((funcall objetivop no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) (ramificacao-media no-inicial nos-gerados) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL (ou se o número de nós gerados for superior a 1900)
            ((null abertos-novo) (list nil nos-expandidos-novo nos-gerados-novo 0 0 (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) (ramificacao-media (car solucao) nos-gerados-novo) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
            ; Aplica recursividade para continuar a procurar
            (t (bfs-loop (car abertos-novo) objetivop funcao-sucessores operadores nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados (list no-inicial)) tempo-inicial))
          )
        )
  )
)
```

### 1.1.1. Função Principal do Algoritmo BFS

```lisp
;; Algoritmo de procura em largura
(defun bfs (no-inicial objetivo funcao-sucessores operadores &optional abertos fechados (tempo-inicial (get-internal-real-time)))
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
  ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((null no-inicial) (error "Nó inicial não pode ser nulo"))
        ((= (no-profundidade no-inicial) 0)
          (cond ((not (cavalo-colocado-p (no-estado no-inicial)))
                  ; Se o cavalo ainda não foi colocado, geram-se os sucessores do nó inicial
                  (let ((sucessores-no-inicial (funcall funcao-sucessores no-inicial (car operadores) 'bfs)))
                  ; Se não há sucessores, retorna uma lista com informações sobre a execução
                    (cond ((null sucessores-no-inicial) (list nil 1 0 0 0 (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                          (t (bfs (car sucessores-no-inicial) objetivo funcao-sucessores (cdr operadores) (cdr sucessores-no-inicial) (append fechados (list no-inicial)) tempo-inicial))
                    )
                  )
                )
                (t (bfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores (cdr operadores) (length fechados) (1+ (length abertos)) abertos fechados tempo-inicial))
          )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (bfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores operadores (length fechados) (1+ (length abertos)) abertos fechados tempo-inicial))
  )
)
```

### 1.1.2. Função Auxiliar do Algoritmo BFS

```lisp
;; Função auxiliar que implementa o algoritmo de procura em largura
(defun bfs-loop (no-inicial objetivop funcao-sucessores operadores nos-expandidos nos-gerados abertos fechados tempo-inicial)
  "Função auxiliar para o algoritmo de procura em largura"
         ; Gera a lista de nós sucessores, gerados pelo nó passado como argumento, através dos operadores
  (let* ((sucessores-gerados (remove-if (lambda (suc) (no-existp suc fechados 'bfs)) (funcall funcao-sucessores no-inicial operadores 'bfs)))
         ; Gera a lista de nós que são solução
         (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-gerados))))
         ; Gera a lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos) adicionados
         (abertos-novo (abertos-bfs abertos sucessores-gerados))
        )
        (let ((nos-expandidos-novo (1+ nos-expandidos))
              (nos-gerados-novo (+ nos-gerados (length sucessores-gerados)))
             )
          (cond 
            ; Verifica se o nó inicial é solução, se for retorna-o
            ((funcall objetivop no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) (ramificacao-media no-inicial nos-gerados) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL (ou se o número de nós gerados for superior a 1900)
            ((null abertos-novo) (list nil nos-expandidos-novo nos-gerados-novo 0 0 (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) (ramificacao-media (car solucao) nos-gerados-novo) (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
            ; Aplica recursividade para continuar a procurar
            (t (bfs-loop (car abertos-novo) objetivop funcao-sucessores operadores nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados (list no-inicial)) tempo-inicial))
          )
        )
  )
)
```

### 1.2. Algoritmo Depth-First Search Completo

```lisp
;; Algoritmo de procura em profundidade
(defun dfs (no-inicial objetivo funcao-sucessores operadores profundidade-max &optional abertos fechados (tempo-inicial (get-internal-real-time)))
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
         ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((null no-inicial) (error "Nó inicial não pode ser nulo"))
        ((= (no-profundidade no-inicial) 0)
          (cond ((not (cavalo-colocado-p (no-estado no-inicial)))
                  ; Se o cavalo ainda não foi colocado, geram-se os sucessores do nó inicial
                  (let ((sucessores-no-inicial (sucessores no-inicial (car operadores) 'dfs profundidade-max)))
                    ; Se não há sucessores, retorna uma lista com informações sobre a execução
                    (cond ((null sucessores-no-inicial) (list nil 1 0 0 0 (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                          (t (dfs (car sucessores-no-inicial) objetivo funcao-sucessores (cdr operadores) profundidade-max (cdr sucessores-no-inicial) (append fechados (list no-inicial)) tempo-inicial))
                    )
                  )
                )
                (t (dfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores (cdr operadores) profundidade-max (length fechados) (1+ (length abertos)) abertos fechados tempo-inicial))
          )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (dfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores operadores profundidade-max (length fechados) (1+ (length abertos)) abertos fechados tempo-inicial))
  )
)

;; Função auxiliar que implementa o algoritmo de procura em profundidade
(defun dfs-loop (no-inicial objetivop funcao-sucessores operadores profundidade-max nos-expandidos nos-gerados abertos fechados tempo-inicial)
  "Função auxiliar para o algoritmo de procura em profundidade"
         ; Lista de nós abertos juntamente com os nós fechados
  (let* ((abertos-fechados (append abertos fechados))
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
        (t (dfs-loop (car abertos-novo) objetivop funcao-sucessores operadores profundidade-max nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados-recalculados (list no-inicial)) tempo-inicial))
      )
    )
  )
)
```

### 1.2.1. Função Principal do Algoritmo DFS

```lisp
;; Algoritmo de procura em profundidade
(defun dfs (no-inicial objetivo funcao-sucessores operadores profundidade-max &optional abertos fechados (tempo-inicial (get-internal-real-time)))
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
         ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((null no-inicial) (error "Nó inicial não pode ser nulo"))
        ((= (no-profundidade no-inicial) 0)
          (cond ((not (cavalo-colocado-p (no-estado no-inicial)))
                  ; Se o cavalo ainda não foi colocado, geram-se os sucessores do nó inicial
                  (let ((sucessores-no-inicial (sucessores no-inicial (car operadores) 'dfs profundidade-max)))
                    ; Se não há sucessores, retorna uma lista com informações sobre a execução
                    (cond ((null sucessores-no-inicial) (list nil 1 0 0 0 (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))
                          (t (dfs (car sucessores-no-inicial) objetivo funcao-sucessores (cdr operadores) profundidade-max (cdr sucessores-no-inicial) (append fechados (list no-inicial)) tempo-inicial))
                    )
                  )
                )
                (t (dfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores (cdr operadores) profundidade-max (length fechados) (1+ (length abertos)) abertos fechados tempo-inicial))
          )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (dfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores operadores profundidade-max (length fechados) (1+ (length abertos)) abertos fechados tempo-inicial))
  )
)
```

### 1.2.2. Função Auxiliar do Algoritmo DFS

```lisp
;; Função auxiliar que implementa o algoritmo de procura em profundidade
(defun dfs-loop (no-inicial objetivop funcao-sucessores operadores profundidade-max nos-expandidos nos-gerados abertos fechados tempo-inicial)
  "Função auxiliar para o algoritmo de procura em profundidade"
         ; Lista de nós abertos juntamente com os nós fechados
  (let* ((abertos-fechados (append abertos fechados))
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
        (t (dfs-loop (car abertos-novo) objetivop funcao-sucessores operadores profundidade-max nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados-recalculados (list no-inicial)) tempo-inicial))
      )
    )
  )
)
```

### 1.3. Algoritmo A* Completo

### 1.3.1. Função Principal do Algoritmo A*

### 1.3.2. Função Auxiliar do Algoritmo A*

### 2. Descrição dos objetos que compõem o projeto, incluindo dados e procedimentos

#### Dados

* Nó - Objeto base do projeto; Consiste numa lista que contém o tabuleiro, o custo e o nó pai (antecessor); Neste contexto, é o estado atual do jogo;

* Operador - Consiste numa operação a aplicar a um nó; Neste contexto está relacionado com uma operação intermédia necessária para chegar ao estado pretendido, associado à jogada efetuada;

* Sucessores - Consiste numa lista que contém os nós sucessores de um determinado nó, após a aplicação de um operador; Neste contexto, diz respeito a todas as casas disponíveis para a jogada seguinte;

* Lista de abertos - Consiste numa lista que contém os nós que ainda não foram explorados;

* Lista de fechados - Consiste numa lista que contém os nós que já foram explorados;

* Solução - Consiste numa lista que contém os nós que compõem a solução (caminho com os nós) para o problema.

#### Procedimentos

* Aplicar operador aos nós sucessores do nó atual, que se encontram na lista de abertos, expandindo-os;

* Colocar nó expandido na lista de fechados, verificando se algum dos nós que se encontram nessa lista é solução;

* Repetir os dois passos acima até à lista de abertos estar vazia;

* O programa termina quando encontrar a solução, ou quando a lista de abertos estiver vazia, encontrando ou não a solução.

### 3. Limitações e opções técnicas



### 4. Análise critica dos resultados das execuções do programa, transparecendo a compreensão das limitações do projeto



### 5. Análise comparativa do conjunto de execuções do programa para cada algoritmo e cada problema, permitindo verificar o desempenho de cada algoritmo e das heurísticas

| | Algoritmo BFS encontrou solução? (Sim/Não e porquê) | Algoritmo DFS encontrou solução? (Sim/Não e porquê) | Algoritmo A* encontrou solução? (Sim/Não e porquê) | Nós expandidos | Nós gerados | Penetrância | Fator de Ramificação Média | Tempo de Execução |
| :--------: | :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: |
| Problema A | | | | | | | | |
| Problema B | | | | | | | | |
| Problema C | | | | | | | | |
| Problema D | | | | | | | | |
| Problema E | | | | | | | | |
| Problema F | | | | | | | | |
| Problema G | ? | ? | ? | ? | ? | ? | ? | ? |

### 6. Lista dos requisitos do projeto (listados no enunciado) que não foram implementados

* Implementação do Algoritmo RBFS;

* Implementação do Algoritmo IDA*;

* Implementação do Algoritmo SMA*;
