# **Manual Técnico**

## Inteligência Artificial | Projeto 1 - Jogo do Cavalo | André Meseiro 202100225 e Pedro Anjos 202100230

### 1. Algoritmo geral, por partes e devidamente comentado

### 1.1. Algoritmo Breadth-First Search Completo

```lisp
;; Algoritmo de procura em largura
(defun bfs (no-inicial objetivop sucessores operadores &optional abertos fechados)
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
  ; Caso base: primeira chamada da função; a profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((= (no-profundidade no-inicial) 0) 
         (let ((sucessores-no-inicial (sucessores-iniciais no-inicial)))
          (bfs (car sucessores-no-inicial) objetivop sucessores operadores (cdr sucessores-no-inicial) (append fechados (list no-inicial)))
         )
        )
        ; Chamada recursiva para a próxima profundidade com o primeiro sucessor
        (t (bfs-loop no-inicial objetivop sucessores operadores abertos fechados))
  )
)

;; Função auxiliar que implementa o algoritmo de procura em largura
(defun bfs-loop (no-inicial objetivop sucessores operadores &optional abertos fechados)
  "Função auxiliar para o algoritmo de procura em largura"
  ; Gera a lista de nós sucessores, gerados pelo nó passado como argumento, através dos operadores
  (let* ((sucessores-gerados (funcall sucessores no-inicial operadores 'bfs))
         ; Gera a lista de nós abertos
         (sucessores-abertos (apply #'append (mapcar (lambda (suc) (cond ((not (or (no-existp suc abertos 'dfs) (null (no-estado suc)))) (list suc)))) sucessores-gerados)))
         ; Gera a lista de nós que são solução
         (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-abertos))))
         ; Gera a lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos) adicionados
         (abertos-novo (abertos-dfs abertos sucessores-abertos))
        )
          (cond 
            ; Verifica se o nó inicial é solução, se for retorna-o
            ((funcall objetivop no-inicial) no-inicial)
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL
            ((null abertos-novo) nil)
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (car solucao))
            ; Aplica recursividade para continuar a procurar
            (t (bfs-loop (car abertos-novo) objetivop sucessores operadores (cdr abertos-novo) (append fechados (list no-inicial))))
          )
  )
)
```

### 1.1.1. Função Principal do Algoritmo DFS

```lisp
;; Algoritmo de procura em largura
(defun bfs (no-inicial objetivop sucessores operadores &optional abertos fechados)
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
  ; Caso base: primeira chamada da função; a profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((= (no-profundidade no-inicial) 0) 
         (let ((sucessores-no-inicial (sucessores-iniciais no-inicial)))
          (bfs (car sucessores-no-inicial) objetivop sucessores operadores (cdr sucessores-no-inicial) (append fechados (list no-inicial)))
         )
        )
        ; Chamada recursiva para a próxima profundidade com o primeiro sucessor
        (t (bfs-loop no-inicial objetivop sucessores operadores abertos fechados))
  )
)
```

### 1.1.2. Função Auxiliar do Algoritmo DFS

```lisp
;; Função auxiliar que implementa o algoritmo de procura em largura
(defun bfs-loop (no-inicial objetivop sucessores operadores &optional abertos fechados)
  "Função auxiliar para o algoritmo de procura em largura"
  ; Gera a lista de nós sucessores, gerados pelo nó passado como argumento, através dos operadores
  (let* ((sucessores-gerados (funcall sucessores no-inicial operadores 'bfs))
         ; Gera a lista de nós abertos
         (sucessores-abertos (apply #'append (mapcar (lambda (suc) (cond ((not (or (no-existp suc abertos 'dfs) (null (no-estado suc)))) (list suc)))) sucessores-gerados)))
         ; Gera a lista de nós que são solução
         (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-abertos))))
         ; Gera a lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos) adicionados
         (abertos-novo (abertos-dfs abertos sucessores-abertos))
        )
          (cond 
            ; Verifica se o nó inicial é solução, se for retorna-o
            ((funcall objetivop no-inicial) no-inicial)
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL
            ((null abertos-novo) nil)
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (car solucao))
            ; Aplica recursividade para continuar a procurar
            (t (bfs-loop (car abertos-novo) objetivop sucessores operadores (cdr abertos-novo) (append fechados (list no-inicial))))
          )
  )
)
```

### 1.2. Algoritmo Depth-First Search Completo

```lisp
;; Algoritmo de procura em profundidade
(defun dfs (no-inicial objetivop sucessores operadores profundidade-max &optional abertos fechados)
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
  ; Caso base: primeira chamada da função; a profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((= (no-profundidade no-inicial) 0) 
         (let ((sucessores-no-inicial (sucessores-iniciais no-inicial)))
         ; Chamada recursiva para a próxima profundidade com o primeiro sucessor
          (dfs (car sucessores-no-inicial) objetivop sucessores operadores profundidade-max (cdr sucessores-no-inicial) (append fechados (list no-inicial)))
         )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (dfs-loop no-inicial objetivop sucessores operadores profundidade-max abertos fechados))
  )
)

;; Função auxiliar que implementa o algoritmo de procura em profundidade
(defun dfs-loop (no-inicial objetivop sucessores operadores profundidade-max &optional abertos fechados)
  "Função auxiliar para o algoritmo de procura em profundidade. Recebe o nó inicial, o objetivo da pontuação, a lista de nós sucessores, a lista de operadores, a profundidade máxima e como parâmetros opcionais a lista de abertos e fechados."
  ; Gera a lista de nós sucessores, gerados pelo nó passado como argumento, através dos operadores
  (let* ((sucessores-gerados (funcall sucessores no-inicial operadores 'dfs profundidade-max))
         ; Gera a lista que consiste na junção das listas de nós abertos e fechados
         (abertos-fechados (append abertos fechados))
         ; Gera a lista de nós sucessores que não existem na lista de nós abertos e fechados
         (sucessores-abertos (apply #'append (mapcar (lambda (suc) (cond ((not (or (no-existp suc abertos-fechados 'dfs) (null (no-estado suc)))) (list suc))))sucessores-gerados)))
         ; Gera a lista de nós que são solução
         (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-abertos))))
         ; Gera a lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos e fechados) adicionados
         (abertos-novo (abertos-dfs abertos sucessores-abertos))
        )
          (cond 
            ; Verifica se o nó inicial é solução, se for retorna-o
            ((funcall objetivop no-inicial) no-inicial)
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL
            ((null abertos-novo) nil)
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (car solucao))
            ; Aplica recursividade para continuar a procurar
            (t (dfs-loop (car abertos-novo) objetivop sucessores operadores profundidade-max (cdr abertos-novo) (append fechados (list no-inicial))))
          )
  )
)
```

### 1.2.1. Função Principal do Algoritmo DFS

```lisp
;; Algoritmo de procura em profundidade
(defun dfs (no-inicial objetivop sucessores operadores profundidade-max &optional abertos fechados)
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
  ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((= (no-profundidade no-inicial) 0) 
         (let ((sucessores-no-inicial (sucessores-iniciais no-inicial)))
         ; Chamada recursiva para a próxima profundidade com o primeiro sucessor
          (dfs (car sucessores-no-inicial) objetivop sucessores operadores profundidade-max (cdr sucessores-no-inicial) (append fechados (list no-inicial)))
         )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (dfs-loop no-inicial objetivop sucessores operadores profundidade-max abertos fechados))
  )
)
```

### 1.2.2. Função Auxiliar do Algoritmo DFS

```lisp
(defun dfs-loop (no-inicial objetivop sucessores operadores profundidade-max &optional abertos fechados)
  "Função auxiliar para o algoritmo de procura em profundidade. Recebe o nó inicial, o objetivo da pontuação, a lista de nós sucessores, a lista de operadores, a profundidade máxima e como parâmetros opcionais a lista de abertos e fechados."
  ; Gera a lista de nós sucessores, gerados pelo nó passado como argumento, através dos operadores
  (let* ((sucessores-gerados (funcall sucessores no-inicial operadores 'dfs profundidade-max))
         ; Gera a lista que consiste na junção das listas de nós abertos e fechados
         (abertos-fechados (append abertos fechados))
         ; Gera a lista de nós sucessores que não existem na lista de nós abertos e fechados
         (sucessores-abertos (apply #'append (mapcar (lambda (suc) (cond ((not (or (no-existp suc abertos-fechados 'dfs) (null (no-estado suc)))) (list suc))))sucessores-gerados)))
         ; Gera a lista de nós que são solução
         (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-abertos))))
         ; Gera a lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos e fechados) adicionados
         (abertos-novo (abertos-dfs abertos sucessores-abertos))
        )
          (cond 
            ; Verifica se o nó inicial é solução, se for retorna-o
            ((funcall objetivop no-inicial) no-inicial)
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL
            ((null abertos-novo) nil)
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (car solucao))
            ; Aplica recursividade para continuar a procurar
            (t (dfs-loop (car abertos-novo) objetivop sucessores operadores profundidade-max (cdr abertos-novo) (append fechados (list no-inicial))))
          )
  )
)
```

### 2. Descrição dos objetos que compõem o projeto, incluindo dados e procedimentos

* Nó - Objeto base do projeto; Consiste numa lista que contém o tabuleiro, o custo e o nó pai (antecessor); Neste contexto, é o estado atual do jogo;

* Operador - Consiste numa operação a aplicar a um nó; Neste contexto está relacionado com uma operação intermédia necessária para chegar ao estado pretendido, associado à jogada efetuada;

* Sucessores - Consiste numa lista com os nós sucessores de um determinado nó, após a aplicação de um operador; Neste contexto, diz respeito a todas as casas disponíveis para a jogada seguinte;

### 3. Limitações e opções técnicas

### 4. Análise critica dos resultados das execuções do programa, onde deverá transparecer a compreensão das limitações do projeto

### 5. Análise comparativa do conjunto de execuções do programa para cada algoritmo e cada problema, permitindo verificar o desempenho de cada algoritmo e das heurísticas

### 6. Lista dos requisitos do projeto (listados neste documento) que não foram implementados