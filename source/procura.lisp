;;;; procura.lisp
;;;; Implementa os algoritmos de procura.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro

;;;; Algoritmo de procura em largura e profundidade (BFS e DFS) e A*

;;; Construtor

;; Cria um nó com o tabuleiro, o custo e o nó pai
(defun cria-no (tabuleiro &optional (g 0) (h 0) (pai nil) (pontuacao 0))
  "Cria um nó com o tabuleiro, o custo e o nó pai"
  (list tabuleiro (+ g h) g h pai pontuacao)
)

;;; Seletores

;; Retorna o tabuleiro do nó dado como argumento
(defun no-estado (no)
  "Retorna o tabuleiro do nó dado como argumento"
  (first no)
)

;; Retorna o custo total do nó dado como argumento
(defun no-custo (no)
  "Retorna o custo total do nó dado como argumento"
  (second no)
)

;; Retorna o custo do nó dado como argumento
(defun no-profundidade (no)
  "Retorna o custo do nó dado como argumento"
  (third no)
)

;; Retorna a heurística do nó dado como argumento
(defun no-heuristica (no)
  "Retorna a heurística do nó dado como argumento"
  (fourth no)
)

;; Retorna o nó pai do nó dado como argumento
(defun no-pai (no)
  "Retorna o nó pai do nó dado como argumento"
  (fifth no)
)

;; Retorna a pontuação do nó dado como argumento
(defun no-pontuacao (no)
  "Retorna a pontuação do nó dado como argumento"
  (sixth no)
)

;; Função que retorna as posições percorridas pelo cavalo
(defun no-estados-posicoes (no) 
  "Função que retorna as posições percorridas pelo cavalo"
  (cond ((null no) nil)
        (t (let ((posicao (posicao-cavalo (no-estado no))))
             (cond 
              ((null posicao) (no-estados-posicoes (no-pai no)))
              (t (append (list (list (first posicao) (second posicao))) (no-estados-posicoes (no-pai no))))
             )
           )
        )
  )
)

;; Função que retorna o caminho percorrido pelo cavalo
(defun no-caminho (no &optional fim-inicio)
  "Função que retorna o caminho percorrido pelo cavalo"
  (cond ((null no) nil)
        (fim-inicio (no-estados-posicoes no))
        (t (reverse (no-estados-posicoes no)))
  )
)

;;; Sucessores

;; Função que retorna o sucessor de um nó com o operador dado como argumento
(defun novo-sucessor (no operador)
  "Função que retorna o sucessor de um nó com o operador dado como argumento"
  (let ((estado-gerado (funcall operador (no-estado no)))
        (g (1+ (no-profundidade no)))
        (h 0))
    (cond
     ((null estado-gerado) nil)
     (t (let* ((posicao-destino (posicao-cavalo estado-gerado))
              (valor-destino (celula (first posicao-destino) (second posicao-destino) (no-estado no))))
          (cria-no estado-gerado g h no (+ (no-pontuacao no) valor-destino))
        )
     )
    )
  )
)

;; Função que retorna a lista de sucessores de um nó
(defun sucessores (no operadores algoritmo &optional (profundidade-max 0))
  "Função que retorna a lista de sucessores de um nó"
  (cond ((>= (no-profundidade no) 8) nil)
        ((and (equal algoritmo 'dfs) (>= (no-profundidade no) profundidade-max)) nil)
        (t (mapcar (lambda (op) (novo-sucessor no op)) operadores))
  )
)

;; Função que retorna a lista de sucessores de um nó após a colocação inicial do cavalo
(defun sucessores-iniciais (no &optional (i 0))
  "Função que retorna a lista de sucessores de um nó após a colocação inicial do cavalo"
  (cond ((>= i (length (car (no-estado no)))) nil) 
        (t (let* ((estado-gerado (colocar-cavalo (no-estado no) i))) 
            (cond ((null estado-gerado) (sucessores-iniciais no (1+ i)))
                  (t 
                    (let* ((posicao-destino (posicao-cavalo estado-gerado))
                          (valor-destino (celula (first posicao-destino) (second posicao-destino) (no-estado no))))   
                      (append (list (cria-no estado-gerado (1+ (no-profundidade no)) 0 no (+ (no-pontuacao no) valor-destino))) (sucessores-iniciais no (1+ i)))
                    )
                  )
            )
          )
        ))
)

;;; Funções auxiliares e de ordenação de nós

;; Função que adiciona os nós sucessores à lista de nós abertos,
;; inserindo-os no fim da lista
(defun abertos-bfs (lista-abertos lista-sucessores)
  "Função que adiciona os nós sucessores à lista de nós abertos (bfs)"
  (append lista-abertos lista-sucessores)
)

;; Função que adiciona os nós sucessores à lista de nós abertos,
;; inserindo-os no início da lista
(defun abertos-dfs (lista-abertos lista-sucessores)
  "Função que adiciona os nós sucessores à lista de nós abertos (dfs)"
  (append lista-sucessores lista-abertos)
)

;; Função que verifica se um nó existe numa lista de nós
(defun no-existp (no lista algoritmo)
  "Função que verifica se um nó existe numa lista de nós"
  (cond
   ((or (null no) (null lista)) nil)
   ((equal algoritmo 'dfs) 
      (cond ((equal (no-estado no) (no-estado (car lista))) 
                (cond ((>= (no-profundidade no) (no-profundidade (car lista))) t)
                      (t nil)
                )
            )
            (t (no-existp no (cdr lista) algoritmo))))
   (t (cond ((equal (no-estado no) (no-estado (car lista))))
            (t (no-existp no (cdr lista) algoritmo)))
    )
  )
)

;;; Algoritmos de procura

;; Algoritmo de procura em largura
(defun bfs (no-inicial objetivop sucessores operadores &optional abertos fechados)
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
        ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((= (no-profundidade no-inicial) 0) 
         (let ((sucessores-no-inicial (sucessores-iniciais no-inicial)))
          (bfs (car sucessores-no-inicial) objetivop sucessores operadores (cdr sucessores-no-inicial) (append fechados (list no-inicial)))
         )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (bfs-loop no-inicial objetivop sucessores operadores abertos fechados))
  )
)

;; Função auxiliar que implementa o algoritmo de procura em largura
(defun bfs-loop (no-inicial objetivop sucessores operadores &optional abertos fechados)
  "Função auxiliar para o algoritmo de procura em largura"
        ; Gera a lista de nós sucessores, gerados pelo nó passado como argumento, através dos operadores
  (let* ((sucessores-gerados (funcall sucessores no-inicial operadores 'bfs))
         ; Gera a lista que consiste na junção das listas de nós abertos
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

;; Algoritmo de procura em profundidade
(defun dfs (no-inicial objetivop sucessores operadores profundidade-max &optional abertos fechados)
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
        ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((= (no-profundidade no-inicial) 0) 
         (let ((sucessores-no-inicial (sucessores-iniciais no-inicial)))
          (dfs (car sucessores-no-inicial) objetivop sucessores operadores profundidade-max (cdr sucessores-no-inicial) (append fechados (list no-inicial)))
         )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (dfs-loop no-inicial objetivop sucessores operadores profundidade-max abertos fechados))
  )
)

;; Função auxiliar que implementa o algoritmo de procura em profundidade
(defun dfs-loop (no-inicial objetivop sucessores operadores profundidade-max &optional abertos fechados)
  "Função auxiliar para o algoritmo de procura em profundidade"
         ; Lista de nós sucessores gerados pelo nó passado como argumento através dos operadores
  (let* ((sucessores-gerados (funcall sucessores no-inicial operadores 'dfs profundidade-max))
         ; Lista de nós abertos juntamente com os nós fechados
         (abertos-fechados (append abertos fechados))
         ; Lista de nós sucessores que não existem na lista de nós abertos e fechados
         (sucessores-abertos (apply #'append (mapcar (lambda (suc) (cond ((not (or (no-existp suc abertos-fechados 'dfs) (null (no-estado suc)))) (list suc))))sucessores-gerados)))
         ; Lista de nós que são solução
         (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-abertos))))
         ; Lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos e fechados) adicionados
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

;;; Escrita de soluções

;; Função que escreve o caminho percorrido
(defun escreve-caminho (caminho)
  "Função que escreve o caminho percorrido"
  (cond ((null caminho) nil)
        ((= (length caminho) 1) (escreve-posicao (car caminho)))
        (t (progn (escreve-posicao (car caminho)) (format t "->") (escreve-caminho (cdr caminho))))     
  )
)