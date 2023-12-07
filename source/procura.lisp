;;;; procura.lisp
;;;; Implementa os algoritmos de procura.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro

; TODO: Rever comentérios

;;;; Algoritmo de procura em largura e profundidade (BFS e DFS) e A*

;;; Construtor

;; Cria um nó com o tabuleiro, o custo e o nó pai
(defun cria-no (tabuleiro &optional (g 0) (h 0) (pai nil) (pontuacao 0))
  "Cria um nó com o tabuleiro, o custo e o nó pai"
  (list tabuleiro g h pai pontuacao)
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
  (+ (no-profundidade no) (no-heuristica no))
)

;; Retorna o custo do nó dado como argumento
(defun no-profundidade (no)
  "Retorna o custo do nó dado como argumento"
  (second no)
)

;; Retorna a heurística do nó dado como argumento
(defun no-heuristica (no)
  "Retorna a heurística do nó dado como argumento"
  (third no)
)

;; Retorna o nó pai do nó dado como argumento
(defun no-pai (no)
  "Retorna o nó pai do nó dado como argumento"
  (fourth no)
)

;; Retorna a pontuação do nó dado como argumento
(defun no-pontuacao (no)
  "Retorna a pontuação do nó dado como argumento"
  (fifth no)
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
  (let ((estado-gerado (funcall operador (no-estado no))))
    (cond
     ((null estado-gerado) nil)
     (t (let* ((posicao-destino (posicao-cavalo estado-gerado))
               (valor-destino (celula (first posicao-destino) (second posicao-destino) (no-estado no)))
               (pontuacao (+ (no-pontuacao no) valor-destino))
               (g (1+ (no-profundidade no))))
          (cria-no estado-gerado g 0 no pontuacao)
        )
     )
    )
  )
)

;; Função que retorna a lista de sucessores de um nó
(defun sucessores (no operadores algoritmo &optional (profundidade-max 0))
  "Função que retorna a lista de sucessores de um nó"
  (cond ((null no) nil)
        ((and (equal algoritmo 'dfs) (>= (no-profundidade no) profundidade-max)) nil)
        (t (apply #'append (mapcar (lambda (op) 
              (let ((sucessor (novo-sucessor no op)))
                    (cond ((null sucessor) nil)
                          (t (list sucessor))
                    ))
              ) operadores))
        )
  )
)

;; Função que retorna a lista de sucessores de um nó após a colocação inicial do cavalo
; TODO: Remover e tornar a função sucessores genérica
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

;; Função que recebe um valor e retorna uma função lambda
;; que recebe um nó e verifica se a sua pontuação é maior ou igual ao valor dado
(defun cria-objetivo (valor)
  "Função que recebe um valor e retorna uma função lambda que recebe um nó e verifica se a sua pontuação é maior ou igual ao valor dado"
  (list (lambda (no) (>= (no-pontuacao no) valor)) valor)
)

(defun objetivo-funcao (objetivo)
  "Função que retorna a função objetivo de um nó"
  (first objetivo)
)

(defun objetivo-valor (objetivo)
  "Função que retorna o valor objetivo de um nó"
  (second objetivo)
)

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

;; Compara dois nós, retornando o nó com maior pontuação
(defun compara-pontuacao-nos (no1 no2)
  "Compara dois nós, retornando o nó com maior pontuação"
  (cond ((and (null no1) (null no2)) nil)
        ((null no1) no2)
        ((null no2) no1)
        (t (cond ((>= (no-pontuacao no1) (no-pontuacao no2)) no1)
                 (t no2))
        )
  ) 
)

(defun compara-custo-nos (no1 no2)
  "Compara dois nós, retornando o nó com menor custo"
    (cond ((and (null no1) (null no2)) nil)
        ((null no1) no2)
        ((null no2) no1)
        (t (cond ((<= (no-custo no1) (no-custo no2)) no1)
                 (t no2))
        )
  ) 
)

;; Função que retorna o nó com menor custo
(defun no-menor-custo (lista-nos &optional no-min)
  "Função que retorna o nó com menor custo"
  (cond ((null lista-nos) no-min)
        (t (compara-custo-nos (no-menor-custo (cdr lista-nos)) (car lista-nos)))
  )
)

;; Função que retorna a média dos valores fornecidos
(defun media (lista)
  "Função que retorna a média dos valores fornecidos"
  (/ (apply #'+ lista) (length lista))
)

;; Função que ordena a lista de nós por ordem crescente de custo
(defun ordena-nos-custo (lista-nos)
  "Função que ordena a lista de nós por ordem crescente de custo"
  (cond ((null lista-nos) nil)
        (t (cons (no-menor-custo lista-nos) (ordena-nos-custo (remover-se (lambda (no) (equal no (no-menor-custo lista-nos))) lista-nos))))
  )
)

(defun lista-nos-custo-menores-que (lista-nos valor)
  "Função que retorna a lista de nós com o custo menor que o valor dado"
  (cond ((null lista-nos) nil)
        ((>= (no-custo (car lista-nos)) valor) (lista-nos-custo-menores-que (cdr lista-nos) valor))
        (t (cons (car lista-nos) (lista-nos-custo-menores-que (cdr lista-nos) valor)))
  )
)

;;; Heurísticas

;; Função que representa uma heurística base
(defun heuristica-base (tabuleiro objetivo pontuacao-atual)
  "Função que representa uma heurística base"
  (let ((heuristica (/ (- objetivo pontuacao-atual) (media (numeros-tabuleiro tabuleiro)))))
    (cond ((< heuristica 0) 0)
          (t heuristica)
    )
  )
)

(defun aplicar-heuristica (lista-sucessores funcao-heurisica objetivo)
  "Função que aplica uma heurística a uma lista de nós"
  (mapcar (lambda (suc) (let ((h (funcall funcao-heurisica (no-estado suc) objetivo (no-pontuacao suc)))) 
            (cria-no (no-estado suc) (no-profundidade suc) h (no-pai suc) (no-pontuacao suc))))
   lista-sucessores)
)

;;; Algoritmos de procura

;; Algoritmo de procura em largura
(defun bfs (no-inicial objetivo funcao-sucessores operadores &optional abertos fechados)
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
        ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((= (no-profundidade no-inicial) 0) 
         (let ((sucessores-no-inicial (sucessores-iniciais no-inicial)))
          (bfs (car sucessores-no-inicial) objetivo funcao-sucessores operadores (cdr sucessores-no-inicial) (append fechados (list no-inicial)))
         )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (bfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores operadores (length fechados) (1+ (length abertos)) abertos fechados))
  )
)

;; Função auxiliar que implementa o algoritmo de procura em largura
(defun bfs-loop (no-inicial objetivop funcao-sucessores operadores &optional (nos-expandidos 0) (nos-gerados 0) abertos fechados)
  "Função auxiliar para o algoritmo de procura em largura"
         ; Gera a lista de nós sucessores, gerados pelo nó passado como argumento, através dos operadores
  (let* ((sucessores-gerados (remover-se (lambda (suc) (no-existp suc fechados 'bfs)) (funcall funcao-sucessores no-inicial operadores 'bfs)))
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
            ((funcall objetivop no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) 0))
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL
            ((null abertos-novo) (list nil nos-expandidos-novo nos-gerados-novo 0 0))
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) 0))
            ; Aplica recursividade para continuar a procurar
            (t (bfs-loop (car abertos-novo) objetivop funcao-sucessores operadores nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados (list no-inicial))))
          )
        )
  )
)

;; Algoritmo de procura em profundidade
(defun dfs (no-inicial objetivo funcao-sucessores operadores profundidade-max &optional abertos fechados)
  "Implementação do algoritmo de procura em largura. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
         ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((= (no-profundidade no-inicial) 0) 
         (let ((sucessores-no-inicial (sucessores-iniciais no-inicial)))
          (dfs (car sucessores-no-inicial) objetivo funcao-sucessores operadores profundidade-max (cdr sucessores-no-inicial) (append fechados (list no-inicial)))
         )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (dfs-loop no-inicial (objetivo-funcao objetivo) funcao-sucessores operadores profundidade-max (length fechados) (1+ (length abertos)) abertos fechados))
  )
)

;; Função auxiliar que implementa o algoritmo de procura em profundidade
(defun dfs-loop (no-inicial objetivop funcao-sucessores operadores profundidade-max &optional (nos-expandidos 0) (nos-gerados 0) abertos fechados)
  "Função auxiliar para o algoritmo de procura em profundidade"
         ; Lista de nós abertos juntamente com os nós fechados
  (let* ((abertos-fechados (append abertos fechados))
         ; Lista de nós sucessores gerados pelo nó passado como argumento através dos operadores
         (sucessores-gerados (remover-se (lambda (suc) (no-existp suc abertos-fechados 'dfs)) (funcall funcao-sucessores no-inicial operadores 'dfs profundidade-max)))
         ; Lista de nós que são solução
         (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall objetivop suc) suc))) sucessores-gerados))))
         ; Lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos e fechados) adicionados
         (abertos-novo (abertos-dfs abertos sucessores-gerados))
        )
        (let ((nos-expandidos-novo (1+ nos-expandidos))
              (nos-gerados-novo (+ nos-gerados (length sucessores-gerados)))
             )
          (cond 
            ; Verifica se o nó inicial é solução, se for retorna-o
            ((funcall objetivop no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) 0))
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL
            ((null abertos-novo) (list nil nos-expandidos-novo nos-gerados-novo 0 0))
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) 0))
            ; Aplica recursividade para continuar a procurar
            (t (dfs-loop (car abertos-novo) objetivop funcao-sucessores operadores profundidade-max nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados (list no-inicial))))
          )
        )
  )
)

(defun aestrela (no-inicial objetivo funcao-sucessores funcao-heuristica operadores &optional abertos fechados)
  "Implementação do algoritmo A*. Recebe o nó inicial, o objetivo de pontuação, os nós sucessores, os operadores e como parâmetros opcionais a lista de abertos e fechados. Retorna uma lista com os nós que compõem o caminho, ou NIL."
         ; Caso base: primeira chamada da função; profundidade é zero; o cavalo é colocado na primeira linha
  (cond ((= (no-profundidade no-inicial) 0) 
         (let* ((sucessores-no-inicial (sucessores-iniciais no-inicial))
                (sucessores-no-inicial-heuristica (aplicar-heuristica sucessores-no-inicial funcao-heuristica (objetivo-valor objetivo)))
                (sucessores-ordenados (ordena-nos-custo sucessores-no-inicial-heuristica)))
          (aestrela (car sucessores-ordenados) objetivo funcao-sucessores funcao-heuristica operadores (cdr sucessores-ordenados) (append fechados (list no-inicial)))
         )
        )
        ; Caso recursivo: executa a função normalmente, com recurso à função auxiliar
        (t (aestrela-loop no-inicial objetivo funcao-sucessores funcao-heuristica operadores (length fechados) (1+ (length abertos)) abertos fechados))
  )
)

(defun aestrela-loop (no-inicial objetivo funcao-sucessores funcao-heuristica operadores &optional (nos-expandidos 0) (nos-gerados 0) abertos fechados menor-custo)
  "Função auxiliar para o algoritmo A*"
         ; Lista de nós abertos juntamente com os nós fechados
  (let* ((abertos-fechados (append abertos fechados))
         ; Lista de nós sucessores gerados pelo nó passado como argumento através dos operadores
         (sucessores-gerados (remover-se (lambda (suc) (no-existp suc abertos-fechados 'aestrela)) (funcall funcao-sucessores no-inicial operadores 'aestrela)))
         ; Lista de nós sucessores com a heurística aplicada
         (sucessores-heuristica (aplicar-heuristica sucessores-gerados funcao-heuristica (objetivo-valor objetivo)))
         ; Gera a lista de nós que são solução
         (solucao (list (apply #'append (mapcar (lambda (suc) (cond ((funcall (objetivo-funcao objetivo) suc) suc))) sucessores-heuristica))))
         ; Gera a lista de nós abertos com os nós sucessores (que não constam na lista de nós abertos) adicionados
         (abertos-novo (ordena-nos-custo (append abertos sucessores-heuristica)))
         ; Ordena a lista de aberrtos por ordem crescente de custo
         (menor-custo (no-custo (no-menor-custo (append abertos-novo (remover-se (lambda (no) (null (no-pai no))) fechados))))))
        (format t "Menor custo: ~5,5f~%" menor-custo)
        (let ((nos-expandidos-novo (1+ nos-expandidos))
              (nos-gerados-novo (+ nos-gerados (length sucessores-gerados))))
          (cond 
            ; Verifica se o nó inicial é solução, se for retorna-o
            ((funcall (objetivo-funcao objetivo) no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) 0))
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL
            ((null abertos-novo) (list nil nos-expandidos-novo nos-gerados-novo 0 0))
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) 0))
            ; Aplica recursividade para continuar a procurar
            (t (aestrela-loop (car abertos-novo) objetivo funcao-sucessores funcao-heuristica operadores nos-expandidos-novo nos-gerados-novo (cdr abertos-novo) (append fechados (list no-inicial))))
          )
        )
  )
)

(defun aestrela-teste ()
  (aestrela (cria-no (tabuleiro-teste)) (cria-objetivo 2000) 'sucessores 'heuristica-base (operadores))
)

;;; Medidas de desempenho

;; Função que calcula a penetrância do resultado
(defun penetrancia (no nos-gerados)
  "Calcula a penetrância do resultado"
  (/ (no-profundidade no) nos-gerados)
)