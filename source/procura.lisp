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

;; Função que recalcula a profundidade dos nós que se encontram em abertos ou fechados
(defun recalcular-profundidade (lista-sucessores lista-nos &optional nos-recalculados)
  "Função que recalcula a profundidade dos nós que se encontram em abertos ou fechados"
  (cond ((null lista-nos) nos-recalculados)
        ((and (null lista-sucessores) (null nos-recalculados)) lista-nos)
        ((null lista-sucessores) nos-recalculados)
        (t (recalcular-profundidade lista-sucessores (cdr lista-nos) (append nos-recalculados (list (recalcular-profundidade-auxiliar (car lista-nos) lista-sucessores)))))
  )
)

;; Função auxiliar que recalcula a profundidade de um nó baseado na lista de sucessores
(defun recalcular-profundidade-auxiliar (no-atual lista-sucessores &optional no-recalculado)
  "Função auxiliar que recalcula a profundidade de um nó baseado na lista de sucessores"
  (cond ((and (null lista-sucessores) (null no-recalculado)) no-atual)
        ((null lista-sucessores) no-recalculado)
        (t (let ((no-recalculado-novo (procurar-e-substituir-no no-atual (car lista-sucessores))))
              (cond ((null no-recalculado-novo) (recalcular-profundidade-auxiliar no-atual (cdr lista-sucessores) no-recalculado))
                    ((null no-recalculado) (recalcular-profundidade-auxiliar no-atual (cdr lista-sucessores) no-recalculado-novo))
                    (t (cond ((< (no-profundidade no-recalculado-novo) (no-profundidade no-recalculado)) (recalcular-profundidade-auxiliar no-atual (cdr lista-sucessores) no-recalculado-novo))
                             (t (recalcular-profundidade-auxiliar no-atual (cdr lista-sucessores) no-recalculado))
                       )
                    )
              ) 
            )
        )
        (t (recalcular-profundidade-auxiliar no-atual (cdr lista-sucessores) no-recalculado))   
  )
)

;; Função que procura na estrutura do nó passado como argumento (o nó e os pais) e substitui o nó encontrado pelo nó substituto
(defun procurar-e-substituir-no (no no-substituto)
  "Função que procura na estrutura do nó passado como argumento (o nó e os pais) e substitui o nó encontrado pelo nó substituto"
  (cond ((null (no-pai no)) nil)
        (t (cond ((equal (no-estado no) (no-estado no-substituto))
                  (cria-no (no-estado no-substituto) (no-profundidade no-substituto) (no-heuristica no-substituto) (no-pai no) (no-pontuacao no-substituto)))
                 (t (let ((pai (procurar-e-substituir-no (no-pai no) no-substituto)))
                      (cond ((null pai) nil)
                            (t (cria-no (no-estado no) (1+ (no-profundidade pai)) (no-heuristica no) pai (no-pontuacao no)))
                      )
                    )
                 )
           )
        )
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
            ((funcall objetivop no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) (ramificacao-media no-inicial nos-gerados)))
            ; Verifica se a lista de nós abertos é nula, se for retorna NIL
            ((null abertos-novo) (list nil nos-expandidos-novo nos-gerados-novo 0 0))
            ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
            ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) (ramificacao-media (car solucao) nos-gerados-novo)))
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
         ; Lista de nós abertos com as profundidades recalculadas
         (abertos-recalculados (recalcular-profundidade sucessores-gerados abertos-novo))
         ; Lista de nós fechados com as profundidades recalculadas
         (fechados-recalculados (recalcular-profundidade sucessores-gerados fechados)))
    (let ((nos-expandidos-novo (1+ nos-expandidos))
          (nos-gerados-novo (+ nos-gerados (length sucessores-gerados))))
      (cond 
        ; Verifica se o nó inicial é solução, se for retorna-o
        ((funcall objetivop no-inicial) (list no-inicial nos-expandidos-novo nos-gerados (penetrancia no-inicial nos-gerados) (ramificacao-media no-inicial nos-gerados)))
        ; Verifica se a lista de nós abertos é nula, se for retorna NIL
        ((null abertos-recalculados) (list nil nos-expandidos-novo nos-gerados-novo 0 0))
        ; Verifica se a lista de nós solução não é nula, se não for retorna o 1º nó da lista
        ((not (null (car solucao))) (list (car solucao) nos-expandidos-novo nos-gerados-novo (penetrancia (car solucao) nos-gerados-novo) (ramificacao-media (car solucao) nos-gerados-novo)))
        ; Aplica recursividade para continuar a procurar
        (t (dfs-loop (car abertos-novo) objetivop funcao-sucessores operadores profundidade-max nos-expandidos-novo nos-gerados-novo (cdr abertos-recalculados) (append fechados-recalculados (list no-inicial))))
      )
    )
  )
)

;;; Medidas de desempenho

;; Função que calcula a penetrância do resultado
(defun penetrancia (no nos-gerados)
  "Calcula a penetrância do resultado"
  (/ (no-profundidade no) nos-gerados)
)

;; Função que calcula o factor de ramificação médio
(defun ramificacao-media (no nos-gerados)
  "Calcula o factor de ramificação médio"
  (let* ((comprimento-caminho (no-profundidade no))
        (f (lambda (x) (- (/ (* x (- (expt x comprimento-caminho) 1)) (- x 1)) nos-gerados)))
        (f-primeira-derivada (lambda (x) (/ (+ (- (* comprimento-caminho (expt x (+ comprimento-caminho 1))) (* (+ comprimento-caminho 1) (expt x comprimento-caminho))) 1) (expt (- x 1) 2))))
       )
    (newton-raphson f f-primeira-derivada 0 nos-gerados :maximum-number-of-iterations 100)
  )
)

;; Função auxiliar que retorna a média dos valores fornecidos
(defun media (lista)
  "Função que retorna a média dos valores fornecidos"
  (/ (apply #'+ lista) (length lista))
)

;; Função retirada de http://faculty.washington.edu/dbp/SAPACLISP-1.x/basic-math.lisp
(defun Newton-Raphson
       (f
        f-prime
        x-left
        x-right
        &key
        (accuracy (* 10.0 single-float-epsilon))
        (maximum-number-of-iterations 20)
        (prevent-bracket-jumping-p t))
  "given
   [1] f (required)
       ==> a function with a single argument
   [2] f-prime (required)
       ==> another function with a single argument,
           this one being the first derivative of f
   [3] x-left (required)
       ==> left-hand bracket for the desired root;
           i.e., left-hand bracket <= desired root
   [4] x-right (required)
       ==> right-hand bracket for the desired root;
           i.e., desired root <= right-hand bracket
   [5] accuracy (keyword; (* 10.0 single-float-epsilon))
       ==> desired relative accuracy for computed rood
   [6] maximum-number-of-iterations (keyword; 20)
       ==> maximum number of iterations
   [7] prevent-bracket-jumping-p (keyword; t)
       ==> if t, allows Newton-Raphson to continue
           if it jumps out of the interval
           [x-left, x-right];
           if nil, jumping out of the interval
           causes an error to be signaled
  returns
   [1] a root of f in [x-left, x-right];
       i.e., a value x in that interval
       such that f(x) = 0
   [2] the number of iterations required
  ---
  Note: this function is based loosely on rtnewt,
  Section 9.4, Numerical Recipes, Second Edition"
  (assert (< x-left x-right))
  (let ((x (* 0.5 (+ x-left x-right)))
        delta-x denom-for-accuracy-test)
    (dotimes (j maximum-number-of-iterations
                (if (not (cerror "returns solution so far"
                                 "exceeding maximum number of iterations"))
                  (values x maximum-number-of-iterations)))
      (setf delta-x (/ (funcall f x)  (funcall f-prime x)))
      (setf denom-for-accuracy-test (+ (abs x)
                                       (abs (decf x delta-x))))
      (cond
       (prevent-bracket-jumping-p
        (if (< x x-left) (setf x x-left))
        (if (> x x-right) (setf x x-right))
        (if (< (/ (abs delta-x) denom-for-accuracy-test) accuracy)
          (return (values x (1+ j)))))
       ((<= x-left x x-right)
        (if (< (/ (abs delta-x) denom-for-accuracy-test) accuracy)
          (return (values x (1+ j)))))
       (t
        (error "jumped out of brackets"))))))
