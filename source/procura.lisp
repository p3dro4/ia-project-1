;;;; procura.lisp
;;;; Implementa os algoritmos de procura.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro

;;;; Algoritmo de procura em largura e profundidade (BFS e DFS)

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
             (append (list (list (first posicao) (second posicao))) (no-estados-posicoes (no-pai no)))
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
  (cond  
    ((>= (no-profundidade no) 8) nil)
    ((and (equal algoritmo 'dfs) (>= (no-profundidade no) profundidade-max)) nil)
    (t (mapcar (lambda (op) (novo-sucessor no op)) operadores))
  )
)

;;; Funções auxliares de procura

;; Função que verifica se um nó é solução para o problema A (pontuação >= 70)
(defun solucao-a-p (no)
  "Função que verifica se um nó é solução"
  (>= (no-pontuacao no) 70)
)

;; Função que verifica se um nó é solução para o problema B (pontuação = 60)
(defun solucao-b-p (no)
  "Função que verifica se um nó é solução"
  (= (no-pontuacao no) 60)
)

;; Função que verifica se um nó é solução para o problema C (pontuação = 270)
(defun solucao-c-p (no)
  "Função que verifica se um nó é solução"
  (= (no-pontuacao no) 270)
)

;; Função que verifica se um nó é solução para o problema D (pontuação = 600)
(defun solucao-d-p (no)
  "Função que verifica se um nó é solução"
  (= (no-pontuacao no) 600)
)

;; Função que verifica se um nó é solução para o problema E (pontuação = 300)
(defun solucao-e-p (no)
  "Função que verifica se um nó é solução"
  (= (no-pontuacao no) 300)
)

;; Função que verifica se um nó é solução para o problema F (pontuação = 2000)
(defun solucao-f-p (no)
  "Função que verifica se um nó é solução"
  (= (no-pontuacao no) 2000)
)

;;; Funções auxiliares e de ordenação de nós

;; abertos-bfs
(defun abertos-bfs (lista-abertos lista-sucessores)
  (append lista-abertos lista-sucessores)
)

;; abertos-dfs
(defun abertos-dfs (lista-abertos lista-sucessores)
  (append lista-sucessores lista-abertos)
)

;; no-existp
(defun no-existp (no lista algoritmo)
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
(defun bfs (no-inical objetivop sucessores operadores &optional abertos fechados)
  (let* (
    (sucessores-gerados (funcall sucessores no-inical operadores 'bfs))
    (sucessores-nao-fechados (apply #'append (mapcar (lambda (suc) (
      cond ((not (or (no-existp suc fechados 'bfs) (null (no-estado suc)))) (list suc))))
    sucessores-gerados)))
    (solucao (apply #'append (mapcar (lambda (suc) (
      cond ((funcall objetivop suc) suc))) sucessores-nao-fechados)))
    (abertos-novo (abertos-bfs abertos sucessores-nao-fechados))
    )
    (cond 
      ((funcall objetivop no-inical) no-inical)
      ((null abertos-novo) nil)
      ((not (null solucao)) solucao)
      (t (bfs 
            (car abertos-novo)
            objetivop
            sucessores
            operadores
            (cdr abertos-novo)
            (append fechados (list no-inical))
          ))
    )
  )
)

;; Algoritmo de procura em profundidade
(defun dfs (no-inical objetivop sucessores operadores profundidade-max &optional abertos fechados)
  (let* ((sucessores-gerados (funcall sucessores no-inical operadores 'dfs profundidade-max))
         (abertos-fechados (append abertos fechados))
         (sucessores-nao-fechados (apply #'append (mapcar (lambda (suc) (
           cond ((not (or (no-existp suc abertos-fechados 'dfs) (null (no-estado suc)))) (list suc))))
          sucessores-gerados)))
         (solucao (apply #'append (mapcar (lambda (suc) (
           cond ((funcall objetivop suc) suc))) sucessores-nao-fechados)))
         (abertos-novo (abertos-dfs abertos sucessores-nao-fechados))
        )
    (cond 
      ((funcall objetivop no-inical) no-inical)
      ((null abertos-novo) nil)
      ((not (null solucao)) solucao)
      (t (dfs 
            (car abertos-novo)
            objetivop
            sucessores
            operadores
            profundidade-max
            (cdr abertos-novo)
            (append fechados (list no-inical))
          ))
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