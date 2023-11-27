;;;; procura.lisp
;;;; Implementa os algoritmos de procura.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro

;;;; Algoritmo de procura em largura e profundidade (BFS e DFS)
;;; Construtor
(defun cria-no (tabuleiro &optional (g 0) (pai nil))
  (list tabuleiro g pai)
)

;;; Funções auxliares de procura

;;; Funções auxliares e de ordenação de nós