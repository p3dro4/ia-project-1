;;;; auxiliar.lisp
;;;; Funções auxiliares fora do âmbito do problema.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro

;; Função que recebe um tabuleiro e retorna um tabuleiro preenchido com '.'.
(defun tabuleiro-jogadas-vazio (numero-linhas tamanho-linha)
  "Tabuleiro vazio"
  (tabuleiro-aleatorio (loop for n from 0 below (* tamanho-linha numero-linhas) collect ".") tamanho-linha)
)

;; Função que recebe um operador e um tabuleiro e altera o tabuleiro de acordo com o operador. 
;; Retorna o tabuleiro passado como argumento caso o operador não seja válido.
(defun regista-operacao (operador tabuleiro)
  "Regista a operação aplicada ao tabuleiro"
  (cond ((or (null tabuleiro) (not (cavalo-colocado-p tabuleiro))) nil)
        (t (let ((origem (posicao-cavalo tabuleiro))
                 (destino (posicao-cavalo (funcall operador tabuleiro))))
             (cond ((null destino) tabuleiro)
                    (t (regista-movimento (casas-movimento (- (first destino) (first origem)) (- (second destino) (second origem)) tabuleiro) tabuleiro))
             )
          ))
  )
)

;; Função auxiliar que recebe o destino e o tabuleiro e
;; regista o movimento do cavalo no tabuleiro: marcando a origem (o) e o caminho até lá (- ou |).
(defun regista-movimento (casas tabuleiro &optional (origem (posicao-cavalo tabuleiro)) casa-anterior )
  "Regista o movimento do cavalo no tabuleiro"
  (let ((casa-atual (car casas)))
    (cond ((null tabuleiro) tabuleiro)
      ((= (length casas) 1) (substituir (first casa-atual) (second casa-atual) tabuleiro t))
      ((equal casa-atual origem) (regista-movimento (cdr casas) (substituir (first casa-atual) (second casa-atual) tabuleiro "o") origem))
      ((and (not (null casa-anterior)) (equal (celula (first casa-anterior) (second casa-anterior) tabuleiro) "|"))  (regista-movimento (cdr casas) (substituir (first casa-atual) (second casa-atual) tabuleiro "-") origem casa-atual))
      ((and (not (null casa-anterior)) (equal (celula (first casa-anterior) (second casa-anterior) tabuleiro) "-"))  (regista-movimento (cdr casas) (substituir (first casa-atual) (second casa-atual) tabuleiro "|") origem casa-atual))
      ((= (first casa-atual) (first origem)) (regista-movimento (cdr casas) (substituir (first casa-atual) (second casa-atual) tabuleiro "-") origem casa-atual))
      (t (regista-movimento (cdr casas) (substituir (first casa-atual) (second casa-atual) tabuleiro "|") origem casa-atual))
    )
  )    
)

;; Fução auxiliar que recebe o movimento a realizar em i e j, i.e (2 -1) e o tabuleiro e
;; retorna uma lista com as casas que constituem o movimento do cavalo.
(defun casas-movimento (i j tabuleiro &optional (lista-casas (list (posicao-cavalo tabuleiro))))
  "Retorna uma lista com as casas que constituem o movimento do cavalo"
  (let* ((posicao (posicao-cavalo tabuleiro))
         (tabuleiro-origem-nil (substituir (first posicao) (second posicao) tabuleiro nil))
        )
    (cond ((> i 0) (casas-movimento (1- i) j (substituir (1+ (first posicao)) (second posicao) tabuleiro-origem-nil t) (append lista-casas (list (list (1+ (first posicao)) (second posicao))))))
          ((< i 0) (casas-movimento (1+ i) j (substituir (1- (first posicao)) (second posicao) tabuleiro-origem-nil t) (append lista-casas (list (list (1- (first posicao)) (second posicao))))))
          ((> j 0) (casas-movimento i (1- j) (substituir (first posicao) (1+ (second posicao)) tabuleiro-origem-nil t) (append lista-casas (list (list (first posicao) (1+ (second posicao)))))))
          ((< j 0) (casas-movimento i (1+ j) (substituir (first posicao) (1- (second posicao)) tabuleiro-origem-nil t) (append lista-casas (list (list (first posicao) (1- (second posicao)))))))
          (t lista-casas)
    )
  )
)