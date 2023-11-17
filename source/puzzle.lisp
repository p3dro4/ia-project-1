;;;; puzzle.lisp
;;;; Código relacionado com o problema.   
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro


;;; Tabuleiros

;; Função que retorna um tabuleiro predefinido.
(defun tabuleiro-teste ()
    "Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

;; Função que retorna um tabuleiro predefinido com o cavalo colocado.
(defun tabuleiro-jogado ()
  "Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posição: i=0 e j=0"
  '(
    (T 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80)
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71)
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

;; Função que recebe um tabuleiro e retorna um tabuleiro preenchido com '.'.
(defun tabuleiro-jogadas-vazio (numero-linhas tamanho-linha)
  "Tabuleiro vazio"
  (tabuleiro-aleatorio (loop for n from 0 below (* tamanho-linha numero-linhas) collect ".") tamanho-linha)
)

;; Função que recebe um tabuleiro e se não tiver nenhum cavalo colocado,
;; coloca um cavalo numa casa da 1ª linha e retorna o tabuleiro com o cavalo colocado.
(defun colocar-cavalo (tabuleiro)
  "Coloca o cavalo numa casa da 1ª linha do tabuleiro"
  (cond ((cavalo-colocado-p tabuleiro) tabuleiro)
        (t (let ((n (length (car tabuleiro))))
          (substituir 0 (random n) tabuleiro t)
        ))
  )
)

;;; Tabuleiro Aleatório

;; Função que recebe um número positivo i e cria uma lista com todos os números
;; entre 0 (inclusivé) e o número passado como argumento (eiclusivé). 
;; Por default o i é 100.
(defun lista-numeros (&optional (n 100))
  "Retorna uma lista com todos os numeros entre 0 e n"
  (reverse (loop for n from 0 below n collect n))
)

;; Função que recebe uma lista e muda aleatoriamente os seus números.
(defun baralhar (lista)
  "Baralha os elementos da lista de forma aleatoria"
    (cond ((null lista) nil)
          (t (let ((num-aleatorio (nth (random (length lista)) lista)))
                  (append (list num-aleatorio) (baralhar (remover-se 
                            (lambda (num) (= num num-aleatorio)) lista)))))
    )
)

;; Função que recebe uma lista de números (por default gera uma lista nova) 
;; e o tamanhho da linha (por default o valor é 10) e retorna um tabuleiro com esses parâmetros.
(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (i 10))
  "Retorna um tabuleiro aleatorio com os números da lista e com o tamanho da linha i"
  (cond
    ((not (= (mod (length lista) i) 0)) (error "O tamanho da lista não é multiplo do tamanho da linha"))
    ((null lista) nil)
    (t (cons (subseq lista 0 i) (tabuleiro-aleatorio (subseq lista i) i)))
  )
)

;;; Seletores

;; Função que recebe um índice e o tabuleiro e 
;; retorna uma lista que representa essa linha do tabuleiro.
(defun linha (i tabuleiro)
  "Retorna a linha i do tabuleiro"
  (nth i tabuleiro)
)

;; Função que recebe dois índices e o tabuleiro e 
;; retorna o valor presente nessa célula do tabuleiro.
(defun celula (i j tabuleiro)
  "Retorna o valor da celula (i,j) do tabuleiro"
  (nth j (nth i tabuleiro))
)

;;; Funções auxiliares

;; Função auxiliar que recebe um predicado e uma lista e
;; remove da lista os elementos que satisfazem o predicado.
(defun remover-se (pred lista)
  "Remove da lista os elementos que satisfazem o predicado"
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (t (cons (car lista) (remover-se pred (cdr lista)))))
)

;; Função que recebe um índice, uma lista e um valor (por default o valor é NIL) e
;; substitui pelo valor pretendido nessa posição.
(defun substituir-posicao (i lista &optional valor)
  "Substitui o valor da posicao i da lista pelo novo valor"
  (cond ((null lista) nil)
        ((= i 0) (cons valor (cdr lista)))
        (t (cons (car lista) (substituir-posicao (1- i) (cdr lista) valor)))
  )
)

;; Função que recebe dois índices, o tabuleiro e um valor (por default o valor é NIL). 
;; A função deverá retornar o tabuleiro com a célula substituída pelo valor pretendido
(defun substituir (i j tabuleiro &optional valor)
  "Substitui o valor da celula (i,j) do tabuleiro pelo novo valor"
  (cond ((null tabuleiro) nil)
        ((= i 0) (cons (substituir-posicao j (car tabuleiro) valor) (cdr tabuleiro)))
        (t (cons (car tabuleiro) (substituir (1- i) j (cdr tabuleiro) valor)))
  )
)

;; Função que recebe o tabuleiro e devolve a posição (i j) em que se encontra o
;; cavalo. Caso o cavalo não se encontre no tabuleiro deverá ser retornado NIL.
(defun posicao-cavalo (tabuleiro &optional (i 0) (j 0))
  "Retorna a posicao do cavalo no tabuleiro"
  (cond ((null tabuleiro) nil)
          (t (let ((n (length tabuleiro))
              (cel (celula i j tabuleiro))
            )
          (cond 
            ((>= i n) nil)
            ((= j n) (posicao-cavalo tabuleiro (1+ i) 0))
            ((< j (length (linha i tabuleiro))) 
                (cond ((eq cel t) (list i j))
                      (t (posicao-cavalo tabuleiro i (1+ j)))
                ))
    ))))
)

;; Predicado que recebe a posição de destino e o tabuleiro e
;; verifica se a posição de destino é válida.
(defun validop (destino tabuleiro)
  "Verifica se a posicao de destino é valida"
  (let ((n (length tabuleiro)))
    (cond ((null destino) nil)
          ((or (< (first destino) 0) (< (second destino) 0)) nil)
          ((or (>= (first destino) n) (>= (second destino) n)) nil)
          ((eq (celula (first destino) (second destino) tabuleiro) nil) nil)
          (t t)
    )
  )
)

;; Função auxiliar que recebe a posição do cavalo, a posição de destino e o tabuleiro e
;; retorna o tabuleiro com o cavalo movido e NIL na casa de onde partiu. Retorna NIL caso a posição de destino não seja válida.
(defun mover-cavalo (posicao destino tabuleiro)
  "Move o cavalo para a posição de destino"
  (cond ((not (validop destino tabuleiro)) nil)
        (t (let ((tabuleiro-substituido-posicao (substituir (first posicao) (second posicao) tabuleiro)))
          (substituir (first destino) (second destino) tabuleiro-substituido-posicao t)
          ; TODO: Falta aplicar as regras após o movimento do cavalo
        ))
  )
)

;; Predicao que recebe um tabuleiro e 
;; verifica se o cavalo está colocado no tabuleiro.
(defun cavalo-colocado-p (tabuleiro)
  "Verifica se o cavalo está colocado no tabuleiro"
  (cond ((posicao-cavalo tabuleiro) t)
        (t nil)
  )
)

;;; Operadores

;; Função que retorna a lista de operadores aplicáveis a um estado.
(defun operadores ()
  "Retorna a lista de movimentos do cavalo"
  (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8)
)

;; Função que movimenta o cavalo 2 casas para baixo e 1 casa para a esquerda.
(defun operador-1 (tabuleiro)
  "Movimenta o cavalo 2 casas para baixo e 1 casa para a esquerda"
  (let* ((posicao (posicao-cavalo tabuleiro))
         (destino (list (+ (first posicao) 2) (1- (second posicao))))
        )
        (mover-cavalo posicao destino tabuleiro)
  )
)

;; Função que movimenta o cavalo 2 casas para baixo e 1 casa para a direita.
(defun operador-2 (tabuleiro)
  "Movimenta o cavalo 2 casas para baixo e 1 casa para a direita"
  (let* ((posicao (posicao-cavalo tabuleiro))
         (destino (list (+ (first posicao) 2) (1+ (second posicao))))
        )
        (mover-cavalo posicao destino tabuleiro)
  )
)

;; Função que movimenta o cavalo 2 casas para a direita e 1 casa para baixo.
(defun operador-3 (tabuleiro)
  "Movimenta o cavalo 2 casas para a direita e 1 casa para baixo"
  (let* ((posicao (posicao-cavalo tabuleiro))
         (destino (list (1+ (first posicao)) (+ (second posicao) 2)))
        )
        (mover-cavalo posicao destino tabuleiro)
  )
)

;; Função que movimenta o cavalo 2 casas para a direita e 1 casa para cima.
(defun operador-4 (tabuleiro)
  "Movimenta o cavalo 2 casas para a direita e 1 casa para cima"
  (let* ((posicao (posicao-cavalo tabuleiro))
         (destino (list (1- (first posicao)) (+ (second posicao) 2)))
        )
        (mover-cavalo posicao destino tabuleiro)
  )
)

;; Função que movimenta o cavalo 2 casas para cima e 1 casa para a direita.
(defun operador-5 (tabuleiro)
  "Movimenta o cavalo 2 casas para cima e 1 casa para a direita"
  (let* ((posicao (posicao-cavalo tabuleiro))
         (destino (list (- (first posicao) 2) (1+ (second posicao))))
        )
        (mover-cavalo posicao destino tabuleiro)
  )
)

;; Função que movimenta o cavalo 2 casas para cima e 1 casa para a esquerda.
(defun operador-6 (tabuleiro)
  "Movimenta o cavalo 2 casas para cima e 1 casa para a esquerda"
  (let* ((posicao (posicao-cavalo tabuleiro))
         (destino (list (- (first posicao) 2) (1- (second posicao))))
        )
        (mover-cavalo posicao destino tabuleiro)
  )
)

;; Função que movimenta o cavalo 2 casas para a esquerda e 1 casa para cima.
(defun operador-7 (tabuleiro)
  "Movimenta o cavalo 2 casas para a esquerda e 1 casa para cima"
  (let* ((posicao (posicao-cavalo tabuleiro))
         (destino (list (1- (first posicao)) (- (second posicao) 2)))
        )
        (mover-cavalo posicao destino tabuleiro)
  )
)

;; Função que movimenta o cavalo 2 casas para a esquerda e 1 casa para baixo.
(defun operador-8 (tabuleiro)
  "Movimenta o cavalo 2 casas para a esquerda e 1 casa para baixo"
  (let* ((posicao (posicao-cavalo tabuleiro))
         (destino (list (1+ (first posicao)) (- (second posicao) 2)))
        )
        (mover-cavalo posicao destino tabuleiro)
  )
)
;; Função que recebe um tabuleiro e escreve-o no ecrã.
(defun escreve-tabuleiro (tabuleiro)
  "Escreve o tabuleiro no ecrã"
  (cond ((null tabuleiro) nil)
        (t (progn (format t "~a~%" (car tabuleiro)) (escreve-tabuleiro (cdr tabuleiro))))
  )
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
(defun regista-movimento (casas tabuleiro &optional (origem (posicao-cavalo tabuleiro)) casa-anterior)
  "Regista o movimento do cavalo no tabuleiro"
  (let ((casa-atual (car casas)))
    (cond ((null tabuleiro) tabuleiro)
      ((= (length casas) 1) (substituir (first casa-atual) (second casa-atual) tabuleiro t))
      ((equal casa-atual origem) (regista-movimento (cdr casas) (substituir (first casa-atual) (second casa-atual) tabuleiro "o") origem))
      ((= (first casa-atual) (first origem)) (regista-movimento (cdr casas) (substituir (first casa-atual) (second casa-atual) tabuleiro "-") origem))
      (t (regista-movimento (cdr casas) (substituir (first casa-atual) (second casa-atual) tabuleiro "|") origem))
    )
  )    
  ; TODO: Falta escolher o caracter para marcar o caminho
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

(defun teste ()
  (let* ((tabuleiro (tabuleiro-aleatorio))
         (tamanho-linha (length (car tabuleiro)))
         (numero-linhas (length tabuleiro))
         (posicao-cavalo-inicial (posicao-cavalo (tabuleiro-jogado)))
         (tabuleiro-vazio (tabuleiro-jogadas-vazio numero-linhas tamanho-linha))
         (tabuleiro-jogadas (substituir (first posicao-cavalo-inicial) (second posicao-cavalo-inicial) tabuleiro-vazio t))
        )
    (format t "Tabuleiro aleatorio:~%")
    (escreve-tabuleiro tabuleiro)
    (format t "~%Tabuleiro de teste:~%")
    (escreve-tabuleiro (tabuleiro-jogado))
    (format t "~%Tabuleiro de teste com o cavalo colocado:~%")
    (escreve-tabuleiro (tabuleiro-jogado))
    (format t "~%Tabuleiro de teste apos o movimento do cavalo com (operador-2):~%")
    (escreve-tabuleiro (operador-2 (tabuleiro-jogado)))
    (format t "~%Tabuleiro com as jogadas vazio:~%")
    (escreve-tabuleiro tabuleiro-jogadas)
    (format t "~%Tabuleiro com as jogadas apos o movimento do cavalo com (operador-2):~%")
    (escreve-tabuleiro (regista-operacao 'operador-2 (operador-2 tabuleiro-jogadas)))
  )
)