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

(defun escreve-tabuleiro (tabuleiro)
  "Escreve o tabuleiro no ecra"
  (cond ((null tabuleiro) nil)
        (t (progn (format t "~a~%" (car tabuleiro)) (escreve-tabuleiro (cdr tabuleiro))))
  )
)

(defun teste ()
  (let* ((tabuleiro (tabuleiro-aleatorio))
         (tabuleiro-jogado (colocar-cavalo tabuleiro))
        )
    (format t "Tabuleiro aleatório:~%")
    (escreve-tabuleiro tabuleiro)
    (format t "~%Tabuleiro com o cavalo colocado:~%")
    (escreve-tabuleiro tabuleiro-jogado)
    (format t "~%Tabuleiro após o movimento do cavalo com (operador-1):~%")
    (escreve-tabuleiro (operador-1 tabuleiro-jogado))
    (format t "~%")
  )
)