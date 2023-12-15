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
(defun colocar-cavalo (tabuleiro &optional (j (random (length (car tabuleiro)))))
  "Coloca o cavalo numa casa da 1ª linha do tabuleiro"
  (cond ((cavalo-colocado-p tabuleiro) nil)
        (t (cond ((null (celula 0 j tabuleiro)) nil)
                (t (aplicar-regras (substituir 0 j tabuleiro t) (celula 0 j tabuleiro)))
           )
        )
  )
)

;;; Tabuleiro Aleatório

;; Função que recebe um número positivo i e cria uma lista com todos os números
;; entre 0 (inclusivé) e o número passado como argumento (eiclusivé). 
;; Por default o i é 100.
;; teste: (lista-numeros 10)
;; resultado: (9 8 7 6 5 4 3 2 1 0)
(defun lista-numeros (&optional (n 100) (i 0))
  "Retorna uma lista com todos os numeros entre 0 e n"
  (cond ((< n 0) (error "O número tem de ser positivo"))
        ((= i n) nil)
        (t (cons (1- (- n i)) (lista-numeros n (1+ i))))
  )
)

;; Função que recebe uma lista e muda aleatoriamente os seus números.
;; teste: (baralhar '(1 2 3 4 5 6 7 8 9 10))
;; resultado: "Elementos da lista ordenados aleatoriamente"
(defun baralhar (lista)
  "Baralha os elementos da lista de forma aleatoria"
    (cond ((null lista) nil)
          (t (let ((num-aleatorio (nth (random (length lista)) lista)))
                  (append (list num-aleatorio) (baralhar (remove-if
                            (lambda (num) (= num num-aleatorio)) lista)))))
    )
)

;; Função que recebe uma lista de números (por default gera uma lista nova) 
;; e o tamanhho da linha (por default o valor é 10) e retorna um tabuleiro com esses parâmetros.
;; teste: (tabuleiro-aleatorio)
;; resutaldo: "Tabuleiro aleatorio com 10 linhas e 10 colunas"
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
;; teste: (linha 0 (tabuleiro-jogado))
;; resultado: (T 25 54 89 21 8 36 14 41 96)
(defun linha (i tabuleiro)
  "Retorna a linha i do tabuleiro"
  (nth i tabuleiro)
)

;; Função que recebe dois índices e o tabuleiro e 
;; retorna o valor presente nessa célula do tabuleiro.
;; teste: (celula 0 0 (tabuleiro-jogado))
;; resultado: T
(defun celula (i j tabuleiro)
  "Retorna o valor da celula (i,j) do tabuleiro"
  (cond ((or (< i 0) (< j 0)) nil)
        ((or (>= i (length tabuleiro)) (>= j (length (linha i tabuleiro)))) nil)
        (t (nth j (nth i tabuleiro)))
  )
)

;;; Funções auxiliares

;; Função que recebe um número e retorna o número simétrico 
;; teste: (simetrico 123)
;; resultado: 321
(defun simetrico (num)
  "Retorna o número simétrico"
  (cond ((< num 10) (lista-para-numero (reverse (list 0 num))))
        (t (lista-para-numero (reverse (numero-para-lista num))))
  )
)

;; Função que recebe um número e retorna t se o número for duplo i.e. se for composto por dois algarismos iguais.
;; teste: (duplop 11)
;; resultado: t
(defun duplop (num)
  "Retorna t se o número for duplo i.e. se for composto por dois algarismos iguais"
  (let ((lista (numero-para-lista num)))
    (cond ((null lista) nil)
          ((not (= (length lista) 2)) nil)
          ((= (first lista) (second lista)) t)
          (t nil)
    )
  )
)

;; Função que recebe um tabuleiro e retorna o maior número do tabuleiro.
;; teste: (maior-numero-tabuleiro (tabuleiro-jogado))
;; resultado: 99
(defun maior-numero-tabuleiro (tabuleiro)
  "Retorna o maior número do tabuleiro"
  (cond ((null tabuleiro) nil)
        (t (let ((tabuleiro-numeros (numeros-tabuleiro tabuleiro)))
            (cond ((null tabuleiro-numeros) nil)
                  (t (apply 'max tabuleiro-numeros))
            )
           )
        )
  )
)

;; Função que retorna todos os números de um tabuleiro numa lista.
;; teste: (numeros-tabuleiro (tabuleiro-jogado))
;; resultado: tabuleiro com os símbolos nil e t removidos
(defun numeros-tabuleiro (tabuleiro)
  (remove-if (lambda (num) (or (eq num t) (null num))) (juntar-linhas tabuleiro))
)

;; Função auxiliar que recebe um tabuleiro e retorna uma lista com todas as linhas do tabuleiro juntas.
(defun juntar-linhas (tabuleiro)
  "Junta as linhas do tabuleiro numa lista"
  (cond ((null tabuleiro) nil)
        (t (append (car tabuleiro) (juntar-linhas (cdr tabuleiro))))
  )
)

;; Função auxiliar que recebe uma lista de algarismos e retorna o número correspondente.
;; teste: (lista-para-numero '(1 2 3))
;; resultado: 123
(defun lista-para-numero (lista)
  "Retorna o número correspondente à lista de algarismos"
  (cond ((null lista) 0)
        ((not (numberp (car lista))) (error "A lista não é composta por números"))
        (t (+ (* (car lista) (expt 10 (1- (length lista)))) (lista-para-numero (cdr lista))))
  )
)

;; Função auxiliar que recebe um número e retorna uma lista com os algarismos do número.
;; teste: (numero-para-lista 123)
;; resultado: (1 2 3)
(defun numero-para-lista (num)
  "Retorna uma lista com os algarismos do número"
  (cond ((not (numberp num)) (error "A lista não é composta por números"))
        ((< num 10) (list num))
        (t (append (numero-para-lista (floor (/ num 10))) (list (mod num 10))))
  )
)

;; Função auxiliar que recebe o número da coluna e retorna a letra correspondente
;; teste: (coluna-para-letra 2)
;; resultado: #\c
(defun coluna-para-letra (num)
  "Retorna a letra correspondente ao número da coluna"
  (let ((letra #\a))
    (cond ((not (numberp num)) (error "O argumento não é um número"))
          ((= num 0) letra)
          (t (int-char (+ (char-int letra) num)))
    )
  )
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

;; Função que recebe o tabuleiro e o valor a procurar e
;; retorna a posição (i j) em que se encontra o valor. Caso o valor não se encontre no tabuleiro deverá ser retornado NIL.
(defun posicao-valor (tabuleiro valor &optional (i 0) (j 0))
  "Retorna a posicao do valor no tabuleiro"
      (cond ((null tabuleiro) nil)
            (t (let ((n (length tabuleiro))
                     (cel (celula i j tabuleiro)))
                    (cond 
                      ((>= i n) nil)
                      ((= j n) (posicao-valor tabuleiro valor (1+ i) 0))
                      ((< j (length (linha i tabuleiro))) 
                       (cond ((equal cel valor) (list i j))
                             (t (posicao-valor tabuleiro valor i (1+ j)))
                       ))))
            )
      )
)

;; Função que recebe o tabuleiro e devolve a posição (i j) em que se encontra o
;; cavalo. Caso o cavalo não se encontre no tabuleiro deverá ser retornado NIL.
(defun posicao-cavalo (tabuleiro)
  "Retorna a posicao do cavalo no tabuleiro"
  (posicao-valor tabuleiro t)
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
        (t (let ((tabuleiro-substituido-posicao (substituir (first posicao) (second posicao) tabuleiro))
                  (valor-destino (celula (first destino) (second destino) tabuleiro)))
              (aplicar-regras (substituir (first destino) (second destino) tabuleiro-substituido-posicao t) valor-destino)
            )
        )
  )
)

;; Função que recebe o tabuleiro, o valor de destino e o valor a remover-se duplo (por default o maior valor do tabuleiro) e
;; retorna o tabuleiro com as regras aplicadas.
(defun aplicar-regras (tabuleiro valor-destino &optional (valor-a-remover-se-duplo (maior-numero-tabuleiro tabuleiro)))
  "Aplica as regras do jogo"
  (cond ((duplop valor-destino) ; Se o valor de destino for duplo, remove o valor de valor-a-remover-se-duplo (por default o maior valor do tabuleiro)
          (let ((posicao-valor-a-remover (posicao-valor tabuleiro valor-a-remover-se-duplo)))
            (cond ((null posicao-valor-a-remover) tabuleiro)
                  (t (substituir (first posicao-valor-a-remover) (second posicao-valor-a-remover) tabuleiro))
            )
          )
        )
        ((not (null (posicao-valor tabuleiro (simetrico valor-destino))))  ; Se o valor simétrico do valor de destino estiver no tabuleiro, remove-o
          (let ((posicao-valor-a-remover (posicao-valor tabuleiro (simetrico valor-destino))))
           (substituir (first posicao-valor-a-remover) (second posicao-valor-a-remover) tabuleiro)
          )
        )
        (t tabuleiro)
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

(defun no-pontuacao (no &optional (pontuacao 0))
  "Função que retorna a pontuação de um nó"
  (cond ((null (no-pai no)) 0)
        (t (let ((posicao (posicao-cavalo (no-estado no))))
            (+ pontuacao (celula (first posicao) (second posicao) (no-estado (no-pai no))) (no-pontuacao (no-pai no) pontuacao))
           )
        )
  )
)

;; Função que verifica se um nó existe numa lista de nós
(defun no-existep (no lista algoritmo)
  "Função que verifica se um nó existe numa lista de nós"
  (member-if (lambda (no-lista) (
    cond ((equal algoritmo 'dfs) (and (equal (no-estado no) (no-estado no-lista)) (>= (no-custo no) (no-custo no-lista))))
         ((equal algoritmo 'aestrela) (and (equal (no-estado no) (no-estado no-lista)) (>= (no-custo no) (no-custo no-lista))))
         (t (equal (no-estado no) (no-estado no-lista)))
  )) lista) 
)

;;; Sucessores

;; Função que retorna o sucessor de um nó com o operador dado como argumento
(defun novo-sucessor (no operador funcao-heuristica)
  "Função que retorna o sucessor de um nó com o operador dado como argumento"
  (let ((estado-gerado (funcall operador (no-estado no))))
    (cond
     ((null estado-gerado) nil)
     (t (let* ((posicao-destino (posicao-cavalo estado-gerado))
               (valor-destino (celula (first posicao-destino) (second posicao-destino) (no-estado no)))
               (pontuacao (+ (no-pontuacao no) valor-destino))
               (g (1+ (no-profundidade no)))
               (h (cond ((null funcao-heuristica) 0) (t (funcall funcao-heuristica estado-gerado pontuacao)))))
          (cria-no estado-gerado g h no)
        )
     )
    )
  )
)

;; Função que retorna a lista de sucessores de um nó
(defun sucessores (no funcao-sucessores algoritmo &optional (profundidade-max 0) funcao-heuristica)
  "Função que retorna a lista de sucessores de um nó"
  (cond ((null no) nil)
        ((and (equal algoritmo 'dfs) (>= (no-profundidade no) profundidade-max)) nil)
        ((and (= (no-profundidade no) 0) (not (cavalo-colocado-p (no-estado no)))) (sucessores-iniciais no (car funcao-sucessores) funcao-heuristica))
        (t (apply #'append (mapcar (lambda (op) 
              (let ((sucessor (novo-sucessor no op funcao-heuristica)))
                    (cond ((null sucessor) nil)
                          (t (list sucessor))
                    ))
              ) (cdr funcao-sucessores)))
        )
  )
)

;; Função que retorna a lista de sucessores de um nó após a colocação inicial do cavalo
(defun sucessores-iniciais (no op &optional funcao-heuristica (i 0))
  "Função que retorna a lista de sucessores de um nó após a colocação inicial do cavalo"
  (cond ((>= i (length (car (no-estado no)))) nil) 
        (t (let* ((estado-gerado (funcall op (no-estado no) i))) 
            (cond ((null estado-gerado) (sucessores-iniciais no op funcao-heuristica (1+ i)))
                  (t (let* ((posicao-destino (posicao-cavalo estado-gerado))
                            (valor-destino (celula (first posicao-destino) (second posicao-destino) (no-estado no)))
                            (pontuacao (+ (no-pontuacao no) valor-destino)))   
                      (append (list (cria-no estado-gerado (1+ (no-profundidade no)) (cond ((null funcao-heuristica) 0) (t (funcall funcao-heuristica estado-gerado pontuacao))) no)) (sucessores-iniciais no op funcao-heuristica (1+ i)))
                     )
                  )
            )
          )
        )
  )
)

;; Objetivo

;; Função que recebe um valor e retorna uma função lambda
;; que recebe um nó e verifica se a sua pontuação é maior ou igual ao valor dado
(defun cria-objetivo (valor)
  "Função que recebe um valor e retorna uma função lambda que recebe um nó e verifica se a sua pontuação é maior ou igual ao valor dado"
  (lambda (no) (>= (no-pontuacao no) valor))
)

;; Função que retorna a função objetivo
(defun objetivo-funcao (objetivo)
  "Função que retorna a função objetivo de um nó"
  (first objetivo)
)

;; Função que retorna o valor do objetivo
(defun objetivo-valor (objetivo)
  "Função que retorna o valor objetivo de um nó"
  (second objetivo)
)

;;; Operadores

;; Função que retorna a lista de operadores aplicáveis a um estado.
(defun operadores ()
  "Retorna a lista de movimentos do cavalo"
  (list 'colocar-cavalo 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8)
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

;;; Escrita do tabuleiro

;; Função que recebe um tabuleiro e escreve-o no saida.
(defun escreve-tabuleiro (tabuleiro &optional (saida t))
  "Escreve o tabuleiro no ecrã"
  (cond ((null tabuleiro) nil)
        ((= (length tabuleiro) 1) (format saida "~a" (car tabuleiro)))
        (t (progn (format saida "~a~%" (car tabuleiro)) (escreve-tabuleiro (cdr tabuleiro) saida)))
  )
)

;; Função que recebe um tabuleiro e escreve-o no saida formatado.
(defun escreve-tabuleiro-formatado (tabuleiro &optional (saida t) (numero-linha t) (letra-coluna t) (preenchimento-esquerda 0) (i 0))
  "Escreve o tabuleiro no ecrã formatado"
  (cond (letra-coluna (progn (cond (numero-linha (format saida "   "))) 
                                (cond ((> preenchimento-esquerda 0) (format t "~v,a" preenchimento-esquerda " ")))
                                (format saida "   A   B   C   D   E   F   G   H   I   J~%") 
                                (escreve-tabuleiro-formatado tabuleiro saida numero-linha nil preenchimento-esquerda i)))
        ((null tabuleiro) nil)
        ((>= i (length tabuleiro)) nil)
        (t (progn
            (cond ((> preenchimento-esquerda 0) (format t "~v,a" preenchimento-esquerda " ")))
            (cond (numero-linha (format saida "~2,'0d " (1+ i))))
            (format saida "|" )
            (mapcar (lambda (cel) (cond 
              ((numberp cel) (format saida " ~2,'0d " cel))
              ((eq cel t) (format saida " TT "))
              (t (format saida " .. "))
            )) (linha i tabuleiro))
            (format saida "|~%")
            (escreve-tabuleiro-formatado tabuleiro saida numero-linha nil preenchimento-esquerda (1+ i))
           )
        )
  )
)

;; Função que recebe uma posição e escreve-a no ecrã.
(defun escreve-posicao (posicao &optional (saida t))
  "Escreve a posicao no ecrã"
  (cond ((null posicao) nil)
        ((not (= (length posicao) 2)) (error "A posição não tem 2 elementos"))
        (t (format saida "~a~a" (coluna-para-letra (second posicao)) (1+ (first posicao))))	
  ) 
)

;;;; Funções relacionadas com a resolução do problema

;;; Heurísticas

;; Função que representa uma heurística base
(defun heuristica-base (tabuleiro objetivo pontuacao)
  "Função que representa uma heurística base"
  (let ((numeros (numeros-tabuleiro tabuleiro)))
    (cond ((null numeros) 0)
          (t (let ((heuristica (/ (- objetivo pontuacao) (media numeros))))
              (cond ((< heuristica 0) 0)
                    (t heuristica)
              )
             )
          )
    )
  )
)

(defun heuristica-implementada (tabuleiro objetivo pontuacao)
  "Função que representa uma heurística base melhorada"
  (let* ((numeros (numeros-tabuleiro tabuleiro))
         (numero-de-duplos (length (remove-if (lambda (num) (not (duplop num))) numeros))))
    (cond ((null numeros) 0)
          (t (let ((heuristica (/ (- objetivo pontuacao) (media numeros))))
              (cond ((<= heuristica 0) 0)
                    ((> numero-de-duplos 0) (* (/ (length numeros) numero-de-duplos) heuristica))
                    (t heuristica)
              )
             )
          )
    )
  )
)

(defun numeros-redor-cavalo (tabuleiro &optional (i -2) (j -2) numeros)
  "Função que retorna os números em redor do cavalo (5x5)"
  (cond ((null tabuleiro) nil)
        (t (cond ((not (cavalo-colocado-p tabuleiro)) nil)
                 ((> i 2) (remove-if (lambda (num) (null num)) numeros))
                 ((> j 2) (numeros-redor-cavalo tabuleiro (1+ i) -2 numeros))
                 (t (let ((posicao (posicao-cavalo tabuleiro)))
                      (cond ((and (= i 0) (= j 0)) (numeros-redor-cavalo tabuleiro i (1+ j) numeros))
                            (t (numeros-redor-cavalo tabuleiro i (1+ j) (cons (celula (+ (first posicao) i) (+ (second posicao) j) tabuleiro) numeros)))    
                      )
                    )
                 )
            )
        )
  )
)