;;;; projeto.lisp
;;;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro

;; Função que inicializa o programa
(defun inicializar ()
  "Inicializa o programa"
  (format t ";; A carregar ficheiros...~%")
  (cond ((not (null *load-pathname*)) (progn (carregar-componentes *load-pathname*) (format t ";; Ficheiros carregados.~%")))
        ((not (null *compile-file-pathname*)) (progn (carregar-componentes *compile-file-caminhoname*) (format t ";; Ficheiros carregados.~%")))
        (t (format t ";; Falha ao carregar ficheiros!!!~%"))
  )
)

;; Função que carrega todos os ficheiros de código e
;; define as funções necessárias
(defun carregar-componentes (caminho)
  "Carrega todos os ficheiros de código e define as funções necessárias"
  (carregar-ficheiros caminho)
  (carregar-funcao-problemas-dat caminho)
  (carregar-funcao-recarregar caminho)
)

;; Função que carrega os ficheiros de código 
(defun carregar-ficheiros (caminho)
  "Carrega os ficheiros de código"
  (load (merge-pathnames "procura" caminho)) 
  (load (merge-pathnames "puzzle" caminho))
  (load (merge-pathnames "testes" caminho)) ; TODO: Remover ficheiro (temporário)
)

;; Função carrega o ficheiro de problemas
(defun carregar-funcao-problemas-dat (caminho)
  "Carrega o ficheiro de problemas"
  (defun problemas-dat ()
    (merge-pathnames "../problemas.dat" caminho)
  )
)

;; Função que recarrega o ficheiro atual
(defun carregar-funcao-recarregar (caminho)
  "Recarrega o ficheiro atual"
  (defun recarregar ()
    (format t ";; A recarregar ficheiros...~%")
    (load caminho) 
    (format t ";; Ficheiros recarregados.~%")
  )
)

;; Função que lê o ficheiro de problemas e devolve uma lista com todos os problemas
(defun ler-todos-problemas (&optional (i 0) problemas (ficheiro (problemas-dat)))
  "Lê todos os problemas"
  (let ((problema (ler-problema i ficheiro)))
    (cond ((null problema) problemas)
          (t (ler-todos-problemas (+ i 1) (append problemas (list problema)) ficheiro))
    )
  )
)

;; Função que lê o ficheiro de problemas e devolve o problema n
(defun ler-problema (n &optional (ficheiro (problemas-dat)))
  "Lê o ficheiro de problemas e devolve o problema n"
  (with-open-file (stream ficheiro :direction :input :if-does-not-exist nil)
    (ler-ficheiro stream n)
  )
)

;; Função que lê as linhas do ficheiro de problemas e se encontrar o problema n
;; chama a função ler-propriedades-problema
(defun ler-ficheiro (stream n)
  "Lê as linhas do ficheiro de problemas"
  (let ((linha (read-line stream nil)))
    (cond ((null linha) nil)
          ((equal linha (concatenate 'string "*" (write-to-string n))) (ler-propriedades-problema stream))
          (t (ler-ficheiro stream n ))
    )
  )
)

;; Função que lê as propriedades do problema e devolve uma lista com o nome,
;; o tabuleiro e o objetivo
(defun ler-propriedades-problema (stream &optional nome (tabuleiro "") (objetivo 0))
  "Lê as propriedades do problema"
  (let ((linha (read-line stream nil)))
    (cond ((null linha) (list nome (read-from-string tabuleiro) objetivo))          
          ((equal linha "") (ler-propriedades-problema stream nome tabuleiro objetivo))
          ((equal (subseq linha 0 1) "*") (list nome (read-from-string tabuleiro) objetivo))
          ((null nome) (ler-propriedades-problema stream linha tabuleiro objetivo))
          ((equal (subseq linha 0 3) "Obj") (ler-propriedades-problema stream nome tabuleiro (parse-integer (subseq linha 10))))
          (t (ler-propriedades-problema stream nome (concatenate 'string tabuleiro linha) objetivo))
    )
  )
)

(defun executar-experiencia (problema &optional (output t))
    (format output "~a~%" (first problema))
    (escreve-tabuleiro-formatado (second problema) output t t)
    (format output "Objetivo: ~a~%" (third problema))
    (format output "~%** Algoritmos de procura **~%" )
    (format output "-- BFS --~%" )
    ;(progn (format output "Solucao: ") (escreve-caminho (no-caminho (bfs (cria-no (second problema)) (lambda-objetivo (third problema)) 'sucessores (operadores))) output))
    (let ((resultado (bfs (cria-no (second problema)) (lambda-objetivo (third problema)) 'sucessores (operadores))))
      (progn (format output "Solucao: ") (escreve-caminho (no-caminho (first resultado)) output))
      (format output "~%Pontuacao: ~a~%" (no-pontuacao (first resultado)))
      (format output "Nos expandidos: ~a~%" (second resultado))
      (format output "Nos gerados: ~a~%" (third resultado))
    )
)

(defun escrever-ficheiro-experiencias (problema &optional caminho)
  (with-open-file (stream "experiencias.txt" :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "#~a - ~a~%" 0 (escreve-tempo (get-universal-time)))
    (executar-experiencia problema stream)
    (format stream "~%------------------------------------------~%")
    (close stream)
  )
)

(defun ultimo-id-experiencia (nome-ficheiro caminho)
  
)

(defun escreve-tempo (tempo-universal &optional output)
  (multiple-value-bind (segundos minutos horas dia mes ano) (decode-universal-time tempo-universal)
    (format output "~2,'0d/~2,'0d/~2,'0d @ ~2,'0d:~2,'0d:~2,'0d" dia mes ano horas minutos segundos)
  )
)

(inicializar)