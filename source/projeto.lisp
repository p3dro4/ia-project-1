;;;; projeto.lisp
;;;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro

;; Função que inicializa o programa
(defun inicializar ()
  "Inicializa o programa"
  (format t ";; A carregar ficheiros...~%")
  (cond ((not (null *load-pathname*)) (progn (carregar-componentes *load-pathname*) (format t ";; Ficheiros carregados.~%")))
        ((not (null *compile-file-pathname*)) (progn (carregar-componentes *compile-file-pathname*) (format t ";; Ficheiros carregados.~%")))
        (t (format t ";; Falha ao carregar ficheiros!!!~%"))
  )
)

;; Função que carrega todos os ficheiros de código e
;; define as funções necessárias
(defun carregar-componentes (path)
  "Carrega todos os ficheiros de código e define as funções necessárias"
  (carregar-ficheiros path)
  (carregar-funcao-problemas-dat path)
  (carregar-funcao-recarregar path)
  (carregar-funcao-compilar-e-carregar path)
)

;; Função que carrega os ficheiros de código 
(defun carregar-ficheiros (path)
  "Carrega os ficheiros de código"
  (load (merge-pathnames "procura" path)) 
  (load (merge-pathnames "puzzle" path))
)

;; Função carrega o ficheiro de problemas
(defun carregar-funcao-problemas-dat (path)
  "Carrega o ficheiro de problemas"
  (defun problemas-dat ()
    (merge-pathnames "../problemas.dat" path)
  )
)

;; Função que recarrega o ficheiro atual
(defun carregar-funcao-recarregar (path)
  "Recarrega o ficheiro atual"
  (defun recarregar ()
    (format t ";; A recarregar ficheiro \"~a.~a\"...~%" (pathname-name path) (pathname-type path))
    (load path) 
    (format t ";; Ficheiro \"~a.~a\" recarregado.~%" (pathname-name path) (pathname-type path))
  )
)

(defun carregar-funcao-compilar-e-carregar (path)
  (defun compilar-ficheiros ()
    (compile-file path)
    (compile-file (merge-pathnames "procura" path))
    (compile-file (merge-pathnames "puzzle" path))
  )
  (defun carregar-ficheiros-compilados ()
    (load (merge-pathnames (pathname-name path) path))
    (carregar-ficheiros path)
  )
  (defun compilar-e-carregar ()
    (format t ";; A compilar ficheiros...~%")
    (compilar-ficheiros)
    (format t ";; Ficheiros compilados.~%")
    (format t ";; A carregar ficheiros...~%")
    (carregar-ficheiros-compilados)
    (format t ";; Ficheiros carregados.~%")
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

(inicializar)