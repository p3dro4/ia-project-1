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
;; define a função problemas-dat
(defun carregar-componentes (path)
  "Carrega todos os ficheiros de código e define a função problemas-dat"
  (carregar-ficheiros path)
  (carregar-funcao-problemas-dat path)
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

(defun ler-problema (n &optional (ficheiro (problemas-dat)))
  "Lê o ficheiro de problemas e devolve o problema n"
  (with-open-file (stream ficheiro :direction :input :if-does-not-exist nil)
    (ler-ficheiro stream n)
  )
)

(defun ler-ficheiro (stream n)
  "Lê as linhas do ficheiro de problemas"
  (let ((linha (read-line stream nil)))
    (cond ((null linha) nil)
          ((equal linha (concatenate 'string "*" (write-to-string n))) (ler-propriedades-problema stream))
          (t (ler-ficheiro stream n ))
    )
  )
)

(defun ler-propriedades-problema (stream &optional nome (tabuleiro "") (objetivo 0))
  "Lê as propriedades do problema"
  (let ((linha (read-line stream nil)))
    (cond ((null linha) (list nome (read-from-string tabuleiro) objetivo))
          ((equal (subseq linha 0 1) "*") (list nome (read-from-string tabuleiro) objetivo))
          ((null nome) (ler-propriedades-problema stream linha tabuleiro objetivo))
          ((equal (subseq linha 0 3) "Obj") (ler-propriedades-problema stream nome tabuleiro (parse-integer (subseq linha 10))))
          (t (ler-propriedades-problema stream nome (concatenate 'string tabuleiro linha) objetivo))
    )
  )
)

(inicializar)