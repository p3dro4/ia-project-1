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

;; Função que executa a experiência/resolução do problema fornecido
;; e escreve no ficheiro experiencias.txt
(defun carregar-funcao-escrever-ficheiro-experiencias (caminho)
  "Carrega a função escrever-ficheiro-experiencias"
  (defun escrever-ficheiro-experiencias (problema)
    "Executa a experiencia/resolução do problema fornecido e escreve no ficheiro experiencias.txt"
    (with-open-file (stream (merge-pathnames "../experiencias.txt" caminho) :direction :output :if-exists :append :if-does-not-exist :create)
      (format stream "#~a - ~a~%" 0 (escreve-tempo (get-universal-time)))
      (executar-experiencia problema stream)
      (format stream "~%------------------------------------------~%")
      (close stream)
    )
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

;; Função que executa a experiência/resolução do problema fornecido
;; e escreve no output fornecido
(defun executar-experiencia (problema &optional (output t))
    "Executa a experiencia/resolução do problema fornecido"
    (format output "~a~%" (first problema))
    (escreve-tabuleiro-formatado (second problema) output t t)
    (format output "Objetivo: ~a~%" (third problema))
    (format output "~%** Algoritmos de procura **~%" )
    (format output "-- BFS --~%" )
    (let ((resultado (bfs (cria-no (second problema)) (lambda-objetivo (third problema)) 'sucessores (operadores))))
      (progn (format output "Solucao: ") (cond ((not (null (first resultado))) (escreve-caminho (no-caminho (first resultado)) output)) (t (format output "**SEM SOLUCAO**"))))
      (format output "~%Pontuacao: ~a~%" (cond ((not (null (no-pontuacao (first resultado)))) (no-pontuacao (first resultado))) (t "**SEM SOLUCAO**")))
      (format output "Max. Pontuacao: ~a~%" (no-pontuacao (fourth resultado)))
      (format output "Nos expandidos: ~a~%" (second resultado))
      (format output "Nos gerados: ~a~%" (third resultado))
    )
)

; TODO: Adicionar o print do tabuleiro
(defun print-recursivo-no (no)
  (cond ((null no) nil)
        (t (progn (print-recursivo-no (no-pai no)) (format t "~a~%" (no-estado no))))
  )
)

;; Função que executa a experiência/resolução do problema fornecido
;; e escreve no ficheiro experiencias.txt
(defun escrever-ficheiro-experiencias (problema)
  "Executa a experiencia/resolução do problema fornecido e escreve no ficheiro experiencias.txt"
  (with-open-file (stream "experiencias.txt" :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "#~a - ~a~%" 0 (escreve-tempo (get-universal-time)))
    (executar-experiencia problema stream)
    (format stream "~%------------------------------------------~%")
    (close stream)
  )
)

(defun ultimo-id-experiencia (caminho)
  
)

;; Função que escreve o tempo no formato dd/mm/aaaa @ hh:mm:ss
;; no output fornecido
(defun escreve-tempo (tempo-universal &optional output)
  "Escreve o tempo fornecido no formato dd/mm/aaaa @ hh:mm:ss"
  (multiple-value-bind (segundos minutos horas dia mes ano) (decode-universal-time tempo-universal)
    (format output "~2,'0d/~2,'0d/~2,'0d @ ~2,'0d:~2,'0d:~2,'0d" dia mes ano horas minutos segundos)
  )
)

(inicializar)