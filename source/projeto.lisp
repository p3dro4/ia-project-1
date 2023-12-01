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
  (carregar-funcao-experiencias-txt caminho)
  (carregar-funcao-recarregar caminho)
)

;; Função que carrega os ficheiros de código 
(defun carregar-ficheiros (caminho)
  "Carrega os ficheiros de código"
  (load (merge-pathnames "procura" caminho)) 
  (load (merge-pathnames "puzzle" caminho))
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

;; Função carrega o ficheiro de problemas
(defun carregar-funcao-problemas-dat (caminho)
  "Carrega o ficheiro de problemas"
  (defun problemas-dat ()
    (merge-pathnames "../problemas.dat" caminho)
  )
)

;; Função que carrega o ficheiro de experiencias
(defun carregar-funcao-experiencias-txt (caminho)
  "Carrega o ficheiro de experiencias"
  (defun experiencias-txt ()
    (merge-pathnames "../experiencias.txt" caminho)
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
    (ler-ficheiro-problemas stream n)
  )
)

;; Função que lê as linhas do ficheiro de problemas e se encontrar o problema n
;; chama a função ler-propriedades-problema
(defun ler-ficheiro-problemas (stream n)
  "Lê as linhas do ficheiro de problemas"
  (let ((linha (read-line stream nil)))
    (cond ((null linha) nil)
          ((equal linha (concatenate 'string "*" (write-to-string n))) (ler-propriedades-problema stream))
          (t (ler-ficheiro-problemas stream n ))
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

;; Função que escreve a experiência/resolução do problema fornecido
;; no output fornecido (por default escreve no ficheiro experiencias.txt)
(defun escreve-experiencia (problema &optional (output (experiencias-txt)))
  "Executa a experiencia/resolução do problema fornecido e escreve no ficheiro experiencias.txt"
  (let ((ultimo-id (ultimo-id-experiencia)))
    (cond ((pathnamep output) 
          (with-open-file (ficheiro output :direction :output :if-exists :append :if-does-not-exist :create)
            (escreve-conteudo-experiencia problema ficheiro ultimo-id)
            (close ficheiro)
          ))
          (t (escreve-conteudo-experiencia problema output))
    )
  )
)

;; Função auxiliar que escreve o conteúdo da experiência/resolução do problema fornecido
;; no output fornecido
(defun escreve-conteudo-experiencia (problema output &optional (ultimo-id 0))
  "Escreve o conteúdo da experiencia/resolução do problema fornecido no output fornecido"
  (format output "#~a - ~a~%" (1+ ultimo-id) (escreve-tempo (get-universal-time)))
  (executar-experiencia problema output)
  (format output "~%------------------------------------------~%")
)

;; Função que executa a experiência/resolução do problema fornecido
;; e escreve no output fornecido
(defun executar-experiencia (problema &optional (output t) (max-profundidade 8))
    "Executa a experiencia/resolução do problema fornecido"
    (format output "~a~%" (first problema))
    (escreve-tabuleiro-formatado (second problema) output t t)
    (format output "Objetivo: ~a~%" (third problema))
    (format output "~%*Algoritmos de procura*~%" )
    (format output "--BFS--~%" )
    (executar-algoritmo-problema problema 'bfs output)
    (format output "~%--DFS--~%" )
    (executar-algoritmo-problema problema 'dfs output max-profundidade)
)

;; Função que executa o algoritmo de procura fornecido no problema fornecido
(defun executar-algoritmo-problema (problema algoritmo &optional (output t) (max-profundidade 8))
  "Executa o algoritmo de procura fornecido no problema fornecido"
  (let* ((tempo-inicial (get-internal-real-time))
           (resultado (funcall algoritmo (cria-no (problema-tabuleiro problema)) (lambda-objetivo (problema-objetivo problema)) 'sucessores (operadores) max-profundidade))
           (tempo-de-execucao (- (get-internal-real-time) tempo-inicial))
          )
      (progn (format output "Solucao: ") (cond ((not (null (resultado-no resultado))) (escreve-caminho (no-caminho (resultado-no resultado)) output)) (t (format output "**SEM SOLUCAO**"))))
      (format output "~%Pontuacao: ~a~%" (cond ((not (null (no-pontuacao (resultado-no resultado)))) (no-pontuacao (resultado-no resultado))) (t "**SEM SOLUCAO**")))
      (format output "Max. Pontuacao: ~a~%" (no-pontuacao (resultado-no-max-pontuacao resultado)))
      (format output "Nos expandidos: ~a~%" (resultado-nos-expandidos resultado))
      (format output "Nos gerados: ~a~%" (resultado-nos-gerados resultado))
      (format output "Tempo de execucao: ~,6f seg.~%" (/ tempo-de-execucao internal-time-units-per-second))
    )
)

;;; Seletores

;; Função que seleciona o nome do problema
(defun problema-nome (problema)
  "Seleciona o nome do problema"
  (first problema)
)

;; Função que seleciona o tabuleiro do problema~
(defun problema-tabuleiro (problema)
  "Seleciona o tabuleiro do problema"
  (second problema)
)

;; Função que seleciona o objetivo do problema
(defun problema-objetivo (problema)
  "Seleciona o objetivo do problema"
  (third problema)
)

;; Função que seleciona o nó resultante do algoritmo de procura
(defun resultado-no (resultado)
  "Seleciona o nó resultante do algoritmo de procura"
  (first resultado)
)

;; Função que seleciona o nó com a pontuacao máxima do resultado
(defun resultado-nos-expandidos (resultado)
  "Seleciona o número de nós expandidos do resultado"
  (second resultado)
)

;; Função que seleciona o nó com a pontuacao máxima do resultado
(defun resultado-nos-gerados (resultado)
  "Seleciona o número de nós gerados do resultado"
  (third resultado)
)

;; Função que seleciona o nó com a pontuacao máxima do resultado
(defun resultado-no-max-pontuacao (resultado)
  "Seleciona o nó com a pontuacao máxima do resultado"
  (fourth resultado)
)

;; Funções Auxiliares

;; Função que escreve o tempo no formato dd/mm/aaaa @ hh:mm:ss
;; no output fornecido
(defun escreve-tempo (tempo-universal &optional output)
  "Escreve o tempo fornecido no formato dd/mm/aaaa @ hh:mm:ss"
  (multiple-value-bind (segundos minutos horas dia mes ano) (decode-universal-time tempo-universal)
    (format output "~2,'0d/~2,'0d/~2,'0d @ ~2,'0d:~2,'0d:~2,'0d" dia mes ano horas minutos segundos)
  )
)

;; Função que escreve os passos do caminho no output fornecido
(defun escreve-estados-resultado (no &optional (output t))
  "Escreve os passos do caminho no output fornecido"
  (cond ((null no) nil)
        (t (progn (escreve-estados-resultado (no-pai no)) (escreve-tabuleiro-formatado (no-estado no) output t t) (format output "~%")))
  )
)

;; Função que retorna o último id da experiência
;; do ficheiro experiencias.txt
(defun ultimo-id-experiencia ()
  "Retorna o último id da experiência do ficheiro experiencias.txt"
  (with-open-file (ficheiro (experiencias-txt) :direction :input :if-does-not-exist :create)
    (ultimo-id-experiencia-ficheiro ficheiro)
  )
)

;; Função axuiliar que retorna o último id da experiência
(defun ultimo-id-experiencia-ficheiro (ficheiro &optional (ultimo-id 0))
  "Retorna o último id da experiência"
  (let ((linha (read-line ficheiro nil)))
    (cond ((null linha) ultimo-id)
          ((equal linha "") (ultimo-id-experiencia-ficheiro ficheiro ultimo-id))
          ((equal (subseq linha 0 1) "#") (ultimo-id-experiencia-ficheiro ficheiro (obter-id-linha linha ultimo-id)))
          (t (ultimo-id-experiencia-ficheiro ficheiro ultimo-id))
    )
  )
)

;; Função que obtém o id da linha
(defun obter-id-linha (linha &optional (ultimo-id 0))
  "Obtém o id da linha"
  (let ((id (parse-integer (subseq linha 1) :junk-allowed t)))
    (cond ((null id) ultimo-id)
          (t id)
    )
  )
)

(inicializar)