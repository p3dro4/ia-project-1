;;;; projeto.lisp
;;;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro

; TODO: Rever comentérios

;;; Carregar ficheiros de código e definir funções necessárias

;; Função que inicializa o programa
(defun inicializar ()
  "Inicializa o programa"
  (format t ";; A carregar ficheiros...~%")
  (cond ((not (null *load-pathname*)) (progn (carregar-componentes *load-pathname*) (format t ";; Ficheiros carregados.~%")))
        ((not (null *compile-file-pathname*)) (progn (carregar-componentes *compile-file-pathname*) (format t ";; Ficheiros carregados.~%")))
        (t (format t ";; Falha ao carregar ficheiros!!!~%"))
  )
)

;; Função que carrega a função caminho-raiz
(defun carregar-funcao-caminho-raiz (caminho)
  "Carrega a função caminho-raiz"
  (defun caminho-raiz ()
    "Retorna o caminho raiz do projeto"
    (make-pathname :directory (pathname-directory (merge-pathnames "../" caminho )))
  )
)

;; Função que carrega todos os ficheiros de código e
;; define as funções necessárias
(defun carregar-componentes (caminho)
  "Carrega todos os ficheiros de código e define as funções necessárias"
  (carregar-ficheiros caminho)
  (carregar-funcao-recarregar caminho)
  (carregar-funcao-caminho-raiz caminho)
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

;; Função que retorna o caminho do ficheiro problemas.dat
  (defun problemas-dat ()
    "Retorna o caminho do ficheiro problemas.dat"
    (merge-pathnames "problemas.dat" (caminho-raiz))
  )

;; Função que retorna o caminho do ficheiro log.dat
  (defun log-dat ()
    "Retorna o caminho do ficheiro log.dat"
    (merge-pathnames "../log.dat" (caminho-raiz))
  )

;; Função que limpa o ficheiro experiencias
(defun limpar-log-dat ()
  "Limpa o ficheiro log.dat"
  (with-open-file (ficheiro (log-dat) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format ficheiro "")
    (close ficheiro)
  )
)

;;; Executar experiência 

;; Função que executa a experiência/resolução do problema fornecido
;; e escreve no saida fornecido

(defun executar-experiencia (problema)
    "Executa a experiencia/resolução do problema fornecido"
    (let ((resultado-bfs (executar-algoritmo-problema problema 'bfs))
          (resultado-dfs (executar-algoritmo-problema problema 'dfs 20))
          (resultado-aestrela (executar-algoritmo-problema problema 'aestrela 'heuristica-base)))
      (list problema resultado-bfs resultado-dfs resultado-aestrela)
    )
)

;; Função que executa o algoritmo de procura fornecido no problema fornecido
(defun executar-algoritmo-problema (problema algoritmo &optional (funcao-heuristica 'heuristica-base) (max-profundidade 20))
  "Executa o algoritmo de procura fornecido no problema fornecido"
  (cond ((null problema) nil)
        ((equal algoritmo 'aestrela) (funcall algoritmo (cria-no (problema-tabuleiro problema)) (cria-objetivo (problema-objetivo problema)) 'sucessores funcao-heuristica (operadores)))
        (t (funcall algoritmo (cria-no (problema-tabuleiro problema)) (cria-objetivo (problema-objetivo problema)) 'sucessores (operadores) max-profundidade))
  )
)

;;; Construtor 

;; Função que cria um problema
(defun cria-problema (nome tabuleiro objetivo)
  "Cria um problema"
  (list nome tabuleiro objetivo)
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

;; Função que seleciona o número de nós expandidos do resultado	
(defun resultado-nos-expandidos (resultado)
  "Seleciona o número de nós expandidos do resultado"
  (second resultado)
)

;; Função que seleciona o número de nós gerados do resultado
(defun resultado-nos-gerados (resultado)
  "Seleciona o número de nós gerados do resultado"
  (third resultado)
)

;; Função que seleciona a penetrância do resultado
(defun resultado-penetrancia (resultado)
  "Seleciona a penetrância do resultado"
  (fourth resultado)
)

;; Função que seleciona o factor de ramificação média do resultado
(defun resultado-ramificacao-media (resultado)
  "Seleciona o factor de ramificação média do resultado"
  (fifth resultado)
)

;; Função que seleciona o tempo de execução do resultado
(defun resultado-tempo-execucao (resultado)
  "Seleciona o tempo de execução do resultado"
  (sixth resultado)
)

;; Função que seleciona o problema da experiência
(defun experiencia-problema (experiencia)
  "Seleciona o problema da experiência"
  (first experiencia)
)

;; Função que seleciona o resultado do BFS da experiência
(defun experiencia-resultado-bfs (experiencia)
  "Seleciona o resultado do BFS da experiência"
  (second experiencia)
)

;; Função que seleciona o resultado do DFS da experiência
(defun experiencia-resultado-dfs (experiencia)
  "Seleciona o resultado do DFS da experiência"
  (third experiencia)
)

;; Função que seleciona o resultado do A* da experiência
(defun experiencia-resultado-aestrela (experiencia)
  "Seleciona o resultado do A* da experiência"
  (fourth experiencia)
)

;; Funções Auxiliares

;; Função que retorna o último id da experiência
;; do ficheiro experiencias.txt
(defun ultimo-id-experiencia ()
  "Retorna o último id da experiência do ficheiro experiencias.txt"
  (with-open-file (ficheiro (log-dat) :direction :input :if-does-not-exist :create)
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

;;; Leitura

;; Função que lê o ficheiro de problemas e devolve uma lista com todos os problemas
(defun ler-todos-problemas (&optional (i 1) problemas (ficheiro (problemas-dat)))
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
          ((equal linha (format nil "~a~a" "#" (write-to-string n))) (ler-propriedades-problema stream))
          (t (ler-ficheiro-problemas stream n ))
    )
  )
)

;; Função que lê as propriedades do problema e devolve uma lista com o nome,
;; o tabuleiro e o objetivo
(defun ler-propriedades-problema (stream &optional (nome "**SEM NOME**") (tabuleiro "") (objetivo nil))
  "Lê as propriedades do problema"
  (let ((linha (read-line stream nil)))
    (cond ((equal linha "") (ler-propriedades-problema stream nome tabuleiro objetivo))
          ((null linha) (list nome (read-from-string tabuleiro) objetivo))
          ((equal (subseq linha 0 1) "#") (list nome (read-from-string tabuleiro) objetivo))
          ((equal (subseq linha 0 5) "Nome:") (ler-propriedades-problema stream (subseq linha 6) tabuleiro objetivo))
          ((equal (subseq linha 0 9) "Objetivo:") (ler-propriedades-problema stream nome tabuleiro (parse-integer (subseq linha 10))))
          (t (ler-propriedades-problema stream nome (format nil "~a~a" tabuleiro linha) objetivo))
    )
  )
)

;;; Escrita

;; Função que escreve os passos do caminho no saida fornecido
(defun escreve-estados-resultado (no &optional (saida t))
  "Escreve os passos do caminho no saida fornecido"
  (cond ((null no) nil)
        (t (progn (escreve-estados-resultado (no-pai no) saida) (escreve-tabuleiro-formatado (no-estado no) saida) (format saida "~%")))
  )
)

;; Função que escreve o tempo no formato dd/mm/aaaa @ hh:mm:ss
;; no saida fornecido
(defun escreve-tempo (tempo-universal &optional saida)
  "Escreve o tempo fornecido no formato dd/mm/aaaa @ hh:mm:ss"
  (multiple-value-bind (segundos minutos horas dia mes ano) (decode-universal-time tempo-universal)
    (format saida "~2,'0d/~2,'0d/~2,'0d @ ~2,'0d:~2,'0d:~2,'0d" dia mes ano horas minutos segundos)
  )
)

;; Função que escreve a experiência/resolução do problema fornecido
;; no saida fornecido (por default escreve no ficheiro experiencias.txt)
(defun escreve-experiencia (experiencia &optional (saida (log-dat)))
  "Executa a experiencia/resolução do problema fornecido e escreve no ficheiro experiencias.txt"
  (let ((ultimo-id (ultimo-id-experiencia)))
    (cond ((pathnamep saida) 
          (with-open-file (ficheiro saida :direction :output :if-exists :append :if-does-not-exist :create)
            (escreve-conteudo-experiencia experiencia ficheiro ultimo-id)
            (close ficheiro)
          ))
          (t (escreve-conteudo-experiencia experiencia saida))
    )
  )
)

;; Função auxiliar que escreve o conteúdo da experiência/resolução do problema fornecido
;; no saida fornecido
(defun escreve-conteudo-experiencia (experiencia saida &optional (ultimo-id 0))
  "Escreve o conteúdo da experiencia/resolução do problema fornecido no saida fornecido"
  (cond ((not (= ultimo-id 0)) (format saida "~%~%")))
  (format saida "~45,1,,'.:@< Inicio experiencia ~>~%")
  (cond ((streamp saida) (format saida "#~a - ~a~%" (1+ ultimo-id) (escreve-tempo (get-universal-time)))))
  (format saida "~45,1,,'=:@< ~a ~>~%" (problema-nome (experiencia-problema experiencia)))
  (escreve-tabuleiro-formatado (problema-tabuleiro (experiencia-problema experiencia)) saida)
  (format saida "Objetivo: ~a~%" (problema-objetivo (experiencia-problema experiencia)))
  (format saida "~45,1,,'~:@< Algoritmos de procura ~>~%")
  (format saida "~45,1,,'-:@< BFS ~>~%" )
  (escreve-detalhes-resultado (experiencia-resultado-bfs experiencia) saida)
  (format saida "~45,1,,'-:@< DFS ~>~%" )
  (escreve-detalhes-resultado (experiencia-resultado-dfs experiencia) saida)
  (format saida "~45,1,,'-:@< A* (Heuristica Base) ~>~%" )
  (escreve-detalhes-resultado (experiencia-resultado-aestrela experiencia) saida)
  (format saida "~45,1,,'.:@< Fim experiencia ~>")
)

(defun escreve-detalhes-resultado (resultado saida)
  "Escreve os detalhes do resultado"
  (progn (format saida "Solucao: ") (cond ((not (null (resultado-no resultado))) (escreve-caminho (no-caminho (resultado-no resultado)) saida t)) (t (format saida "**SEM SOLUCAO**"))))
  ;(format saida "~%Pontuacao: ~a~%" (cond ((not (null (no-pontuacao (resultado-no resultado)))) (no-pontuacao (resultado-no resultado))) (t "**SEM SOLUCAO**")))
  (format saida "~%Nos expandidos: ~a~%" (resultado-nos-expandidos resultado))
  (format saida "Nos gerados: ~a~%" (resultado-nos-gerados resultado))
  (format saida "Penetrancia: ~,3f (~,1f%)~%" (resultado-penetrancia resultado) (* 100 (resultado-penetrancia resultado)))
  (format saida "Factor de ramificacao media: ~,3f (~,1f%)~%" (resultado-ramificacao-media resultado) (* 100 (resultado-ramificacao-media resultado)))
  (format saida "Tempo de execucao: ~,6f seg.~%" (resultado-tempo-execucao resultado))
)

;; Função que escreve o caminho percorrido
(defun escreve-caminho (caminho &optional (saida t) enrolar (i 0))
  "Função que escreve o caminho percorrido"
  (cond ((null caminho) nil)
        ((and enrolar (= i 9)) (progn (format saida "~%~9<~>") (escreve-caminho caminho saida enrolar 0)))
        ((= (length caminho) 1) (escreve-posicao (car caminho) saida))
        (t (progn (escreve-posicao (car caminho) saida) (format saida "->") (escreve-caminho (cdr caminho) saida enrolar (1+ i))))     
  )
)

(inicializar) ; Inicializa o programa automaticamente