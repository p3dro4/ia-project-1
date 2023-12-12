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
        (t (error "Falha ao carregar ficheiros!~%"))
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
  (progn (load (merge-pathnames "procura" caminho) :verbose nil) (format t "  ;; Ficheiro \"procura.lisp\" carregado.~%"))
  (progn (load (merge-pathnames "puzzle" caminho) :verbose nil) (format t "  ;; Ficheiro \"puzzle.lisp\" carregado.~%"))
)

;; Função que recarrega o ficheiro atual
(defun carregar-funcao-recarregar (caminho)
  "Recarrega o ficheiro atual"
  (defun recarregar ()
    (load caminho :verbose nil) 
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
    (merge-pathnames "log.dat" (caminho-raiz))
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

;; Função que seleciona o nome do algoritmo do resultado
(defun resultado-nome-algoritmo (resultado)
  "Seleciona o nome do algoritmo do resultado"
  (first resultado)
)

(defun resultado-conteudo (resultado)
  "Seleciona o conteúdo do resultado"
  (second resultado)
)

;; Função que seleciona o nó resultante do algoritmo de procura
(defun resultado-no (resultado)
  "Seleciona o nó resultante do algoritmo de procura"
  (first (resultado-conteudo resultado))
)

;; Função que seleciona o número de nós expandidos do resultado	
(defun resultado-nos-expandidos (resultado)
  "Seleciona o número de nós expandidos do resultado"
  (second (resultado-conteudo resultado))
)

;; Função que seleciona o número de nós gerados do resultado
(defun resultado-nos-gerados (resultado)
  "Seleciona o número de nós gerados do resultado"
  (third (resultado-conteudo resultado))
)

;; Função que seleciona a penetrância do resultado
(defun resultado-penetrancia (resultado)
  "Seleciona a penetrância do resultado"
  (fourth (resultado-conteudo resultado))
)

;; Função que seleciona o fator de ramificação média do resultado
(defun resultado-ramificacao-media (resultado)
  "Seleciona o fator de ramificação média do resultado"
  (fifth (resultado-conteudo resultado))
)

;; Função que seleciona o tempo de execução do resultado
(defun resultado-tempo-execucao (resultado)
  "Seleciona o tempo de execução do resultado"
  (sixth (resultado-conteudo resultado))
)

;; Função que seleciona o problema da experiência
(defun experiencia-problema (experiencia)
  "Seleciona o problema da experiência"
  (first experiencia)
)

;; Função que seleciona o resultado do BFS da experiência
(defun experiencia-resultado (experiencia)
  "Seleciona o resultado da experiência"
  (second experiencia)
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
  (cond ((not (= ultimo-id 0)) (format saida "~%")))
  (format saida "~45,1,,'.:@< Inicio experiencia ~>~%")
  (cond ((streamp saida) (format saida "#~a - ~a~%" (1+ ultimo-id) (escreve-tempo (get-universal-time)))))
  (format saida "~45,1,,'=:@< ~a ~>~%" (problema-nome (experiencia-problema experiencia)))
  (escreve-tabuleiro-formatado (problema-tabuleiro (experiencia-problema experiencia)) saida)
  (format saida "Objetivo: ~a~%" (problema-objetivo (experiencia-problema experiencia)))
  (format saida "~45,1,,'~:@< Algoritmo de procura ~>~%")
  (mapcar (lambda (resultado) 
      (progn
        (format saida "~45,1,,'-:@< ~a ~>~%" (resultado-nome-algoritmo resultado))
        (escreve-detalhes-resultado resultado saida)
      )
    ) (experiencia-resultado experiencia))
  (format saida "~45,1,,'.:@< Fim experiencia ~>~%")
)

(defun escreve-detalhes-resultado (resultado saida)
  "Escreve os detalhes do resultado"
  (progn (format saida "Solucao: ") (cond ((not (null (resultado-no resultado))) (escreve-caminho (no-caminho (resultado-no resultado)) saida t)) (t (format saida "**SEM SOLUCAO**"))))
  (format saida "~%Nos expandidos: ~a~%" (resultado-nos-expandidos resultado))
  (format saida "Nos gerados: ~a~%" (resultado-nos-gerados resultado))
  (format saida "Penetrancia: ~,3f (~,1f%)~%" (resultado-penetrancia resultado) (* 100 (resultado-penetrancia resultado)))
  (format saida "Fator de ramificacao media: ~,3f (~,1f%)~%" (resultado-ramificacao-media resultado) (* 100 (resultado-ramificacao-media resultado)))
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

;;; Interface com o utilizador

;; Função que inicializa a interface com o utilizador
(defun iniciar ()
  "Função que inicializa a interface com o utilizador"
  (format t "~45,1,,'#:@< Jogo do Cavalo ~>~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "#~43,1,,:@<1 - Criar problema   ~>#~%")
  (format t "#~43,1,,:@<2 - Escolher problema~>#~%")
  (format t "#~43,1,,:@<0 - Sair da aplicacao~>#~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "~45,1,,'#<~%~>")
  (let ((opcao (ler-opcao 2)))
    (cond ((= opcao 1) (format t "Operacao nao implementada!~%"))
          ((= opcao 2) (escolher-problema))
          ((= opcao 0) (format t "A sair...~%"))
    )
  )
)

(defun ler-opcao (limite &optional (prompt "Escolha > "))
  "Lê a opção do utilizador"
  (format t "~a" prompt)
  (let ((opcao (read)))
    (cond ((and (integerp opcao) (<= 0 opcao limite)) opcao)
          (t (progn (format t "Opcao invalida!~%") (ler-opcao limite prompt)))
    )
  )
)

(defun escolher-problema ()
  "Função que permite escolher um problema"
  (let ((problemas (ler-todos-problemas)))
    (format t "~45,1,,'#:@< Escolher problema ~>~%")
    (format t "#~43,1,,:@<~>#~%")
    (escrever-nomes-problemas problemas)
    (format t "#~43,1,,:@<0 - Voltar   ~>#~%")
    (format t "#~43,1,,:@<~>#~%")
    (format t "~45,1,,'#<~%~>")
    (let ((opcao (ler-opcao (length problemas) "Problema > ")))
      (cond ((= opcao 0) (iniciar))
            (t (escolher-algoritmo (nth (1- opcao) problemas)))
      )
    )
  )
)

(defun escolher-algoritmo (problema)
  "Função que permite escolher um algoritmo"
  (format t "~45,1,,'#:@< ~a ~>~%" (problema-nome problema))
  (format t "#~43,1,,:@<~>#~%")
  (format t "#~43,1,,:@<Algoritmo a usar:~>#~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "#~43,1,,:@<1 - Procura na largura (BFS)~>#~%")
  (format t "#~43,1,,:@<2 - Procura na profundidade (DFS)~>#~%")
  (format t "#~43,1,,:@<3 - Algoritmo A*~>#~%")
  (format t "#~43,1,,:@<4 - Todos os algoritmos~>#~%")
  (format t "#~43,1,,:@<0 - Voltar~>#~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "~45,1,,'#<~%~>")
  (let ((opcao (ler-opcao 4)))
    (cond ((= opcao 0) (escolher-problema))
          ((= opcao 1) 
            (let* ((resultado (executar-algoritmo-problema problema 'bfs))
                   (experiencia (list problema (list (list "Procura na largura (BFS)" resultado)))))
              (escreve-experiencia experiencia t)
              (escreve-experiencia experiencia)
              (executar-outro-algoritmo problema)
            )
          )
          ((= opcao 2) (escolher-profundidade problema))
          ((= opcao 3) (escolher-heuristica problema))
          ((= opcao 4)
            (let* ((profundidade (escolher-profundidade problema t))
                   (resultado-bfs (executar-algoritmo-problema problema 'bfs))
                   (resultado-dfs (executar-algoritmo-problema problema 'dfs profundidade))
                   (resultado-aestrela (executar-algoritmo-problema problema 'aestrela 'heuristica-base))
                   (experiencia (list problema (list (list "Procura na largura (BFS)" resultado-bfs) (list "Procura na profundidade (DFS)" resultado-dfs) (list "A* (Heuristica Base)" resultado-aestrela)))))
              (escreve-experiencia experiencia t)
              (escreve-experiencia experiencia)
              (executar-outro-algoritmo problema)
            )
          )
    )
  )
)

(defun escrever-nomes-problemas (problemas &optional (i 1))
  "Escreve os nomes dos problemas"
  (cond ((null problemas) nil)
        (t (progn (format t "#~43,1,,:@<~a - ~a~>#~%" i (problema-nome (car problemas))) (escrever-nomes-problemas (cdr problemas) (1+ i))))
  )
)

(defun escolher-profundidade (problema &optional retornar-profundidade)
  "Função que permite escolher a profundidade máxima"
  (format t "Profundidade maxima > ")
  (let ((profundidade (read)))
    (cond ((and (integerp profundidade) (> profundidade 0)) 
            (cond (retornar-profundidade profundidade)
                  (t (let* ((resultado (executar-algoritmo-problema problema 'dfs nil profundidade))
                        (experiencia (list problema (list (list "Procura na profundiade (DFS)" resultado)))))
                    (escreve-experiencia experiencia t)
                    (escreve-experiencia experiencia)
                    (executar-outro-algoritmo problema)
                    )
                  )
            )
          )
          (t (progn (format t "~%Profundidade invalida!~%") (escolher-profundidade problema)))
    )
  )
)

(defun escolher-heuristica (problema)
  "Função que permite escolher a heurística"
  (format t "~45,1,,'#:@< ~a ~>~%" (problema-nome problema))
  (format t "#~43,1,,:@<~>#~%")
  (format t "#~43,1,,:@<Heuristica a usar:~>#~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "#~43,1,,:@<1 - Heuristica base~>#~%")
  (format t "#~43,1,,:@<0 - Voltar~>#~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "~45,1,,'#<~%~>")
  (let ((opcao (ler-opcao 1)))
    (cond ((= opcao 0) (escolher-algoritmo problema))
          ((= opcao 1) 
            (let* ((resultado (executar-algoritmo-problema problema 'aestrela 'heuristica-base))
                   (experiencia (list problema (list (list "A* (Heuristica Base)" resultado)))))
              (escreve-experiencia experiencia t)
              (escreve-experiencia experiencia)
              (executar-outro-algoritmo problema)
            )
          )
    )
  )
)

(defun voltar-ao-menu ()
  "Função que permite voltar ao menu"
  (format t "Voltar ao menu? [sim(1) / nao(0)] > ")
  (let ((opcao (read)))
    (cond ((= opcao 1) (iniciar))
          ((= opcao 0) (format t "A sair...~%"))
          (t (voltar-ao-menu))
    )
  )
)

(defun executar-outro-algoritmo (problema)
  "Função que permite executar outro algoritmo para um mesmo problema"
  (format t "Aplicar outro algoritmo ao mesmo problema?~%[sim(1) / nao(0)] > ")
  (let ((opcao (read)))
    (cond ((= opcao 1) (escolher-algoritmo problema))
          ((= opcao 0) (voltar-ao-menu))
          (t (voltar-ao-menu))
    )
  )
)

(inicializar) ; Inicializa o programa automaticamente