;;;; projeto.lisp
;;;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro

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

;;; Executar algoritmo de procura

;; Função que executa o algoritmo de procura fornecido no problema fornecido
(defun executar-algoritmo-problema (problema algoritmo &key funcao-heuristica max-profundidade)
  "Executa o algoritmo de procura fornecido no problema fornecido"
  (cond ((null problema) nil)
        ((equal algoritmo 'aestrela) (funcall algoritmo (cria-no (problema-tabuleiro problema)) (cria-objetivo (problema-objetivo problema)) 'sucessores funcao-heuristica (operadores)))
        ((equal algoritmo 'dfs) (funcall algoritmo (cria-no (problema-tabuleiro problema)) (cria-objetivo (problema-objetivo problema)) 'sucessores (operadores) max-profundidade))
        (t (funcall algoritmo (cria-no (problema-tabuleiro problema)) (cria-objetivo (problema-objetivo problema)) 'sucessores (operadores)))
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

;; Função que retorna o último id
;; do ficheiro experiencias.txt
(defun ultimo-id (&optional (caminho (log-dat)))
  "Retorna o último id da experiência do ficheiro experiencias.txt"
  (with-open-file (ficheiro caminho :direction :input :if-does-not-exist :create)
    (ultimo-id-ficheiro ficheiro)
  )
)

;; Função axuiliar que retorna o último id
(defun ultimo-id-ficheiro (ficheiro &optional (ultimo-id 0))
  "Retorna o último id da experiência"
  (let ((linha (read-line ficheiro nil)))
    (cond ((null linha) ultimo-id)
          ((equal linha "") (ultimo-id-ficheiro ficheiro ultimo-id))
          ((equal (subseq linha 0 1) "#") (ultimo-id-ficheiro ficheiro (obter-id-linha linha ultimo-id)))
          (t (ultimo-id-ficheiro ficheiro ultimo-id))
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
(defun ler-todos-problemas (&optional (ficheiro (problemas-dat)) (i 1) problemas )
  "Lê todos os problemas"
  (let ((problema (ler-problema i ficheiro)))
    (cond ((null problema) problemas)
          (t (ler-todos-problemas ficheiro (+ i 1) (append problemas (list problema))))
    )
  )
)

;; Função que lê o ficheiro de problemas e devolve o problema n
(defun ler-problema (n &optional (ficheiro (problemas-dat)))
  "Lê o ficheiro de problemas e devolve o problema n"
  (with-open-file (ficheiro ficheiro :direction :input :if-does-not-exist nil)
    (ler-ficheiro-problemas ficheiro n)
  )
)

;; Função que lê as linhas do ficheiro de problemas e se encontrar o problema n
;; chama a função ler-propriedades-problema
(defun ler-ficheiro-problemas (ficheiro n)
  "Lê as linhas do ficheiro de problemas"
  (let ((linha (read-line ficheiro nil)))
    (cond ((null linha) nil)
          ((equal linha (format nil "~a~a" "#" (write-to-string n))) (ler-propriedades-problema ficheiro))
          (t (ler-ficheiro-problemas ficheiro n ))
    )
  )
)

;; Função que lê as propriedades do problema e devolve uma lista com o nome,
;; o tabuleiro e o objetivo
(defun ler-propriedades-problema (ficheiro &optional (nome "**SEM NOME**") (tabuleiro "") (objetivo nil))
  "Lê as propriedades do problema"
  (let ((linha (read-line ficheiro nil)))
    (cond ((equal linha "") (ler-propriedades-problema ficheiro nome tabuleiro objetivo))
          ((null linha) (list nome (read-from-string tabuleiro) objetivo))
          ((equal (subseq linha 0 1) "#") (list nome (read-from-string tabuleiro) objetivo))
          ((equal (subseq linha 0 5) "Nome:") (ler-propriedades-problema ficheiro (subseq linha 6) tabuleiro objetivo))
          ((equal (subseq linha 0 9) "Objetivo:") (ler-propriedades-problema ficheiro nome tabuleiro (parse-integer (subseq linha 10))))
          (t (ler-propriedades-problema ficheiro nome (format nil "~a~a" tabuleiro linha) objetivo))
    )
  )
) 

(defun restaurar-backup-problemas-dat (&optional (ficheiro (merge-pathnames "problemas.bak" (caminho-raiz))))
  "Restaura o ficheiro problemas.dat"
  (let ((problemas (ler-todos-problemas ficheiro)))
    (with-open-file (ficheiro (problemas-dat) :direction :output :if-exists :supersede :if-does-not-exist :create)
      (restaurar-backup-problemas problemas ficheiro)
      (close ficheiro)
    )
  )
)

(defun restaurar-backup-problemas (problemas ficheiro &optional (i 1))
  "Restaura os problemas no ficheiro problemas.dat"
  (cond ((null problemas) nil)
        (t (progn (escreve-problema (car problemas) ficheiro i) (restaurar-backup-problemas (cdr problemas) ficheiro (1+ i))))
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

;; Função que escreve o caminho percorrido
(defun escreve-caminho (caminho &optional (saida t) enrolar (i 0))
  "Função que escreve o caminho percorrido"
  (cond ((null caminho) nil)
        ((and enrolar (= i 9)) (progn (format saida "~%~9<~>") (escreve-caminho caminho saida enrolar 0)))
        ((= (length caminho) 1) (escreve-posicao (car caminho) saida))
        (t (progn (escreve-posicao (car caminho) saida) (format saida "->") (escreve-caminho (cdr caminho) saida enrolar (1+ i))))     
  )
)

;; Função que escreve a experiência/resolução do problema fornecido
;; no saida fornecido (por default escreve no ficheiro experiencias.txt)
(defun escreve-experiencia (experiencia &optional (saida (log-dat)))
  "Executa a experiencia/resolução do problema fornecido e escreve no ficheiro experiencias.txt"
  (let ((id (ultimo-id)))
    (cond ((>= id 25) (limpar-log-dat)))
    (cond ((pathnamep saida) 
          (with-open-file (ficheiro saida :direction :output :if-exists :append :if-does-not-exist :create)
            (escreve-conteudo-experiencia experiencia ficheiro id)
            (close ficheiro)
          ))
          (t (escreve-conteudo-experiencia experiencia saida))
    )
  )
)

;; Função auxiliar que escreve o conteúdo da experiência/resolução do problema fornecido
;; no saida fornecido
(defun escreve-conteudo-experiencia (experiencia saida &optional (id 0))
  "Escreve o conteúdo da experiencia/resolução do problema fornecido no saida fornecido"
  (cond ((not (= id 0)) (format saida "~%")))
  (format saida "~45,1,,'.:@< Inicio experiencia ~>~%")
  (cond ((streamp saida) (format saida "#~a - ~a~%" (1+ id) (escreve-tempo (get-universal-time)))))
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

;; Função que escreve os detalhes do resultado
(defun escreve-detalhes-resultado (resultado saida)
  "Escreve os detalhes do resultado"
  (progn (format saida "Solucao: ") (cond ((not (null (resultado-no resultado))) (escreve-caminho (no-caminho (resultado-no resultado)) saida t)) (t (format saida "**SEM SOLUCAO**"))))
  (format saida "~%Nos expandidos: ~a~%" (resultado-nos-expandidos resultado))
  (format saida "Nos gerados: ~a~%" (resultado-nos-gerados resultado))
  (format saida "Penetrancia: ~,3f (~,1f%)~%" (resultado-penetrancia resultado) (* 100 (resultado-penetrancia resultado)))
  (format saida "Fator de ramificacao media: ~,3f (~,1f%)~%" (resultado-ramificacao-media resultado) (* 100 (resultado-ramificacao-media resultado)))
  (format saida "Tempo de execucao: ~,6f seg.~%" (resultado-tempo-execucao resultado))
)

;; Função que escreve o problema fornecido
(defun escreve-problema (problema &optional (saida t) id)
  "Escreve o problema fornecido"
  (cond ((not (null id))
          (cond ((not (= id 1)) (format saida "~%#~a~%" id))
                (t (format saida "#~a~%" id))))
  )
  (format saida "Nome: ~a~%" (problema-nome problema))
  (progn (format saida "(") (escreve-tabuleiro (problema-tabuleiro problema) saida) (format saida ")~%"))
  (format saida "Objetivo: ~a~%" (problema-objetivo problema))
)

(defun escreve-problema-formatado (problema &optional (saida t))
  "Escreve o problema fornecido"
  (format t "~45,1,,'#:@< ~a ~>~%" (problema-nome problema))
  (escreve-tabuleiro-formatado (problema-tabuleiro problema))
  (format t "Objetivo: ~a~%" (problema-objetivo problema))
)

;; Função que escreve o problema no ficheiro problemas.dat
(defun escreve-problema-ficheiro (problema &optional (saida (problemas-dat)))
  "Escreve o problema no ficheiro problemas.dat"
  (let ((id (1+ (ultimo-id (problemas-dat)))))
    (cond ((existe-problema-nome (problema-nome problema)) (reescrever-problema problema saida))
          (t (with-open-file (ficheiro saida :direction :io :if-exists :append :if-does-not-exist :create)
                (progn (escreve-problema problema ficheiro id) (close ficheiro))))
    )
  )
)

;; Função que reescreve o problema no ficheiro problemas.dat
(defun reescrever-problema (problema saida)
  "Rescreve o problema no ficheiro problemas.dat"
  (let ((problemas (ler-todos-problemas)))
    (with-open-file (ficheiro saida :direction :output :if-exists :supersede :if-does-not-exist :create)
      (procurar-e-rescrever problema ficheiro problemas)
      (close ficheiro)
    )
  )
)

;; Função que procura e rescreve o problema no ficheiro problemas.dat
(defun procurar-e-rescrever (problema ficheiro problemas &optional (id 1))
  "Procura e rescreve o problema no ficheiro problemas.dat"
  (cond ((null problemas) nil)
        ((equal (problema-nome problema) (problema-nome (car problemas))) (progn (escreve-problema (list (problema-nome (car problemas)) (problema-tabuleiro problema) (problema-objetivo problema)) ficheiro id) (procurar-e-rescrever problema ficheiro (cdr problemas) (1+ id))))
        (t (progn (escreve-problema (car problemas) ficheiro id) (procurar-e-rescrever problema ficheiro (cdr problemas) (1+ id))))
  )
)

;; Função que verifica se existe um problema com o nome fornecido
(defun existe-problema-nome (nome &optional (problemas (ler-todos-problemas)))
  "Verifica se existe um problema com o nome fornecido"
  (cond ((null problemas) nil)
        ((equal (string-capitalize nome) (string-capitalize (problema-nome (car problemas)))) t)
        (t (existe-problema-nome nome (cdr problemas)))
  )
)

;;; Interface com o utilizador

;; Função que inicializa a interface com o utilizador
(defun iniciar ()
  "Função que inicializa a interface com o utilizador"
  (format t "~45,1,,'#:@< Jogo do Cavalo ~>~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "#~43,1,,:@<1 - Gerir problemas   ~>#~%")
  (format t "#~43,1,,:@<2 - Escolher problema~>#~%")
  (format t "#~43,1,,:@<0 - Sair da aplicacao~>#~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "~45,1,,'#<~%~>")
  (let ((opcao (ler-opcao 2)))
    (cond ((= opcao 1) (gerir-problemas))
          ((= opcao 2) (escolher-problema))
          ((= opcao 0) (format t "A sair...~%"))
    )
  )
)

(defun gerir-problemas ()
  "Função que permite gerir os problemas"
  (format t "~45,1,,'#:@< Gerir problemas ~>~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "#~43,1,,:@<1 - Criar problema~>#~%")
  (format t "#~43,1,,:@<2 - Listar problemas~>#~%")
  (format t "#~43,1,,:@<3 - Restaurar backup~>#~%")
  (format t "#~43,1,,:@<0 - Voltar~>#~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "~45,1,,'#<~%~>")
  (let ((opcao (ler-opcao 3)))
    (cond ((= opcao 1) (criar-problema))
          ((= opcao 2) (listar-problemas))
          ((= opcao 3) (restaurar-backup))
          ((= opcao 0) (iniciar))
    )
  )
)

(defun restaurar-backup ()
  "Função que permite restaurar o backup"
  (format t "Restaurar backup? [sim(1) / nao(0)] > ")
  (let ((opcao (read)))
    (cond ((and (integerp opcao) (or (= opcao 0) (= opcao 1)))
            (cond ((= opcao 1) (progn (restaurar-backup-problemas-dat) (format t "Backup restaurado!~%") (gerir-problemas)))
                  ((= opcao 0) (voltar-ao-menu))
            ))
          (t (restaurar-backup))
    )
  )
)

(defun criar-problema ()
  "Função que permite criar um problema"
  (format t "~45,1,,'#:@< Criar problema ~>~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "#~43,1,,:@<1 - Introduzir tabuleiro~>#~%")
  (format t "#~43,1,,:@<2 - Gerar tabuleiro aleatorio~>#~%")
  (format t "#~43,1,,:@<0 - Voltar~>#~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "~45,1,,'#<~%~>")
  (let ((opcao (ler-opcao 2)))
    (cond ((= opcao 0) (gerir-problemas))
          ((= opcao 1) (progn (format t "Operacao nao implementada!~%") (criar-problema)))
          ((= opcao 2) (criar-problema-gerar-tabuleiro))
    )
  )
)

(defun criar-problema-gerar-tabuleiro ()
  "Função que permite criar um problema, ao gerar um tabuleiro aleatório"
  (let* ((nome (escolher-nome))
         (tabuleiro (tabuleiro-aleatorio))
         (objetivo (escolher-objetivo)))
    (format t "~45,1,,'#:@< ~a ~>~%" nome)
    (escreve-tabuleiro-formatado tabuleiro)
    (format t "Objetivo: ~a~%" objetivo)
    (escolher-guardar-problema (cria-problema nome tabuleiro objetivo))
  )
)

(defun escolher-nome ()
  "Função que permite escolher um nome"
  (format t "Nome do problema > ")
  (let ((nome (read-line)))
    (cond ((equal nome "") (progn (format t "Nome invalido!~%") (escolher-nome)))
          ((existe-problema-nome nome) (cond ((escolher-rescrever-problema) nome) (t (progn (format t "Escolha um nome diferente!~%") (escolher-nome)))))
          (t nome)
    )
  )
)

(defun escolher-rescrever-problema ()
  "Função que permite escolher se quer rescrever o problema"
  (format t "Rescrever problema? [sim(1) / nao(0)] > ")
  (let ((opcao (read)))
    (cond ((and (integerp opcao) (or (= opcao 0) (= opcao 1)))
            (cond ((= opcao 1) t)
                  ((= opcao 0) nil)
                  (t (escolher-rescrever-problema))
            ))
          (t (escolher-rescrever-problema))
    )
  )
)

(defun escolher-guardar-problema (problema)
  "Função que permite escolher se quer guardar o problema"
  (format t "Guardar problema? [sim(1) / nao(0)] > ")
  (let ((opcao (read)))
    (cond ((and (integerp opcao) (or (= opcao 0) (= opcao 1)))
            (cond ((= opcao 1) (progn (escreve-problema-ficheiro problema) (format t "Ficheiro atualizado!~%") (voltar-ao-menu)))
                  ((= opcao 0) nil)
                  (t (escolher-guardar-problema problema))
            ))
          (t (escolher-guardar-problema problema))
    )
  )
)

(defun escolher-objetivo ()
  "Função que permite escolher um objetivo"
  (format t "Objetivo do problema > ")
  (let ((objetivo (read)))
    (cond ((and (integerp objetivo) (>= objetivo 0)) objetivo)
          ((equal objetivo "") (progn (format t "Objetivo invalido!~%") (escolher-objetivo)))
          (t (progn (format t "Objetivo invalido!~%") (escolher-objetivo)))
    )
  )
)

(defun listar-problemas ()
  "Função que permite listar os problemas"
  (let ((problemas (ler-todos-problemas)))
    (mapcar (lambda (problema) (progn (escreve-problema-formatado problema)(format t "~%"))) problemas)
    (voltar-ao-menu)
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
    (format t "#~43,1,,:@<0 - Voltar~>#~%")
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
  (let ((opcao (ler-opcao 4 "Algoritmo > ")))
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
                   (resultado-dfs (executar-algoritmo-problema problema 'dfs :max-profundidade profundidade))
                   (resultado-aestrela-base (executar-algoritmo-problema problema 'aestrela :funcao-heuristica (lambda (estado pontuacao) (funcall 'heuristica-base estado (problema-objetivo problema) pontuacao))))
                   (resultado-aestrela-implementada (executar-algoritmo-problema problema 'aestrela :funcao-heuristica (lambda (estado pontuacao) (funcall 'heuristica-implementada estado (problema-objetivo problema) pontuacao))))
                   (experiencia (list problema (list (list "Procura na largura (BFS)" resultado-bfs) (list "Procura na profundidade (DFS)" resultado-dfs) (list "A* (Heuristica Base)" resultado-aestrela-base) (list "A* (Heuristica Implementada)" resultado-aestrela-implementada)))))
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
                  (t (let* ((resultado (executar-algoritmo-problema problema 'dfs :max-profundidade profundidade))
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
  (format t "#~43,1,,:@<2 - Heuristica implementada~>#~%")
  (format t "#~43,1,,:@<0 - Voltar~>#~%")
  (format t "#~43,1,,:@<~>#~%")
  (format t "~45,1,,'#<~%~>")
  (let ((opcao (ler-opcao 2 "Heuristica > ")))
    (cond ((= opcao 0) (escolher-algoritmo problema))
          ((= opcao 1) 
            (let* ((resultado (executar-algoritmo-problema problema 'aestrela :funcao-heuristica (lambda (estado pontuacao) (funcall 'heuristica-base estado (problema-objetivo problema) pontuacao))))
                   (experiencia (list problema (list (list "A* (Heuristica Base)" resultado)))))
              (escreve-experiencia experiencia t)
              (escreve-experiencia experiencia)
              (executar-outro-algoritmo problema)
            )
          )
          ((= opcao 2) 
            (let* ((resultado (executar-algoritmo-problema problema 'aestrela :funcao-heuristica (lambda (estado pontuacao) (funcall 'heuristica-implementada estado (problema-objetivo problema) pontuacao))))
                   (experiencia (list problema (list (list "A* (Heuristica Implementada)" resultado)))))
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
    (cond ((and (integerp opcao) (or (= opcao 0) (= opcao 1)))
            (cond ((= opcao 1) (iniciar))
                  ((= opcao 0) (format t "A sair...~%"))
                  (t (voltar-ao-menu))
            ))
          (t (voltar-ao-menu))
    )
  )
)

(defun executar-outro-algoritmo (problema)
  "Função que permite executar outro algoritmo para um mesmo problema"
  (format t "Aplicar outro algoritmo ao mesmo problema?~%[sim(1) / nao(0)] > ")
  (let ((opcao (read)))
    (cond ((and (integerp opcao) (or (= opcao 0) (= opcao 1)))
            (cond ((= opcao 1) (escolher-algoritmo problema))
                  ((= opcao 0) (voltar-ao-menu))
                  (t (voltar-ao-menu))
            ))
          (t (executar-outro-algoritmo problema)))
  )
)

(inicializar) ; Inicializa o programa automaticamente