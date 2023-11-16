;;;; projeto.lisp
;;;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
;;;; Autores: 202100230 - Pedro Anjos, 202100225 - André Meseiro
(defun carregar-ficheiros ()
  (format t ";; A carregar ficheiros...~%")
  (cond ((null *load-pathname*) nil)
        (t (let ((load-path *load-pathname*))
              (load (merge-pathnames "procura" load-path))
              (load (merge-pathnames "puzzle" load-path)))))
  (cond ((null *compile-file-pathname*) nil)
        (t (let ((load-path *compile-file-pathname*))
              (load (merge-pathnames "procura" load-path))
              (load (merge-pathnames "puzzle" load-path)))))
  (format t ";; Ficheiros carregados.~%")
)

(carregar-ficheiros)