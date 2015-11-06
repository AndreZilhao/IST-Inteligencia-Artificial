;(load "utils.lisp")


;; Numero de colunas do tabuleiro
(defparameter *num-colunas* 10)
;; Numero de linhas do tabuleiro
(defparameter *num-linhas* 18)

;; Indices maximos das colunas e das linhas do tabuleiro
(defparameter *max-colunas-index* 9)
(defparameter *max-linhas-index* 17)

; Funcao auxiliar que mapeia a numeracao das linhas requerida no enunciado em que a linha 0 encontra-se 
; na posicaoo mais 'abaixo'
(defun maplinha (linha)
	(- *max-linhas-index* linha))

; TAI - accao
;
; Representacao interna : Um par com dois elementos
; Construtor
; cria-accao : coluna x configuracao-geometrica-peca 
; Coluna --> inteiro [0,9] que representa a coluna mais a esquerda para a peca cair 
; configuracao-geometrica-peca --> Array bidimensional que indica a representacao da peca rodada depois de cair
(defun cria-accao (coluna configuracao-geometrica-peca )
	(cons coluna configuracao-geometrica-peca))

; Selectores

; accao-coluna : accao --> inteiro [0,9]  
; Recebe uma accao e devolve a coluna mais a esquerda para a peca cair
(defun accao-coluna (accao)
	(car accao))

; accao-peca : accao --> Array bidimensional que indica a representacao da peca rodada depois de cair  
; Recebe uma accao e devolve a representacao da peca
(defun accao-peca (accao)
	(cdr accao))



; TAI - tabuleiro
; Representacao interna : Uma lista que ira conter todas as linhas do jogo, assim tem 18 posicoes.
; Cada linha do jogo, ira ser representada igualmente por uma lista. Cada linha (cada elemento da lista anterior) varias listas em que cada uma destas listas ira representar uma linha
; do jogo do tetris. A posicao vazia e representada por nil


; Construtor

; cria-accao : coluna x configuracao-geometrica-peca 
; Coluna --> inteiro [0,9] que representa a coluna mais a esquerda para a peca cair 
; configuracao-geometrica-peca --> Array bidimensional que indica a representacao da peca rodada depois de cair
(defun cria-tabuleiro ()
	(make-array (list *num-linhas* *num-colunas*) :initial-element nil))

; cria-accao : coluna x configuracao-geometrica-peca 
; Coluna --> inteiro [0,9] que representa a coluna mais a esquerda para a peca cair 
; configuracao-geometrica-peca --> Array bidimensional que indica a representacao da peca rodada depois de cair
(defun copia-tabuleiro (tabuleiro-a-copiar)
	(let ((novo-tabuleiro (cria-tabuleiro)))
		(loop for linha-actual from 0 to *max-linhas-index* do
      		  (loop for coluna-actual from 0 to *max-colunas-index* do
      		  		(setf (aref novo-tabuleiro linha-actual coluna-actual) (aref tabuleiro-a-copiar linha-actual coluna-actual))))
		novo-tabuleiro))

(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
		(not (equal (aref tabuleiro (maplinha linha) coluna) nil)))

;(defun altura-coluna (tabuleiro coluna)
;		(let ((conta-coluna 0))
;			 (loop  for linha-actual from 0 to *max-linhas-index* 
;			 			until (equal (aref tabuleiro linha-actual coluna) T) do ; until (> linha-actual 10) do 
;			 	    		(progn 
;			 	    			;(princ (equal (aref tabuleiro linha-actual coluna) nil))
;			 	    			(princ "Linha actual")
;			 	    			(princ linha-actual)
			 	    			;(princ conta-coluna)
			 	    			;(princ " ") 
;			 	    			(setf conta-coluna (+ conta-coluna 1))

;			 	    		))
;			 (maplinha conta-coluna)))
				
(defun altura-coluna (tabuleiro coluna)
		(let ((conta-coluna 0))
			 
			 (loop  for linha-actual from 0 to *max-linhas-index* 
			 	   	    until (equal (aref tabuleiro linha-actual coluna) T) do
			 	        	(setf conta-coluna (+ conta-coluna 1)))
			 
			(let ((linha-resultado (maplinha conta-coluna)))
			 	 	(if (< linha-resultado 0)
			 	 		0
			 	 		linha-resultado))))
				



;(defun altura-coluna (tabuleiro coluna)
;		(let ((conta-coluna 0))
;			 (progn
;			 	(dotimes *max-linhas-index*) 
;				 	while (equal (tabuleiro-preenchido-p tabuleiro nlinha coluna) nil) do
;			 			(setf conta-coluna nlinha)
;			 conta-coluna)))

(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(if (and (numberp linha) (numberp coluna)
			 (>= linha 0) (<= linha *max-linhas-index*)
			 (>= coluna 0) (<= coluna *max-colunas-index*))
		(setf (aref tabuleiro (maplinha linha) coluna) T)))		
   


;	loop from
;	(setf (aref b 7 2) T)


;	(let ((novo-tabuleiro tabuleiro-a-copiar)) 
;		novo-tabuleiro))
  