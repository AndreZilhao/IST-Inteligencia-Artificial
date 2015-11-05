;(load "utils.lisp")

; TAI - accao
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



(defun maplinha (linha)
	(- 17 linha))


; TAI - tabuleiro
; Representacao interna : Uma lista que ira conter todas as linhas do jogo, assim tem 18 posicoes.
; Cada linha do jogo, ira ser representada igualmente por uma lista. Cada linha (cada elemento da lista anterior) varias listas em que cada uma destas listas ira representar uma linha
; do jogo do tetris. 
;, a posicao vazia e representada por uma string vazia : ""


; Construtor

; cria-accao : coluna x configuracao-geometrica-peca 
; Coluna --> inteiro [0,9] que representa a coluna mais a esquerda para a peca cair 
; configuracao-geometrica-peca --> Array bidimensional que indica a representacao da peca rodada depois de cair
(defun cria-tabuleiro ()
	(make-array (list 18 10) :initial-element nil))

(defun copia-tabuleiro (tabuleiro-a-copiar)
	(let ((novo-tabuleiro (cria-tabuleiro)))
		(loop for nlinha from 0 to 17 do
      		  (loop for ncoluna from 0 to 9 do
      		  		(setf (aref novo-tabuleiro nlinha ncoluna) (aref tabuleiro-a-copiar nlinha ncoluna))))
		novo-tabuleiro))

(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
		(not (equal (aref tabuleiro (maplinha linha) coluna) nil)))

(defun altura-coluna (tabuleiro coluna)
		(let ((conta-coluna 0))
			 (loop for nlinha from 0 to 17 
			 	while (equal (tabuleiro-preenchido-p tabuleiro nlinha coluna) nil) do
			 		(setf conta-coluna nlinha))
			 conta-coluna))
				



(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(if (and (numberp linha) (numberp coluna)
			 (>= linha 0) (<= linha 17)
			 (>= coluna 0) (<= coluna 9))
		(setf (aref tabuleiro (maplinha linha) coluna) T)))		
   


;	loop from
;	(setf (aref b 7 2) T)


;	(let ((novo-tabuleiro tabuleiro-a-copiar)) 
;		novo-tabuleiro))
  