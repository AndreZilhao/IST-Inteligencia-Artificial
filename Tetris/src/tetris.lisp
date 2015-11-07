(load "utils.lisp")


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
; Representacao interna : Um array bidimensional que ira conter todas as linhas do jogo, assim tem 18 posicoes.
; Assim, cada linha do jogo, ira ser representado por um array. A posicao vazia e representada por nil e a ocupada por T


; Construtores

; cria-tabuleiro 
; Retorna um array bidimensional (18 x 10) com todas as posicoes a nil
(defun cria-tabuleiro ()
	(make-array (list *num-linhas* *num-colunas*) :initial-element nil))

; cria-accao : tabuleiro-a-copiar --> tabuleiro - copia do tabuleiro-a-copiar
; tabuleiro-a-copiar --> array bidimensional que representa o tabuleiro a ser copiado
(defun copia-tabuleiro (tabuleiro-a-copiar)
	(let ((novo-tabuleiro (cria-tabuleiro)))

		(loop for linha-actual from 0 to *max-linhas-index* do
      		  (loop for coluna-actual from 0 to *max-colunas-index* do
      		  		(setf (aref novo-tabuleiro linha-actual coluna-actual) (aref tabuleiro-a-copiar linha-actual coluna-actual))))

		novo-tabuleiro))


; Selectores
; tabuleiro-preenchido-p : tabuleiro x linha x coluna --> retorna T ou nil consoante a posicao a verificar esteja ou nao ocupada
; tabuleiro --> array bidimensional que representa um tabuleiro
; linha --> inteiro [0,17] que representa a linha do tabuleiro
; coluna --> inteiro [0,9] que representa a coluna do tabuleiro
(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
		(not (equal (aref tabuleiro (maplinha linha) coluna) nil)))



; ---------------------- Apenas guardado para ver como se faz debug --------------------
;(defun altura-coluna (tabuleiro coluna)
;		(let ((conta-coluna 0))
;			 (loop  for linha-actual from 0 to *max-linhas-index* 
;			 			until (equal (aref tabuleiro linha-actual coluna) T) do ; until (> linha-actual 10) do 
;			 	    		(progn 
;			 	    			;(princ (equal (aref tabuleiro linha-actual coluna) nil))
;			 	    			(princ "Linha actual")
;			 	    			(princ linha-actual)
;			 	    			;(princ conta-coluna)
;			 	    			;(princ " ") 
;			 	    			(setf conta-coluna (+ conta-coluna 1))
;
;			 	    		))
;			 (maplinha conta-coluna)))
; -------------------------------------- END --------------------------------------



				
; tabuleiro altura-coluna : tabuleiro x coluna --> inteiro que representa a linha mais elevada ocupada
;										 	       da coluna recebida
; tabuleiro --> array bidimensional que representa um tabuleiro
; coluna --> inteiro [0,9] que representa a coluna do tabuleiro
(defun tabuleiro-altura-coluna (tabuleiro coluna)
		(let ((conta-coluna 0))
			 
			 (loop  for linha-actual from 0 to *max-linhas-index* 
			 	   	    until (equal (aref tabuleiro linha-actual coluna) T) do
			 	        	(setf conta-coluna (+ conta-coluna 1)))

			 (let ((linha-resultado (maplinha conta-coluna)))
			 	   (if (< linha-resultado 0)
			 	 		0
			 	 		linha-resultado))))
				


; ----------------- Apenas guardado porque sim ----------------------
;(defun altura-coluna (tabuleiro coluna)
;		(let ((conta-coluna 0))
;			 (progn
;			 	(dotimes *max-linhas-index*) 
;				 	while (equal (tabuleiro-preenchido-p tabuleiro nlinha coluna) nil) do
;			 			(setf conta-coluna nlinha)
;			 conta-coluna)))
; ---------------------------- END ---------------------------------



; Reconhecedor
; tabuleiro-linha-completa-p : tabuleiro x linha --> T se a linha estiver toda preenchida
; tabuleiro --> array bidimensional que representa um tabuleiro
; linha --> inteiro [0,17] que representa a linha do tabuleiro
(defun tabuleiro-linha-completa-p (tabuleiro linha)
		(let ((conta-colunas-preenchidas 0)
			  (linha-a-verificar (maplinha linha)))
			  
			  (loop for coluna-actual from 0 to *max-colunas-index* 
			 	   	    when (equal (aref tabuleiro linha-a-verificar coluna-actual) T) do
			 	   	    	(setf conta-colunas-preenchidas (+ conta-colunas-preenchidas 1)))

			  (= conta-colunas-preenchidas *num-colunas*)))



; Modificador
; tabuleiro-preenche! : tabuleiro x linha x coluna 
; tabuleiro --> array bidimensional que representa um tabuleiro
; linha --> inteiro [0,17] que representa a linha do tabuleiro
; coluna --> inteiro [0,9] que representa a coluna do tabuleiro
; Altera o tabuleiro recebido. Mete a posicao do tabuleiro indicada pela linha
; e coluna recebidas a T, ou seja, mete essa posicao como ocupada 
(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(if (and (numberp linha) (numberp coluna)
			 (>= linha 0) (<= linha *max-linhas-index*)
			 (>= coluna 0) (<= coluna *max-colunas-index*))
		(setf (aref tabuleiro (maplinha linha) coluna) T)))	


; Modificador
; tabuleiros-remove-linha!: tabuleiro x linha
; tabuleiro --> array bidimensional que representa um tabuleiro
; linha --> inteiro [0,17] que representa a linha do tabuleiro
; Altera o tabuleiro recebido. Remove a linha numero passada no 
; argunmento linha. Desce as restantes linha uma posicao e acrecenta
; uma nova linha no topo com todas as posicoes vazias
(defun tabuleiros-remove-linha! (tabuleiro linha)
		(let ((tabuleiros-iguais nil)
			  (linha-mapeada (maplinha linha)))
			 	
			 	(loop  for linha-actual from linha-mapeada downto 0 do
			 		 	(if (= linha-actual 0)
			 		 	 	(loop  for coluna-actual from 0 to *max-colunas-index*  do
			 	        			(setf (aref tabuleiro linha-actual coluna-actual) nil))
			 	 		 	(let ((linha-anterior (1- linha-actual)))
			 	        		    (loop  for coluna-actual from 0 to *max-colunas-index*  do
			 	        				  (setf (aref tabuleiro linha-actual coluna-actual)
			 	        						(aref tabuleiro linha-anterior coluna-actual))))))))

;Reconhecedor
; tabuleiro-topo-preenchido-p : tabuleiro --> T se a linha do topo estiver toda preenchida
; tabuleiro --> array bidimensional que representa um tabuleiro
(defun tabuleiro-topo-preenchido-p (tabuleiro)
	(tabuleiro-linha-completa-p tabuleiro *max-linhas-index*))


;Reconhecedor
; tabuleiros-iguais-p : tabuleiro1 x tabuleiro2 --> T se os tabuleiros tiverem exactamente 
;												    o mesmo conteudo, nil caso contrario
; tabuleiro1 --> array bidimensional que representa um tabuleiro
; tabuleiro2 --> array bidimensional que representa outro tabuleiro
(defun tabuleiros-iguais-p (tab1 tab2)
		(let ((tabuleiros-iguais nil))

			 (loop  for linha-actual from 0 to *max-linhas-index* 
			 	   	    until (equal tabuleiros-iguais T) do
			 	        	(loop  for coluna-actual from 0 to *max-colunas-index* 
			 	   	    			until (equal tabuleiros-iguais T) do
			 	        				(if (not (equal (aref tab1 linha-actual coluna-actual) 
			 	        						        (aref tab2 linha-actual coluna-actual)))
			 	        					(setf tabuleiros-iguais T))))

			 (not tabuleiros-iguais)))

;Transformador de saída
; tabuleiro->array : tabuleiro --> Devolve a representacao de um tabuleiro na forma de um array. 
;								   Por acaso, a nossa representacao interna e tambem um array,
;								   logo a transformacao e directa.
;tabuleiro --> tabuleiro a para ser representado como array
(defun tabuleiro->array (tabuleiro)
	(copia-tabuleiro (tabuleiro)))


;Transformador de entrada
; array->tabuleiro : array --> Constroi um tabuleiro a partir do input de um array
;							   Por acaso, a nossa representacao interna e tambem um array,
;							   logo a transformacao e directa.
;tabuleiro --> array a partir do qual ira ser criado um novo tabuleiro
(defun array->tabuleiro (tabuleiro)
	(copia-tabuleiro (tabuleiro)))



; TAI Estado
(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)

(defun copia-estado (estado-a-copiar)
	(make-estado :pontos (estado-a-copiar-pontos)))



; Tabuleiro de exemplo!!!!!
(defun tab-ex ()
	(let ((a (cria-tabuleiro)))
		(progn
			(tabuleiro-preenche! a 1 0)
			(tabuleiro-preenche! a 1 1)
			(tabuleiro-preenche! a 1 2)
			(tabuleiro-preenche! a 1 3)
			(tabuleiro-preenche! a 1 4)
			(tabuleiro-preenche! a 1 5)
			(tabuleiro-preenche! a 1 6)
			(tabuleiro-preenche! a 1 7)
			(tabuleiro-preenche! a 1 8)
			(tabuleiro-preenche! a 1 9)
			(tabuleiro-preenche! a 17 0)
			(tabuleiro-preenche! a 17 1)
			(tabuleiro-preenche! a 17 2)
			(tabuleiro-preenche! a 17 3)
			(tabuleiro-preenche! a 17 4)
			(tabuleiro-preenche! a 17 5)
			(tabuleiro-preenche! a 17 6)
			(tabuleiro-preenche! a 17 7)
			(tabuleiro-preenche! a 17 8)
			(tabuleiro-preenche! a 17 9)
			(tabuleiro-preenche! a 3 0)
			(tabuleiro-preenche! a 3 1)
			(tabuleiro-preenche! a 4 9)
			a)))

(defun estado-exemplo ()
		(make-estado :pontos 4 :pecas-por-colocar '()))


