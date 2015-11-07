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
(defun tabuleiro-remove-linha! (tabuleiro linha)
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
; Implementado utilizando uma estrutura
; pontos - numero de pontos
; pecas-por-colocar - lista com pecas por colocar. As pecas devem estar ordenadas.
;					  (e.g i,j,l,o,s,z,t)
; pecas-colocadas - lista com pecas ja colocados. Esta lista deve estar ordenada
; tabuleiro - implementacao de um tabuleiro					  
(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)

; Faz uma copia do estado recebido. Este estado ira estar numa posicao de memoria distinta 
; do recebido por input
(defun copia-estado (estado-a-copiar)
	(make-estado :pontos (estado-pontos estado-a-copiar) 
				 :pecas-por-colocar (copy-list (estado-pecas-por-colocar estado-a-copiar))
				 :pecas-colocadas (copy-list (estado-pecas-colocadas estado-a-copiar))
				 :tabuleiro (copia-tabuleiro (estado-tabuleiro estado-a-copiar))))

; Verifica se dois estados sao iguais.
(defun estados-iguais-p (estado1 estado2)
	(and (= (estado-pontos estado1) 
		    (estado-pontos estado2))
		 (equal (estado-pecas-por-colocar estado1) 
		 		(estado-pecas-por-colocar estado2))
		 (equal (estado-pecas-colocadas estado1) 
		 		(estado-pecas-colocadas estado2))
		 (tabuleiros-iguais-p (estado-tabuleiro estado1) 
		 					  (estado-tabuleiro estado2))))

; Verifica se um estado e final
(defun estado-final-p (estado-a-verificar)
	(or (tabuleiro-topo-preenchido-p (estado-tabuleiro estado-a-verificar))
		(equal (estado-pecas-por-colocar estado-a-verificar) '())))

; TAI Problema - Representa um problema generico de procura
;
; Implementado utilizando uma estrutura
; estado - estado inicial do procura de procura
; solucao - funcao que recebe um estado e verifica se e uma solucao para 
;			o problema de procura
; accoes - funcao que recebe um estado e devolve todas as accoes possiveis
;		    desse estado
; resultado - lista com pecas ja colocados. Esta lista deve estar ordenada
; custo-caminho - implementacao de um tabuleiro					  
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)




; Tabuleiro de exemplo!!!!!
(defun tab-ex1 ()
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
			(tabuleiro-preenche! a 8 8)
			a)))

; Tabuleiro de exemplo!!!!!
(defun tab-ex2 ()
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

(defun estado-exemplo1 ()
		(make-estado :pontos 4 :pecas-por-colocar '(i o j l t) :pecas-colocadas '(i o) :tabuleiro (tab-ex1)))

(defun estado-exemplo2 ()
		(make-estado :pontos 19 :pecas-por-colocar '(l) :pecas-colocadas '(i o t) :tabuleiro (tab-ex2)))

(defun estado-exemplo3 ()
		(make-estado :pontos 3 :pecas-por-colocar '(i o j l t) :pecas-colocadas '(i o) :tabuleiro (tab-ex1)))




;;; definicao das configuracoes possiveis para cada peca
;;peca i 
(defconstant peca-i0 (make-array (list 4 1) :initial-element T))
(defconstant peca-i1 (make-array (list 1 4) :initial-element T))
;;peca l
(defconstant peca-l0 (make-array (list 3 2) :initial-contents '((T T)(T nil)(T nil))))
(defconstant peca-l1 (make-array (list 2 3) :initial-contents '((T nil nil)(T T T))))
(defconstant peca-l2 (make-array (list 3 2) :initial-contents '((nil T)(nil T)(T T))))
(defconstant peca-l3 (make-array (list 2 3) :initial-contents '((T T T)(nil nil T))))
;;peca j
(defconstant peca-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T))))
(defconstant peca-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil))))
(defconstant peca-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T))))
(defconstant peca-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T))))
;;peca o
(defconstant peca-o0 (make-array (list 2 2) :initial-element T))
;;peca s
(defconstant peca-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T))))
(defconstant peca-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil))))
;;peca z
(defconstant peca-z0 (make-array (list 2 3) :initial-contents '((nil T T)(T T nil))))
(defconstant peca-z1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(nil T))))
;;peca t
(defconstant peca-t0 (make-array (list 2 3) :initial-contents '((T T T)(nil T nil))))
(defconstant peca-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil))))
(defconstant peca-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T))))
(defconstant peca-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))

;; acrescentei algumas funcoes auxiliares que vao dar jeito para testar automaticamente o codigo dos alunos
(defun ignore-value (x)
	(declare (ignore x))
	'ignore)

;;; random-element: list --> universal
;;; funcao que dada uma lista, devolve um elemento aleatorio dessa lista
;;; se a lista recebida for vazia, e devolvido nil
(defun random-element (list)
  (nth (random (length list)) list))

;;; random-pecas: inteiro --> lista
;;; funcao que recebe um inteiro que especifica o numero de pecas pretendidas, e devolve uma lista com
;;; pecas (representadas atraves de um simbolo) escolhidas aleatoriamente. O tamanho da lista devolvida corresponde
;;; ao inteiro recebido.
(defun random-pecas (n)
	(let ((lista-pecas nil))
		(dotimes (i n)
			(push (random-element (list 'i 'l 'j 'o 's 'z 't)) lista-pecas))
		lista-pecas))
		
;;; cria-tabuleiro-aleatorio: real (opcional) x real (opcional) --> tabuleiro
;;; funcao que recebe um valor real (entre 0 e 1) para a probabilidade a ser usada na primeira linha e outro real
;;; que representa o decrescimento de probabilidade de uma linha para a seguinte. Estes argumentos sao opcionais,
;;; e se nao forem especificados tem o valor por omissao de 1.0 (100%) e 0.05 (5%) respectivamente
;;; A funcao retorna um tabuleiro em que cada posicao foi preenchida de acordo com as probabilidades especificadas
;;; para cada linha. A linha inicial tera uma maior probabilidade, mas as linhas seguintes terao uma menor probabilidade
;;; de preenchimento, resultado em media mais posicoes preenchidas no fundo do tabuleiro do que no topo. 
(defun cria-tabuleiro-aleatorio (&optional (prob-inicial 1.0) (decaimento 0.05))
	(let ((tabuleiro (cria-tabuleiro))
		  (prob prob-inicial)
		  (coluna-a-evitar 0))
		(dotimes (linha 18)
			;;;precisamos de escolher sempre uma coluna para nao preencher, se nao podemos correr o risco de criarmos uma linha
			;;;completamente preenchida
			(setf coluna-a-evitar (random 10)) 
			(dotimes (coluna 10)
				(when (and (not (= coluna-a-evitar coluna)) (<= (random 1.0) prob)) (tabuleiro-preenche! tabuleiro linha coluna)))
			;;;nao podemos permitir valores negativos de probabilidade
			(setf prob (max 0 (- prob decaimento))))
		tabuleiro))
		
;;; executa-jogadas: estado x lista --> inteiro
;;; funcao que recebe um estado e uma lista de accoes e executa as accoes (pela ordem recebida) sobre o tabuleiro do estado inicial,
;;; desenhando no ecra os varios estados do tabuleiro. Para avancar entre ecras, o utilizador deve premir a tecla "Enter".
;;;	retorna o total de pontos obtidos pela sequencia de accoes no tabuleiro
(defun executa-jogadas (estado-inicial lista-accoes)
	(let ((estado estado-inicial))
		(do () ((or (estado-final-p estado) (null lista-accoes)))
			(desenha-estado estado)
			(read-char)
			(desenha-estado estado (first lista-accoes))
			(read-char)
			(setf estado (resultado estado (first lista-accoes)))
			(setf lista-accoes (rest lista-accoes)))
		(desenha-estado estado)
		(estado-pontos estado)))

;;; desenha-estado: estado x accao (opcional) --> {}
;;; funcao que recebe um estado (e pode receber opcionalmente uma accao) e desenha o estado do jogo de tetris no ecra
;;; se for recebida uma accao, entao essa accao contem a proxima jogada a ser feita, e deve ser desenhada na posicao correcta por cima 
;;; do tabuleiro de tetris. Esta funcao nao devolve nada.		
(defun desenha-estado (estado &optional (accao nil))
	(let ((tabuleiro (estado-tabuleiro estado)))
		(desenha-linha-exterior) (format T "  Proxima peca:~A~%" (first (estado-pecas-por-colocar estado))) 
		(do ((linha 3 (- linha 1))) ((< linha 0))
			(desenha-linha-accao accao linha) (format T "~%"))
		(desenha-linha-exterior) (format T "  Pontuacao:~A~%" (estado-pontos estado))
		(do ((linha 16 (- linha 1))) ((< linha 0))
			(desenha-linha tabuleiro linha) (format T "~%"))
		(desenha-linha-exterior)))

;;; desenha-linha-accao: accao x inteiro --> {}
;;; dada uma accao e um inteiro correspondente a uma linha que esta por cima do tabuleiro (linhas 18,19,20,21) desenha
;;; a linha tendo em conta que podera estar la a peca correspondente a proxima accao. Nao devolve nada.
(defun desenha-linha-accao (accao linha)
	(format T "| ")
	(dotimes (coluna 10)
		(format T "~A " (cond ((null accao) " ")
							  ((and (array-in-bounds-p (accao-peca accao) linha (- coluna (accao-coluna accao)))
									(aref (accao-peca accao) linha (- coluna (accao-coluna accao)))) "#")
							  (T " "))))
	(format T "|"))
	
;;; desenha-linha-exterior: {} --> {}
;;; funcao sem argumentos que desenha uma linha exterior do tabuleiro, i.e. a linha mais acima ou a linha mais abaixo
;;; estas linhas sao desenhadas de maneira diferente, pois utilizam um marcador diferente para melhor perceber 
;;; os limites verticais do tabuleiro de jogo
(defun desenha-linha-exterior ()
	(format T "+-")
	(dotimes (coluna 10)
		(format T "--"))
	(format T "+"))
	
;;; desenha-linha-vazia: {} --> {}
;;; funcao sem argumentos que desenha uma linha vazia. Nao devolve nada.
(defun desenha-linha-vazia ()
	(format T "| ")
	(dotimes (coluna 10)
		(format T "~A "))
	(format T "|"))
	
;;; desenha-linha: tabuleiro,inteiro --> {}
;;; esta funcao recebe um tabuleiro, e um inteiro especificando a linha a desenhar
;;; e desenha a linha no ecra, colocando o simbolo "#" por cada posicao preenchida, 
;;; e um espaco em branco por cada posicao nao preenchida. Nao devolve nada.
(defun desenha-linha (tabuleiro linha)
	(format T "| ")
	(dotimes (coluna 10)
		(format T "~A " (if (tabuleiro-preenchido-p tabuleiro linha coluna) "#" " ")))
	(format T "|"))			

			
;;exemplo muito simples de um tabuleiro com a primeira e segunda linha quase todas preenchidas
(defvar t1 (cria-tabuleiro))
(dotimes (coluna 9)
	(tabuleiro-preenche! t1 0 coluna))
(dotimes (coluna 9)
	(tabuleiro-preenche! t1 1 coluna))
(defvar e1 (make-estado :tabuleiro t1 :pecas-por-colocar '(i o j l t i)))
 
;(defvar p1
;	(make-problema :estado-inicial (make-estado :tabuleiro t1 :pecas-por-colocar '(i o j l t i))
;				   :solucao #'solucao
;				   :accoes #'accoes
;				   :resultado #'resultado
;				   :custo-caminho #'custo-oportunidade))

