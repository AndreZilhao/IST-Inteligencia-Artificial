(load "utils.lisp")
;; Numero de colunas do tabuleiro
(defparameter NUM-COLUNAS 10)
;; Numero de linhas do tabuleiro
(defparameter NUM-LINHAS 18)

;; Indices maximos das colunas e das linhas do tabuleiro
(defparameter MAX-COLUNAS-INDEX 9)
(defparameter MAX-LINHAS-INDEX 17)

; Funcao auxiliar que mapeia a numeracao das linhas requerida no enunciado em que a linha 0 encontra-se 
; na posicaoo mais 'abaixo'
(defun maplinha (linha)
	(- MAX-LINHAS-INDEX linha))

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
	(make-array (list NUM-LINHAS NUM-COLUNAS) :initial-element 'F))

; cria-accao : tabuleiro-a-copiar --> tabuleiro - copia do tabuleiro-a-copiar
; tabuleiro-a-copiar --> array bidimensional que representa o tabuleiro a ser copiado
(defun copia-tabuleiro (tabuleiro-a-copiar)
	(let ((novo-tabuleiro (cria-tabuleiro)))

		(loop for linha-actual from 0 to MAX-LINHAS-INDEX do
			(loop for coluna-actual from 0 to MAX-COLUNAS-INDEX do
				(setf (aref novo-tabuleiro linha-actual coluna-actual) (aref tabuleiro-a-copiar linha-actual coluna-actual))))

		novo-tabuleiro))


; Selectores
; tabuleiro-preenchido-p : tabuleiro x linha x coluna --> retorna T ou nil consoante a posicao a verificar esteja ou nao ocupada
; tabuleiro --> array bidimensional que representa um tabuleiro
; linha --> inteiro [0,17] que representa a linha do tabuleiro
; coluna --> inteiro [0,9] que representa a coluna do tabuleiro
(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
	(if (and (numberp linha) (numberp coluna)
		(>= linha 0) (<= linha MAX-LINHAS-INDEX)
		(>= coluna 0) (<= coluna MAX-COLUNAS-INDEX))
	(not (equal (aref tabuleiro (maplinha linha) coluna) 'F))))


; tabuleiro altura-coluna : tabuleiro x coluna --> inteiro que representa a linha mais elevada ocupada
;										 	       da coluna recebida
; tabuleiro --> array bidimensional que representa um tabuleiro
; coluna --> inteiro [0,9] que representa a coluna do tabuleiro
(defun tabuleiro-altura-coluna (tabuleiro coluna)
	(let ((conta-coluna NUM-LINHAS))
		(loop for linha-actual from 0 to MAX-LINHAS-INDEX 
			until (equal (aref tabuleiro linha-actual coluna) T) 
			do
			(decf conta-coluna))
			 				  	;(setf coluna-toda-preenchida nil))
	conta-coluna))	



; Reconhecedor
; tabuleiro-linha-completa-p : tabuleiro x linha --> T se a linha estiver toda preenchida
; tabuleiro --> array bidimensional que representa um tabuleiro
; linha --> inteiro [0,17] que representa a linha do tabuleiro
(defun tabuleiro-linha-completa-p (tabuleiro linha)
	(let (
       	(conta-colunas-preenchidas 0)
		(linha-a-verificar (maplinha linha)))

	(loop for coluna-actual from 0 to MAX-COLUNAS-INDEX 
		when (equal (aref tabuleiro linha-a-verificar coluna-actual) T) do
		(setf conta-colunas-preenchidas (+ conta-colunas-preenchidas 1)))

	(= conta-colunas-preenchidas NUM-COLUNAS)))



; Modificador
; tabuleiro-preenche! : tabuleiro x linha x coluna 
; tabuleiro --> array bidimensional que representa um tabuleiro
; linha --> inteiro [0,17] que representa a linha do tabuleiro
; coluna --> inteiro [0,9] que representa a coluna do tabuleiro
; Altera o tabuleiro recebido. Mete a posicao do tabuleiro indicada pela linha
; e coluna recebidas a T, ou seja, mete essa posicao como ocupada 
(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(if (and (numberp linha) (numberp coluna)
		(>= linha 0) (<= linha MAX-LINHAS-INDEX)
		(>= coluna 0) (<= coluna MAX-COLUNAS-INDEX))
	(setf (aref tabuleiro (maplinha linha) coluna) T)))	


; Modificador
; tabuleiros-remove-linha!: tabuleiro x linha
; tabuleiro --> array bidimensional que representa um tabuleiro
; linha --> inteiro [0,17] que representa a linha do tabuleiro
; Altera o tabuleiro recebido. Remove a linha numero passada no 
; argunmento linha. Desce as restantes linha uma posicao e acrecenta
; uma nova linha no topo com todas as posicoes vazias
(defun tabuleiro-remove-linha! (tabuleiro linha)
	(let ((linha-mapeada (maplinha linha)))

		(loop  for linha-actual from linha-mapeada downto 0 do
			(if (= linha-actual 0)
				(loop  for coluna-actual from 0 to MAX-COLUNAS-INDEX  do
					(setf (aref tabuleiro linha-actual coluna-actual) 'F))
				(let ((linha-anterior (1- linha-actual)))
					(loop  for coluna-actual from 0 to MAX-COLUNAS-INDEX  do
						(setf (aref tabuleiro linha-actual coluna-actual)
							(aref tabuleiro linha-anterior coluna-actual))))))))


;Reconhecedor
; tabuleiro-topo-preenchido-p : tabuleiro x linha --> T se a linha indicada tiver algum valor preenchido
; tabuleiro --> array bidimensional que representa um tabuleiro
; linha --> inteiro [0,17] que representa a linha do tabuleiro a ser verificada
(defun verifica-linha-preenchida-p (tab linha)
	(let ((valor-posicao-actual nil))
		(loop for coluna-actual from 0 to MAX-COLUNAS-INDEX 
			until (equal valor-posicao-actual T) do
			(if (equal (aref tab linha coluna-actual) T) 
				(setf valor-posicao-actual T)))
		valor-posicao-actual))

;Reconhecedor
; tabuleiro-topo-preenchido-p : tabuleiro --> T se a linha do topo tiver algum valor preenchido
; tabuleiro --> array bidimensional que representa um tabuleiro
(defun tabuleiro-topo-preenchido-p (tabuleiro)
	(verifica-linha-preenchida-p tabuleiro (maplinha MAX-LINHAS-INDEX)))




;Reconhecedor
; tabuleiros-iguais-p : tabuleiro1 x tabuleiro2 --> T se os tabuleiros tiverem exactamente 
;												    o mesmo conteudo, nil caso contrario
; tabuleiro1 --> array bidimensional que representa um tabuleiro
; tabuleiro2 --> array bidimensional que representa outro tabuleiro
(defun tabuleiros-iguais-p (tab1 tab2)
	(let ((tabuleiros-iguais nil))

		(loop  for linha-actual from 0 to MAX-LINHAS-INDEX 
			until (equal tabuleiros-iguais T) do
			(loop  for coluna-actual from 0 to MAX-COLUNAS-INDEX 
				until (equal tabuleiros-iguais T) do
				(if (not (equal (aref tab1 linha-actual coluna-actual) 
					(aref tab2 linha-actual coluna-actual)))
				(setf tabuleiros-iguais T))))

		(not tabuleiros-iguais)))

;Transformador de saida
; tabuleiro->array : tabuleiro --> Devolve a representacao de um tabuleiro na forma de um array. 
;								   Por acaso, a nossa representacao interna e tambem um array,
;								   logo a transformacao e directa.
;tabuleiro --> tabuleiro a para ser representado como array
(defun tabuleiro->array (tabuleiro)
	(copia-tabuleiro tabuleiro))


;Transformador de entrada
; array->tabuleiro : array --> Constroi um tabuleiro a partir do input de um array
;							   Por acaso, a nossa representacao interna e tambem um array,
;							   logo a transformacao e directa.
;tabuleiro --> array a partir do qual ira ser criado um novo tabuleiro
(defun array->tabuleiro (tabuleiro)
	(copia-tabuleiro tabuleiro))



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


;
;
;
;verifica se um estado e solucao
(defun solucao (estado-a-verificar)
	(and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado-a-verificar)))
		(equal (estado-pecas-por-colocar estado-a-verificar) '() )))



(defun preenche-lista-accoes (lista-accoes lista-configuracoes) 
	(let ((coluna NUM-COLUNAS)
		(tamanho-lista-configuracoes (length lista-configuracoes)))
	(progn 
		(loop for a from 1 to tamanho-lista-configuracoes do
			(progn
				(setf coluna (- NUM-COLUNAS (array-dimension (car lista-configuracoes) 1)))
				(loop for i from 0 to coluna do
					(setf lista-accoes (append lista-accoes (list (cria-accao i (car lista-configuracoes)))))
					)
				(setf lista-configuracoes (cdr lista-configuracoes))))

		lista-accoes)))

(defun accoes (estado)
	(let ((peca (first (estado-pecas-por-colocar estado)))
		(lista-accoes '()))
	(case peca 
		(i (preenche-lista-accoes lista-accoes (list peca-i0 peca-i1)))
		(l (preenche-lista-accoes lista-accoes (list peca-l0 peca-l1 peca-l2 peca-l3)))
		(j (preenche-lista-accoes lista-accoes (list peca-j0 peca-j1 peca-j2 peca-j3)))
		(o (preenche-lista-accoes lista-accoes (list peca-o0)))
		(s (preenche-lista-accoes lista-accoes (list peca-s0 peca-s1)))
		(z (preenche-lista-accoes lista-accoes (list peca-z0 peca-z1)))
		(t (preenche-lista-accoes lista-accoes (list peca-t0 peca-t1 peca-t2 peca-t3)))))
	)

(defun qualidade (estado)
	(- 0 (estado-pontos estado)))

(defun custo-oportunidade (estado)
	(let* (
		(pontos (estado-pontos estado))
		(pontos-totais 0)
		(pecas-colocadas (estado-pecas-colocadas estado))
		(tamanho-lista-pecas (length pecas-colocadas)))
	(loop for a from 1 to tamanho-lista-pecas do
		(progn
			(case (car pecas-colocadas)
				(i (setf pontos-totais (+ pontos-totais 800)))
				(l (setf pontos-totais (+ pontos-totais 500)))
				(o (setf pontos-totais (+ pontos-totais 300)))
				(j (setf pontos-totais (+ pontos-totais 500)))
				(s (setf pontos-totais (+ pontos-totais 300)))
				(z (setf pontos-totais (+ pontos-totais 300)))
				(t (setf pontos-totais (+ pontos-totais 300)))
				)
			(setf pecas-colocadas (cdr pecas-colocadas)))
		)
	(- pontos-totais pontos))
	)

(defun verifica-posicao (tabuleiro linha coluna peca)
	(let* (
		(peca-altura (array-dimension peca 0))
		(peca-largura (array-dimension peca 1))
		(posicao-invalida nil)
		(index-linha 0))
	(loop for b from 1 to peca-altura do
		(let ((index-coluna 0))
			(loop for a from 1 to peca-largura do
				(progn
					(if  
						(if (equal (aref peca index-linha index-coluna) T)
							(tabuleiro-preenchido-p tabuleiro (+ linha index-linha) (+ coluna index-coluna)))
						(setf posicao-invalida T))
					)
				(setf index-coluna (+ index-coluna 1)))
			(setf index-linha (+ index-linha 1))))
	posicao-invalida)
	)

(defun preenche-peca (tabuleiro linha coluna peca)
  	(let* (
		(peca-altura (array-dimension peca 0))
		(peca-largura (array-dimension peca 1))
		(index-linha 0))
	(loop for b from 1 to peca-altura do
		(let ((index-coluna 0))
			(loop for a from 1 to peca-largura do
				(progn
						(if (equal (aref peca index-linha index-coluna) T)
							(tabuleiro-preenche! tabuleiro (+ linha index-linha) (+ coluna index-coluna)))
      						)
				(setf index-coluna (+ index-coluna 1)))
			(setf index-linha (+ index-linha 1)))))
  )


;
(defun resultado (estado accao)
	(let* (
		(novo-estado (copia-estado estado))
		(coluna (accao-coluna accao))
		(peca (accao-peca accao))
		(linha MAX-LINHAS-INDEX)
		(linha-final (+ MAX-LINHAS-INDEX 1))
		(posicao T)
  		(conta-linhas-removidas 0))
	(progn
;VERIFICACAO DE QUAL A POSICAO DA PECA NOVA
		(loop for a from linha downto 0 while (equal posicao T)  do
			(if (verifica-posicao (estado-tabuleiro estado) a coluna peca)
				(progn
					(setf posicao nil)
					(setf linha-final a)
					)
				))
  		(if (= linha-final 18)
        	(setf linha-final -1))
;COLOCACAO DA PECA NOVA
		(preenche-peca (estado-tabuleiro novo-estado) (+ 1 linha-final) coluna  peca)
;VERIFICACAO DE LINHAS PREENCHIDAS E ACTUALIZACAO DE PONTOS
		(if (equal (tabuleiro-topo-preenchido-p (estado-tabuleiro novo-estado)) T)
		 		novo-estado
		 		(loop for linha from 0 to MAX-LINHAS-INDEX do
		 			(progn 
		 			(if (equal (tabuleiro-linha-completa-p (estado-tabuleiro novo-estado) linha) T)
		 				(progn (incf conta-linhas-removidas)
		 					   (tabuleiro-remove-linha! (estado-tabuleiro novo-estado) linha)
		 					   (decf linha))))))
  		(cond ((>= conta-linhas-removidas 4) (setf (estado-pontos novo-estado) (+ (estado-pontos novo-estado) 800)))
		 	   ((= conta-linhas-removidas 3) (setf (estado-pontos novo-estado) (+ (estado-pontos novo-estado) 500)))
		 	   ((= conta-linhas-removidas 2) (setf (estado-pontos novo-estado) (+ (estado-pontos novo-estado) 300)))
		 	   ((= conta-linhas-removidas 1) (setf (estado-pontos novo-estado) (+ (estado-pontos novo-estado) 100)))
		 	  )
;ACTUALIZACAO DA LISTA DE PECAS COLOCADAS E PECAS POR COLOCAR 
		(setf (estado-pecas-colocadas novo-estado) 
        	(cons (car (estado-pecas-por-colocar novo-estado)) (estado-pecas-colocadas novo-estado)))
		(setf (estado-pecas-por-colocar novo-estado)
        	(cdr (estado-pecas-por-colocar novo-estado)))
		)
	novo-estado)
	)

; Tabuleiro
;--
(defun estado-exemplo1 ()
	(make-estado :pontos 4 :pecas-por-colocar '(i o j l t) :pecas-colocadas '(i o) :tabuleiro (tab-ex1)))

(defun estado-exemplo2 ()
	(make-estado :pontos 19 :pecas-por-colocar '(l) :pecas-colocadas '(i o t) :tabuleiro (tab-ex2)))

(defun estado-exemplo3 ()
	(make-estado :pontos 3 :pecas-por-colocar '(i o j l t) :pecas-colocadas '(i o) :tabuleiro (tab-ex1)))


