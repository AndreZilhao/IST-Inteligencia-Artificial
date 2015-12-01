# IST-Inteligencia-Artificial

## IA Tetris

#### Introdução  
Neste projecto implementou-se um algoritmo de procura capaz de jogar uma variante do jogo
Tetris, em que as peças por colocar são conhecidas à partida. O objectivo é tentar maximizar o número
de pontos com as peças definidas.

![alt text](http://i.imgur.com/i8CK2uM.png "Logo Title Text 1")

O tabuleiro do jogo é constituído por 10 colunas e 18 linhas. As linhas estão numeradas de 0 a 17, e as
colunas de 0 a 9. Cada posição do tabuleiro pode estar vazia ou ocupada. Assim que uma peça é
colocada sobre o tabuleiro, se existir alguma posição ocupada na linha 17 o jogo termina. Caso
Peças por colocar
contrário, todas as linhas que estejam completas1
são removidas e o jogador ganha o seguinte número
de pontos em função do número de linhas removidas:

| Linhas | Pontos  |
| ------ |:-------:| 
| 1      | 100     |
| 2      | 300     | 
| 3      | 500     |
| 4      | 800     |

Quando uma linha é removida, as linhas superiores a essa linha deverão descer.
O objectivo do projecto é escrever um programa em Common Lisp, que utilize técnicas de procura
sistemática, para determinar a sequência de acções de modo a conseguir colocar todas as peças e
tentando maximizar o número de pontos obtidos.

## Tipos Abstractos de Informação
####Tipo Acção

O tipo Acção é utilizado para representar uma acção do jogador. Uma acção é implementada como um
par cujo elemento esquerdo é um número de coluna que indica a coluna mais à esquerda escolhida para
a peça cair, e cujo elemento direito corresponde a um array bidimensional com a configuração geométrica da peça depois de rodada. O ficheiro **utils.lisp** define todas as configurações geométricas possíveis para cada peça.

**cria-accao: inteiro x array ---> accao**  
Este construtor recebe um inteiro correspondente à posição da coluna mais à esquerda a partir
da qual a peça vai ser colocada, e um array com a configuração da peça a colocar, e devolve uma
nova acção.

**accao-coluna: accao ---> inteiro**  
Este selector devolve um inteiro correspondente à coluna mais à esquerda a partir da qual a
peça vai ser colocada.

**accao-peca: accao ---> array**  
Este selector devolve o array com a configuração geométrica exacta com que vai ser colocada.

####Tipo Tabuleiro
O tipo Tabuleiro é utilizado para representar o tabuleiro do jogo de Tetris com 18 linhas e 10 colunas,
em que cada posição do tabuleiro pode estar preenchida ou não. 

**cria-tabuleiro: {} --->  tabuleiro**  
Este construtor não recebe qualquer argumento, e devolve um novo tabuleiro vazio.

**copia-tabuleiro: tabuleiro ---> tabuleiro**  
Este construtor recebe um tabuleiro, e devolve um novo tabuleiro com o mesmo conteúdo do
tabuleiro recebido. O tabuleiro devolvido deve ser um objecto computacional diferente e deverá
garantir que qualquer alteração feita ao tabuleiro original não deve ser repercutida no novo
tabuleiro e vice-versa.

**tabuleiro-preenchido-p: tabuleiro x inteiro x inteiro ---> lógico**  
Este selector recebe um tabuleiro, um inteiro correspondente ao número da linha e um inteiro
correspondente ao número da coluna, e devolve o valor lógico verdade se essa posição estiver
preenchida, e falso caso contrário.

**tabuleiro-altura-coluna: tabuleiro x inteiro ---> inteiro**
Este selector recebe um tabuleiro, um inteiro correspondente ao número de uma coluna, e
devolve a altura de uma coluna, ou seja a posição mais alta que esteja preenchida dessa coluna.
Uma coluna que não esteja preenchida deverá ter altura 0.

**tabuleiro-linha-completa-p: tabuleiro x inteiro ---> lógico**  
Este reconhecedor recebe um tabuleiro, um inteiro correspondente ao número de uma linha, e
devolve o valor lógico verdade se todas as posições da linha recebida estiverem preenchidas, e
falso caso contrário.

**tabuleiro-preenche!: tabuleiro x inteiro x inteiro ---> {}**  
Este modificador recebe um tabuleiro, um inteiro correspondente ao número linha e um inteiro
correspondente ao número da coluna, e altera o tabuleiro recebido para na posição
correspondente à linha e coluna passar a estar preenchido. 

**tabuleiro-remove-linha!: tabuleiro x inteiro ---> {}**  
Este modificador recebe um tabuleiro, um inteiro correspondente ao número de linha, e altera o
tabuleiro recebido removendo essa linha do tabuleiro, e fazendo com que as linhas por cima da
linha removida desçam uma linha. As linhas que estão por baixo da linha removida não podem
ser alteradas. O valor devolvido por desta função não está definido.

**tabuleiro-topo-preenchido-p: tabuleiro ---> lógico**  
Este reconhecedor recebe um tabuleiro, e devolve o valor lógico verdade se existir alguma
posição na linha do topo do tabuleiro (linha 17) que esteja preenchida, e falso caso contrário.

**tabuleiros-iguais-p: tabuleiro x tabuleiro ---> lógico**  
Este teste recebe dois tabuleiros, e devolve o valor lógico verdade se os dois tabuleiros forem
iguais (i.e. tiverem o mesmo conteúdo), e falso caso contrário. 

**tabuleiro->array: tabuleiro --> array**
Este transformador de saída recebe um tabuleiro e devolve um novo array com 18 linhas e 10
colunas, que para cada linha e coluna deverá conter o valor lógico correspondente a cada
posição do tabuleiro. 

**array->tabuleiro: array --> tabuleiro**  
Este transformador de entrada recebe um array com 18 linhas e 10 colunas cujas posições têm o
valor lógico T ou Nil, e constrói um novo tabuleiro com o conteúdo do array recebido.

#### Tipo Estado 

O tipo estado representa o estado de um jogo de Tetris. Está implementado como uma estructura em Common Lisp com os seguintes argumentos:

- pontos – o número de pontos conseguidos até ao momento no jogo  
- pecas-por-colocar – uma lista com as peças que ainda estão por colocar, pela ordem de
colocação. As peças nesta lista são representadas pelo símbolo correspondente à letra da peça,
i.e. i,j,l,o,s,z,t  
- pecas-colocadas – uma lista com as peças já colocadas no tabuleiro (representadas também
pelo símbolo). Esta lista deve encontrar-se ordenada da peça mais recente para a mais antiga  
- Tabuleiro – o tabuleiro com as posições actualmente preenchidas do jogo.  

**copia-estado: estado ---> estado**  
Este construtor recebe um estado e devolve um novo estado cujo conteúdo deve ser copiado a
partir do estado original. 

**estados-iguais-p: estado x estado ---> lógico**  
Este teste recebe dois estados, e devolve o valor lógico verdade se os dois estados forem iguais
(i.e. tiverem o mesmo conteúdo) e falso caso contrário.

**estado-final-p: estado ---> lógico**  
Este reconhecedor recebe um estado e devolve o valor lógico verdade se corresponder a um
estado final onde o jogador já não possa fazer mais jogadas e falso caso contrário. Um estado é
considerado final se o tabuleiro tiver atingido o topo ou se já não existem peças por colocar.

#### Tipo problema
O tipo problema representa um problema genérico de procura. Está implementado como uma estructura em CLisp, com os seguintes campos:

- estado-inicial – contem o estado inicial do problema de procura 
- solucao – função que recebe um estado e devolve T se o estado for uma solução para o
problema de procura, e nil caso contrário 
- accoes – função que recebe um estado e devolve uma lista com todas as acções que são
possíveis fazer nesse estado  
- resultado – função que dado um estado e uma acção devolve o estado sucessor que
resulta de executar a acção recebida no estado recebido
- custo-caminho – função que dado um estado devolve o custo do caminho desde o
estado inicial até esse estado

### Funções Implementadas
Foram implementadas um conjunto de funções para o auxilio dos algoritmos de procura:

**solucao: estado ---> lógico**  
Esta função recebe um estado, e devolve o valor lógico verdade se o estado recebido
corresponder a uma solução, e falso contrário. Um estado do jogo Tetris é considerado solução
se o topo do tabuleiro não estiver preenchido e se já não existem peças por colocar.

**accoes: estado ---> lista de acções**  
Esta função recebe um estado e devolve uma lista de acções correspondendo a todas as acções
válidas que podem ser feitas com a próxima peça a ser colocada.

**resultado: estado x accao --> estado**  
Esta função recebe um estado e uma acção, e devolve um novo estado que resulta de aplicar a
acção recebida no estado original. 

##### Heuristicas

**qualidade: estado ---> inteiro**  
A função qualidade recebe um estado e retorna um valor de qualidade que
corresponde ao valor negativo dos pontos ganhos até ao momento.

**custo-oportunidade: estado ---> inteiro**
Esta função, dado um estado, devolve o custo de
oportunidade de todas as acções realizadas até ao momento, assumindo que é sempre possível
fazer o máximo de pontos por cada peça colocada.

**custo-oportunidade-cheap: estado ---> inteiro**
Esta função, dado um estado, devolve o custo de
oportunidade de todas as acções realizadas até ao momento, com enfase nas peça que fazem mais pontos como sendo prioritárias. Não é óptima, mas devolve uma solução de forma eficiente.

### Algoritmos de Procura

**procura-pp: problema ---> lista acções**
Esta função recebe um problema e usa a procura em profundidade primeiro em árvore para
obter uma solução para resolver o problema. Devolve uma lista de acções que se executada pela
ordem especificada irá levar do estado inicial a um estado objectivo. 

**procura-A*: problema x heurística ---> lista acções**  
Esta função recebe um problema e uma função heurística, e utiliza o algoritmo de procura A*
em árvore para tentar determinar qual a sequência de acções de modo a maximizar os pontos
obtidos. 

**procura-best: array x lista peças ---> lista acções**
Esta função recebe um array correspondente a um tabuleiro e uma lista de peças por colocar,
inicializa o estado e a estrutura problema com as de heurística escolhidas (custo-oportunidade-cheap), e aplica o algoritmo de procura A* em árvore.

---
## Relatório

O relatório do projecto pode ser encontrado em:  
[to do]

