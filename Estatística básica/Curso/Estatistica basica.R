########################################################################
###########           CURSO:INTRODU��O AO SOFTWARE R         ###########
###########             Eduardo Alves da Silva               ###########
###########                easufsj@gmail.com                 ###########
###########                   02/04/2021                     ###########
########################################################################


#################################################################
##                                                             ##
##------------------- PRIMEIROS PASSOS ------------------------##
##                                                             ##
#################################################################


#1) COMANDOS IMPORTANTES


?TukeyHSD #Buscando o termo -> ou help("TukeyHSD")

citation() #Como citar o software R

#No software R, decimal � representado por ponto " . " Se desejamos escrever
#quatro virgula cinco, a representa��o correta � 4.5
#virgula ser� utilizada para separar valores, linhas, colunas, e fun��es


#2) OPERA��ES MATEMATICAS

2+2
3-1
2*3 
4/2
2^2
sqrt(100) #Raiz quadrada
sin(45) #Seno
cos(45) #Coseno
tan(45) #Tangente


4<7	  #x menor que y?
2<=3 	#x menor ou igual a y?
5>2	  #x maior que y?
4>=3	#x maior ou igual a y?
2==3	#x igual a y?
6!=7	#x diferente de y?

#################################################################
##                                                             ##
##----------------------- CRIANDO OBJETOS ---------------------##
##                                                             ##
#################################################################

a<-10  # O comando <- indica que o objeto "a" receber� o valor 7; 
#Esse objeto fica armazenado no ambiente de trabalho 

a     #Chamando o objeto "a" o R apresentar� o valor atribuido ao objeto

b<-20

a+b   #Quando o comando "recebe" (<-) n�o � utilizado, o mesmo 
#n�o ser� armazenado

c<-a+b  #Caso queira armazenar o resultado, � necessario criar um objeto 

c

#Aten��o com sobrepossi��o. Sempre que um objetivo j� existente
#recebe um novo valor, o anterior ser� substituido

a #O objeto "a" � igual a 7

a<-15 #Caso o mesmo objeto receba um novo valor, o mesmo substituir� o antigo

a

a<-10
a2<-15 #Caso n�o deseje apagar o objeto anterior. Deve ser criado um novo objeto, no caso "a2"
a
a2



ls() #Listar os objetos armazenados 


rm(a2) #Fun��o para remover objetos.
       #Observe que somente a2 ser� removido do workspace



rm(list=ls()) #Deleta tudo que est� armazenado no workspace

#################################################################
##                                                             ##
##-------------- ENTRANDO COM DADOS NO R ----------------------##
##                                                             ##
#################################################################
#1) Entrando com dados diretamente no R
a<- c(10, 20, 30, 40, 50, 60) #criando Vetor A
a
b <- c(2, 4, 6, 10, 8, 12) #Criando Vetor B
b

a+b
a*b
a/b

sum(a) #Somat�rio de "a"
sum(a+b)

length(a+b) # n�mero de elementos/observa��es presentes em a+b
media<-sum(a+b)/length(a+b) 
media 
mean(a+b) 

names(b)<-c("A","B","C","D", "E", "F");b #Nomeando o vetor b
a
b
mean(b)

(order(b, na.last = TRUE, decreasing = TRUE)) #Ordenando b
#Observem que mostra a posi��o

#criando novo vetor
novob<-b[(order(b, na.last = TRUE, decreasing = TRUE))]; novob


b2<-b[1:3];b2  #retirar os n�meros do vetor b nas posi��es 1, 2 e 3
b3<-b[-5]; b3  #retirar o quinta observa��o
b[6]<-50; b #Inserir o valor 100 na posi��o 6

a
names(a)<-c("A","B","C","D", "E", "F");a
mean(a)

#Repeti��es
rep<-factor(rep(1:6, each  = 1));rep

d1=data.frame(cbind(a,rep,b))
d1

a2<-a[a>mean(a)]; a2 #retirar os maiores que a m�dia
mean(a2)

ab<-data.frame(a,b); ab #Criando data.frame composto pelos vetores a e b

names(ab)<-c("Var1", "Var2");ab #Renomeando as a e b para ambientes

ab$Var1 #Observar somente o Vari�vel 1

mean(ab$Var1) #M�dia do Vari�vel 1

var(ab$Var1) #Vari�ncia do Vari�vel 1

length(ab[1,])  #[x,y] Neste caso x representa as linhas, e y as colunas
#Estamos interessados em verificar quantos valores temos
#Para a linha 1, considerando todas as colunas.
#Observa-se que temos 2 valores associados a essa linha

mean(ab$Var1)


ab$Var3<- c(30, 60, 90, 100, 200, 500); ab #Adicionando o Ambiente 3
mean(ab$Var3) #m�dia ambiente 3

#� possivel conectar comandos utilizando &
ab[ab$Var1 > mean(ab$Var1)
   & ab$Var2 > mean(ab$Var2)
   & ab$Var3 > mean(ab$Var3), ] #Tratamentos acima da m�dia em todas as variaveis

ab2<-ab[order(ab$Var2, decreasing = TRUE), ]; ab2 #Ordenando de acordo
#com o vari�vel 2


#2) Importando planilha de dados txt

setwd("D:\\An�lises Programa R\\Curso R nefit")
setwd("D:/An�lises Programa R/Curso R nefit")
#Ctrl+Shift+H -> DEFINIR DIRETORIO #Abrir a pasta diretamente

#mortra o diret�rio atual
getwd()
#O que h� dentro do diret�rio
dir()

ex1<-read.table("exemplo1.txt", h=T) #h=T informa que cabe�alho

ex1
names(ex1)=c("Var1", "Var2", "Var3")
head(ex1) #Primeiras linhas do txt
tail(ex1) #Ultimas linhas do txt
str(ex1) #Mostra a estrutura da tabela

rownames(ex1)<-c(LETTERS[1:20])#Nomeando as linhas. 
ex1

ex1$Amb1 #Observar dados somente do ambiente 1

mean(ex1$Amb2) #M�dia do ambiente 2

var(ex1$Amb3) #Vari�ncia do ambiente 3


ex1[ex1$Amb1 > mean(ex1$Amb1)
    & ex1$Amb2 > mean(ex1$Amb2)
    & ex1$Amb3 > mean(ex1$Amb3), ]  #Gen�tipos acima da m�dia em 
#todos os Ambientes

ex12<-ex1[order(ex1$Amb2, decreasing = TRUE), ]; ex12

#################################################################
#Pacotes
library("agricolae")
library(fBasics)
library("car")
library(asbio)
library("MASS")
require(ScottKnott)
library(ExpDes.pt)
library(FactoMineR)
library(stats)
require(factoextra)

#################################################################
##                                                             ##
##---------------- PLANEJAMENTO EXPERIMENTAL ------------------##
##                                                             ##
#################################################################

#1) Repeti��o
#2) Casualiza��o
#3) Controle local

library("agricolae") #Carregar o pacote
DIC=design.crd(1:10,2, serie = 3, seed = 0, kinds = "Super-Duper", randomization=TRUE)
DBC=design.rcbd(1:10, 3, serie = 3, seed = 0, kinds = "Super-Duper", first=TRUE,
                continue=FALSE,randomization=TRUE ); DBC$book
?design.dau # Existem diversos delineamentos experimentais que podem/devem 
# ser utilizados, delineamentos no ingl�s � conhecido como design
# X deve ser substituido por:
# crd para Inteiramente casualizado
# rcdb para Blocos Completos casualizado
# ab para Esquema fatorial
# alpha para Alpha Latice
# bib para Blocos Incompletos
# split para Parcela Subdividida
# dau para Blocos Aumentados
# lsd para Quadrado Latino
# lattice para Latice
# graeco para Quadrado Greco-Latino
# strip para experimentos em faixas


#DIC, X Tratamentos, X Repeti��es
TRT=read.table("SorteioCult.txt", h=T)
dir()
trt<-(1:10) #N�mero de tratamentos
r<-4 #N�mero de repeti��es

sorteioDIC<-design.crd(trt, r, serie = 3, seed = 0, kinds = "Super-Duper",randomization=TRUE)


write.table(sorteioDIC$book, file='SorteioCult.csv', 
            row.names=FALSE, col.names=c("Parcela", "Rep", "Trat"), sep =" ")

file.show("SorteioCult.csv")
#DBCC, X Tratamentos, X Repeti��es

trt<-(1:10)
r<-3

sorteioDBCC<-design.rcbd(trt, r, serie = 3, seed = 0, kinds = "Super-Duper",randomization=TRUE)
sorteioDBCC$book

write.table(sorteioDBCC$book, file='SorteioDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Bloco", "Trat"), sep =" ")



#FATORIAL A por B, 

trt<-c(3,3, 3) #Mais fatores -> separar por virgulas
#Primeira Lacuna s�o os niveis do FATOR A, segunda do FATORB
#Terceira se for o caso do FATOR C
r<-3        #n�mero de repeti��es


sorteioFATDIC<-design.ab(trt, r, serie = 3, design=c("crd"),seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
sorteioFATDIC

sorteioFATDBCC<-design.ab(trt, r, serie = 3, design=c("rcbd"),seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
sorteioFATDBCC

write.table(sorteioFATDIC$book, file='SorteioFATDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Rep", "Fator A", "Fator B"), sep =" ")


write.table(sorteioFATDBCC$book, file='SorteioFATDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Bloco", "Fator A", "Fator B"), sep =" ")



?design.rcbd


#################################################################
##                                                             ##
##------------------- PRESSUPOSTOS DA ANOVA -------------------##
##                                                             ##
#################################################################


# 1) Normalidade dos erros (Os erros devem seguir distribui��o Normal)
# 2) Homocedasticidade dos erros (Os erros devem ser homogeneos)
# 3) Independ�ncia dos erros (Os erros devem ser independentes)
# 4) Aditividade do modelo (O modelo deve conter apenas efeitos aditivos)


# MODELOS

# DIC: 
# DBC: 

# PRESSUPOSTOS DIC
dados<-read.table("exemplo3.txt", h=T)
str(dados)
dados<-transform(dados, Trat=factor(Trat))
str(dados) 
summary(dados)

# Para realizar as an�lises dos pressupostos � necess�rio extrair 
# primeiramente os ERROS.

AOVDados<-aov(ABS ~ Trat, data = dados)
AOVDados$residuals #Extraindo os residuos/erros do conjunto de dados


# NORMALIDADE
shapiro.test(AOVDados$residuals)  
?shapiro.test

library(fBasics)

qqnormPlot(AOVDados$residuals) #Plotar o gr�fico com os residuos. 

histPlot(x = as.timeSeries(AOVDados$residuals)) 
#observar a distribui��o dos dados


# HOMOCEDASTICIDADE

bartlett.test(AOVDados$residuals~Trat,data=dados) 
?bartlett.test
# INDEPEND�NCIA

library("car")

dwt(lm(AOVDados)) # Neste caso ser� utilizado o teste de Durbin-Watson
?dwt

# ADITIVIDADE

library(asbio)

tukey.add.test(dados$ABS,  dados$Trat, dados$Rep) 
?tukey.add.test
#################################################################
##                                                             ##
##------------------- TRANSFORM��O DE DADOS -------------------##
##                                                             ##
#################################################################

# Caso os pressupostos da ANOVA n�o sejam atingindos, os dados do experimento
# n�o podem ser submetidos a An�lise de Vari�ncia.

# Visando atender aos pressupostos algumas estrategias podem ser utilizadas

# Como a transforma��o de dados. 

dados<-read.table("exemplo2.txt", h=T)
str(dados) 
AOVDados<-aov(VarResp ~ Trat + Rep, data = dados)
AOVDados$residuals
shapiro.test(AOVDados$residuals) 
bartlett.test(residuals(AOVDados)~dados$Trat)
car::dwt(lm(AOVDados)) 
asbio::tukey.add.test(dados$VarResp,  dados$Rep, dados$Trat)

##------------------- Testando Transforma��es Comuns -------------------##

# Raiz Quadrada, Raiz Cubica, Log, Pot�ncia
dados$RQUAD<-dados$VarResp^(1/2) 
dados$RCUB<-dados$VarResp^(1/3)
dados$LOG<-log(dados$VarResp)
dados$POT2<-dados$VarResp^2  

AOVRQUAD<- aov(RQUAD~Trat+Rep, data=dados) # AOV dos dados transformados para
AOVRCUB<- aov(RCUB~Trat+Rep, data=dados)
AOVLOG<- aov(LOG~Trat+Rep, data=dados)
AOVPOT2<- aov(POT2~Trat+Rep, data=dados)

# VERIFICANDO OS PRESSUPOSTOS

shapiro.test(residuals(AOVRQUAD))
bartlett.test(residuals(AOVRQUAD)~dados$Trat)
car::dwt(lm(AOVRQUAD))
asbio::tukey.add.test(dados$RQUAD,  dados$Rep, dados$Trat)

#################################################################
##                                                             ##
##---------------- VERIFICANDO VIA BOXCOX ---------------------##
##                                                             ##
#################################################################
library("MASS")

bc<-boxcox(AOVDados)
bc

locator(n=1) #Clicando

lambda <- boxcox(AOVDados)$x[which(boxcox(AOVDados)$y==max(boxcox(AOVDados)$y))]
# ou lambda <- bc$x[which(bc$y==max(bc$y))]
lambda

# usando a tranforma��o indicada -> 0.1818182 ~ 0.18
dados$BC0.18<-dados$VarResp^0.18

AOVBC0.18<- aov(BC0.18~Trat+Rep, data=dados)

shapiro.test(residuals(AOVBC0.18))
bartlett.test(residuals(AOVBC0.18)~dados$Trat)
car::dwt(lm(AOVBC0.18)) 
asbio::tukey.add.test(dados$BC0.18,  dados$Rep, dados$Trat)


##---------------------AN�LISE DE VARI�NCIA ANAVA ou ANOVA--------------------##
#Primeira coisa � definir o diret�rio # TODA VEZ QUE ABRE O "R" DEVE ESPECIFICAR O DIRET�RIO
setwd("D:/An�lises Programa R/Curso R nefit")

dir()
banco1 <- read_csv("disco/pasta/subpasta/arquivo.csv")
setwd("C:/Users/Pc/Desktop/Doutorado-UFLA/2019/Curso B�sico R/Introduction-to-R-UFLA-2019-master/Exemplos")
dados_DIC<-read.table("dic.txt", h=T)
str(dadosDIC)  #Estrutura da tabela
#Quando for dic apenas o tratamento fica como fator
#Quando for outros devemos coloca-los como fator
#cada efeito do modelo � considerado um fator dic um fator
#dBC � dois fatores

dados_DIC<-transform(dados_DIC, Trat=factor(Trat))
dados_DIC$Trat<-as.factor(dados_DIC$Trat)
str(dados_DIC)
dados_DIC
# Saber o modelo dos delineamentos � importante, pois indicaremos para o R como � o
# modelo a ser utilizado.

#aov(formula, data = NULL)


## MODELO DIC:   Yij = m + Ti + eij

Anova_DIC<-aov(Prod ~ Trat, data = dados_DIC) 
summary(Anova_DIC)
anova(Anova_DIC)    # Quadro de An�lise de Vari�ncia

library(agricolae)
cv.model(AnovaDIC) # CV%

## MODELO DBC:   Yij = m + Ti + bj + eij
dados_DBC<-read.table("dbc.txt", h=T)
str(dados_DBC)
dados_DBC<-transform(dados_DBC, Gen=factor(Gen), Bloco=factor(Bloco))

str(dados_DBC) #Check!

Anova_DBC<-aov(Prod ~ Gen + Bloco, data = dados_DBC) 

anova(Anova_DBC)    #Quadro de An�lise de Vari�ncia
cv.model(Anova_DBC) #CV%

##------------------------ TESTE DE M�DIAS ---------------------------##

#DIC
#Tukey
library(agricolae)
#HSD.test(y, trt, DFerror, MSerror, alpha=0.05)

y<-dados_DIC$Prod #y vai receber a coluna produ��o, ou seja, a resposta
trt<-dados_DIC$Trat #trt dessa forma recebe a coluna de tratamento
DFerror<-df.residual(Anova_DIC)#GL (degrees freedom)
MSerror<-((deviance(Anova_DIC)/DFerror)) #Mean square

TukeyDIC<-HSD.test(y, trt, DFerror, MSerror, alpha=0.05);TukeyDIC
#TukeyDIC<-HSD.test(y=dados_DIC$Prod, trt=dados_DIC$Trat, DFerror=Anova_DIC$df.residual, 3825, alpha=0.05)
TukeyDIC$groups

#ScottKnott
#n�o � um teste de m�dias, mas sim um teste de agrupamento de m�dias, avalia a similariedade de dados
library(ScottKnott)
library(ExpDes.pt)
skDIC <- SK(x=dados_DIC, y=dados_DIC$Prod, model="y~Trat", which="Trat", sig.level=0.05, id.trim=10)
summary(skDIC)

#ScottKnott
anova(Anova_DIC)
scottknott(y,trt, DFerror=18, SSerror=50815, alpha = 0.05, group = TRUE, main = NULL)



#DBC
#Tukey
anova(AOV-DBC)

y2<-dados_DBC$Prod
trt2<-dados_DBC$Gen
DFerror2<-df.residual(Anova_DBC)
MSerror2<-((deviance(Anova_DBC)/DFerror2))

TukeyDBC<-HSD.test(y=y2, trt=trt2, DFerror=DFerror2, MSerror=MSerror2, alpha=0.05)
TukeyDBC$groups


#ScottKnott
library(ScottKnott)
skDBC <- SK(x=dados_DBC, y=dados_DBC$Prod, model="y~Gen+Bloco", which="Gen", sig.level=0.05, id.trim=10)
summary(skDBC)   
### Pay attention no "model" olhar sempre as fontes de varia��o no caso bloco e tratamento
# "which" � usado para identificar qual ser� avaliado: tratamento ou bloco.
anova(Anova_DBC)
scottknott(y2,trt2, DFerror=18, SSerror=101342, alpha = 0.05, group = TRUE, main = NULL)


##------------------------ PACOTE ExpDes.pt ---------------------------##
#Este pacote � em portugu�s

library(ExpDes.pt)

# DIC
# Exemplo - Trat Qualitativo

#   dic(trat=dados$LIN, resp=dados$DIAM, quali = TRUE, mcomp = "tukey", nl = FALSE,
#    hvar='bartlett', sigT = 0.05, sigF = 0.05)  

dic(trat=dados_DIC$Trat, resp=dados_DIC$Prod, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)

#resp=dadosDIC$Prod, o cifr�o diz que RESP � igual a produ��o (PROD) dentro dos dadosDIC 
# Dificil de ler -> Vamos criar um arquivo TXT

sink("Saida ANOVA DIC.txt", type = c ("output")) 

dic(trat=dados_DIC$Trat, resp=dados_DIC$Prod, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)

sink()



# DBC
# Exemplo 1 - Trat Qualitativo

sink("Saida ANOVA DBC - QUALITATIVO.txt", type = c ("output"))

dbc(trat=dados_DBC$Gen, bloco=dados_DBC$Bloco, resp=dados_DBC$Prod, quali = TRUE, mcomp = "tukey", nl=FALSE,
    hvar='oneillmathews', sigT = 0.05, sigF = 0.05)

sink()

# DBC
# Exemplo 2 - Trat Quantitativo

dados_DBC2<-read.table("dbc2.txt", h=T)
str(dados_DBC2)

dados_DBC2<-transform(dados_DBC2, Bloco=factor(Bloco))
str(dados_DBC2)

sink("Saida ANOVA DBC - QUANTITATIVO.txt", type = c ("output"))# sink � usado para dar nome
#ou criar arquivo no bloco de notas

analiseDBC<-dbc(trat=dados_DBC2$Epoca, bloco=dados_DBC2$Bloco, resp=dados_DBC2$Brix, quali = FALSE)

plotres(analiseDBC)
sink()#para encerrar os dados no bloco de notas


citation("agricolae")

# Linear
# Muda o "grau" igual a 1
graficos(analiseDBC, grau = 1, mod = TRUE, main = "Gr�fico Regress�o Linear ", sub = " ",
         xlab = "Epoca", ylab = "Brix", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")

# Quadratica
# Muda o "grau" igual a 2

graficos(analiseDBC, grau = 2, mod = TRUE, main = "Gr�fico Regress�o Quadr�tica ", sub = " ",
         xlab = "Epoca", ylab = "Brix", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")


# Regress�o c�bica
# Muda o "grau" igual a 3
graficos(analiseDBC, grau = 3, mod = TRUE, main = "Gr�fico Regress�o Quadr�tica ", sub = " ",
         xlab = "Epoca", ylab = "Brix", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")


#MODELOS LINEARES
#modelo<-lm(resp~trat+bloco, data = dados)

##------------------------ ESQUEMA FATORIAL ---------------------------##


#FATORIAL DUPLO -> y = m + Fator1 + Fator2 + Fator1*Fator2 + "Bloco" + erro
FATDUPLO=read.table("fatdbc.txt", h=T)
str(FATDUPLO)
FATDUPLO<-transform(FATDUPLO, BLOCO=factor(BLOCO), TRAT=factor(TRAT))

AOVFAT<-aov(RESP ~ TRAT + DOSE + TRAT*DOSE + BLOCO, data = FATDUPLO)
AOVFAT
anova(AOVFAT)
str(FATDUPLO)

attach(FATDUPLO)#Para fixar qual planilha sera usada, assim n�o precisa fixar com $
#Ex. FATDUPLO$GEN
detach(FATDUPLO)#Para desfixar a planilha que era usada,
sink("Saida ANOVA FATORIAL DBC.txt", type = c ("output"))
analiseFAT<-fat2.dbc(fator1=TRAT, fator2=DOSE, bloco=BLOCO, resp=RESP, quali = c(TRUE, FALSE), mcomp = "tukey", 
                     fac.names = c("GEN�TIPOS", "DOSES"), sigT = 0.05, sigF = 0.05)
sink()
plotres(analiseFAT)

detach(FATDUPLO)

DBC<-dbc(trat=FATDUPLO$DOSE, bloco=FATDUPLO$BLOCO, resp=FATDUPLO$RESP, quali = FALSE)

graficos(DBC, grau = 2, mod = TRUE, main = "Gr�fico Regress�o ", sub = " ",
         xlab = "DOSE", ylab = "RESP", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")


##------------------------ PARCELA SUBDIVIDIDA ---------------------------##

#PARCELA SUB -> y = m +Fator1 +Bloco +Fator1*Bloco(ERRO A) +Fator2 +Fator1*Fator2 +erro B

SUB=read.table("psubdiv.txt", h=T)
str(SUB)
SUB<-transform(SUB, Bloco=factor(Bloco), Genotipo=factor(Genotipo))
str(SUB)

modelo <- aov(Altura ~ Bloco + Genotipo + Error(Bloco:Genotipo) + Aplic + Genotipo:Aplic, data=SUB) # termo Error para declarar erro A
#Um primeiro experimento e outro dentro dele assim � uma subdividida
#coloca o modelo y= bj+ ti + Eij + outro experimento APk+ AP*G+ eijk o �ltimo n�o escreve
summary(modelo)


attach(SUB)
sink("Saida ANOVA PARCELA SUB.txt", type = c ("output"))
SUBDIC<-psub2.dbc(Genotipo, Aplic, Bloco, Altura, quali = c(TRUE, TRUE), 
                  mcomp = "tukey", fac.names = c("Genotipos", "Aplica��o"), sigT = 0.05, 
                  sigF = 0.05)
sink()
detach(SUB)

library(FactoMineR)
library(factoextra)
##--------------------------------- FIM ----------------------------------##
DADOS=read.table("PCA.txt", h=T)
DADOS=DADOS[-5]
resp.pca<- PCA(DADOS, graph = T)
fviz_screeplot(resp.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_biplot(resp.pca,repel = TRUE)
get_eig(resp.pca)
resp.pca$ind$contrib
var <- get_pca_var(resp.pca)
var$contrib
cat("   ", "\n")
cat("------------------------------------------------------------------------", "\n")
cat("Variancia de cada dimens�o", "\n")

cat("------------------------------------------------------------------------", "\n")

fviz_pca_biplot(ACP.SNSEM)

