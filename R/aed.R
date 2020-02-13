# Script para análise exploratória de dados ---#
# parte do curso Projetos de análise de dados em R
# primeira versão em 2020-02-13
#-----------------------------------------------------------#

# Análise de dados do R para exemplificar

# Para editar os gráficos
?par

data("anscombe")

dim(anscombe) # dimensao dos dados, N de linhas e N de colunas
head(anscombe) # seis primeiras linhas dos dados
class(anscombe) # classe do objeto
str(anscombe) # estrutura do objeto

# A função apply aplica uma funcao a todas as linhas de um objeto
## media de todos os vetores x
apply(anscombe[,1:4], 2, mean)
## media de todos os vetores y
apply(anscombe[,5:8], 2, mean)
## calcula a variância de cada coluna
apply(anscombe, 2, var)

# correlação
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)
# coeficiente de regressão
## primeiro criamos objetos com as regressoes dos quatro conjuntos
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)
## vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)
## agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef) 

# Plotamos os gráficos porque não basta uma análise estatistica
# função par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2, 2), # abre uma janela gráfica com 2 linhas  e 2 colunas
    las=1, # deixa as legendas dos eixos na vertical
    bty="l") # tipo do box do grafico em L 
plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

par(mfrow=c(1, 1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna

# -----------------------------------------------------------------------
# Usaremos os dados iris para fazer imagens

head(iris)
summary(iris) 

table(iris$Species)

# media do comprimento de sepala por especie
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
# a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
# ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data = iris, mean)
# O mesmo pode ser feito para outras variáveis

# Anteriormente calculamos a média de uma variável. Mas como calcular a média de todas ao mesmo tempo?

# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol = 3, nrow = 4)
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]
for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)  
}

# Fazendo uma análise estatística mais detalhada

vars <- iris[,-5]
head(vars)
head(iris)
# Cálculo da mediana
apply(vars, 2, median)

# Cálculo da moda
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]

# Estatisticas de dispersão são muito importantes para entender os dados

# Variancia
apply(vars, 2, var)

# Desvio padrão
sd01 <- apply(vars, 2, sd)
# outra forma:
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))

# Vamos criar uma função para entender o coeficiente de variação
cv <- function(x){
  sd(x)/mean(x)*100
}
apply(vars, 2, cv)

# Quantis
apply(vars, 2, quantile)
# Mudando os quantis
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))
# Outras funções que podem ser colocados no apply é range, IQR (intervalo interquartil)


# Correlação
cor(vars)


# Métodos gráficos

# Gráfico de barras
barplot(table(iris$Species))

# Histogramas
par(mfrow=c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)
# Usando densidade de probabilidade ao invés de frequência
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)
# Vamos adicionar uma curva de densidade de probabilidade
par(mfrow=c(1, 2))
plot(density(iris$Sepal.Width)) # plot da curva de densidade
hist(iris$Sepal.Width, freq = FALSE) # plot da curva de densidade sobre o histograma de densidade
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao o comando add=TRUE
# Retornando à configuração inicial
par(mfrow=c(1, 1))

# Box Plot
boxplot(iris$Sepal.Length) # geral
boxplot(Sepal.Length ~ Species, data = iris) # por espécie
# etc

#E os Outliers?
boxplot(iris$Sepal.Width)
my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out
# qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

# No caso anterior consideramos outliers em relação à distribuição da variável para todas as espécies juntas. É razoável assumir que cada espécie tenha um padrão morfométrico distinto de modo que poderíamos identificar outliers de maneira espécie específica.
boxplot(Sepal.Width ~ Species, data = iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out
# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 & 
       iris$Species == "setosa", 
     c("Sepal.Width", "Species")]

# Entendendo a distribuição dos dados

# a função qqnorm compara os seus dados com uma distribuição normal
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"], 
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"], 
       main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"], 
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])
par(mfrow=c(1,1))

# Relações entre variáveis
pairs(vars)
# O GGaly oferece uma alternativa mais bonita, mas tem que carregar a biblioteca
library("GGally")
ggpairs(vars)