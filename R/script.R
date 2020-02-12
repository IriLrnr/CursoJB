# Instalando o pacote necessário
install.packages("tidyr")

# Carregando os pacotes necessários
library("tidyr")

# Script para ler a tabela de dados formatada e manipulada manualmente. Relativo ao exercício do Diogo
table_format <- read.csv(file = "./data/data_processed.csv", sep = ",")

table_format

# Script para manipulação de dados em bases relacionais ---#
# parte do curso Projetos de análise de dados em R
# dados originais extraídos de Jeliazkov et al 2020 Sci Data
# (https://doi.org/10.1038/s41597-019-0344-7)
# primeira versão em 2020-02-12
#-----------------------------------------------------------#

?list.files()

files.path <- list.files(path = "./data/cestes", pattern = ".csv", full.names = TRUE)

comm <- read.csv(files.path[1])
coord <- read.csv(files.path[2])
envir <- read.csv(files.path[3])
splist <- read.csv(files.path[4])
traits <- read.csv(files.path[5])

# Comandos para a análise do conteúdo de uma tabela
head(splist)
dim(splist)
summary(splist)

# Podemos querer saber o número de linhas de uma tabela, por exemplo. Ou os nomes das colunas.
nrow(splist)

# todas as variáveis exceto a primeira coluna com o id
names(envir)[-1]
# contando quantas variáveis
length(names(envir)[-1])

#Qual a riqueza de cada área? Primeiro, precisamos transformar a nossa matriz que possui dados de abundância em uma matriz de presença e ausência.
comm.pa <- comm[, -1] > 0
# vamos nomear as linhas das planilhas com o id dos sites
row.names(comm.pa) <- envir$Sites
# Somamos o valor de uma linha para saber a riqueza da área
sum(comm.pa[1, ])
# Analisamos a riqueza total dos sitios em questão
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
summary(rich)

# Isolamos o vetor dos sitios
summary(envir$Sites)
# Podemos observar que seus elementos são numéricos. Vamos converter para fatores
class(envir$Sites)
# se usarmos apenas as.factor, não fazemos a conversão, vamos então fazer uma atribuição
envir$Sites <- as.factor(envir$Sites)
coord$Sites <- as.factor(coord$Sites)

# Agora, vamos juntas as tabelas de coordenadas e de ambiente
envir.coord <- merge (x = envir, y = coord, by = "Sites")
# Verificamos se a tabela foi devidamente mergida a partir da dimensão
dim(envir)
dim(coord)
dim(envir.coord)

# Transformando uma matrix espécie vs. área em uma tabela de dados
Sites <- envir$Sites
length(Sites)
n.sp <- nrow(splist)

comm.df <- gather(comm[, -1])

# Vamos renomear as colunas de comm.df
# nomes atuais
colnames(comm.df)
# modificando os nomes das colunas
colnames(comm.df) <-  c("TaxCode", "Abundance")
# checando os novos nomes
colnames(comm.df)

# Vamos adicionar os sites à tabela comm.df
# primeiro criamos a sequência
seq.site <- rep(Sites, each = n.sp)
# checando a dimensão
length(seq.site)
# adicionando ao objeto comm.df
comm.df$Sites <- seq.site
# checando como ficou
head(comm.df)

# Agora vamos juntar o resto das tabelas par a par
comm.sp <- merge(comm.df, splist, by = "TaxCode")
head(comm.sp)

colnames(traits)[1] <- "TaxCode"

comm.traits <- merge(comm.sp, traits, by = "TaxCode")
head(comm.traits)
head (envir.coord)

comm.total <- merge(comm.traits, envir.coord, by = "Sites")
head(comm.total)

# Exportamos tudo em um arquivo final
write.csv(x = comm.total, 
          file = "data/01_data_format_combined.csv", 
          row.names = FALSE)

dim(comm.total)
