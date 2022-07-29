###################################
##      PARTE DO PROJETO 1       ##  
##      PRÉ - PROCESSAMENTO      ##  
###################################

library(readr)

base <- read.csv("breast-cancer.csv", stringsAsFactors = FALSE)

# Função que encontra a moda
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

#Preencher dados ausentes, pela moda
base$breast.quad[base$breast.quad == "?"] <- find_mode(base$breast.quad)
base$node.caps[base$node.caps =="?"] <- find_mode(base$node.caps)


#Conversão dos atributos qualitativos para quantitativos:

###################################
##  Aqui tive que mudar o valor  ##  
##      do atributo de saída!    ##
###################################

base$class = factor(base$class, levels = c('no-recurrence-events','recurrence-events'), labels = c(1,0))
base$irradiat = factor(base$irradiat, levels = c('no','yes'), labels = c(0,1))
base$breast = factor(base$breast, levels = c('left','right'), labels = c(0,1))
base$node.caps = factor(base$node.caps, levels = c('no','yes'), labels = c(0,1))
#Com 3 tipos
base$deg.malig = factor(base$deg.malig, levels = c('1','2','3'),labels = c(0,1,2))

base$age = factor(base$age, levels = c('20-29','30-39','40-49','50-59','60-69','70-79'),labels = c(0,1,2,3,4,5))
base$menopause = factor(base$menopause, levels = c('premeno','lt40','ge40'),labels = c(0,1,2))
base$tumor.size = factor(base$tumor.size, levels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54'),labels = c(0,1,2,3,4,5,6,7,8,9,10))
base$inv.nodes = factor(base$inv.nodes, levels = c('0-2','3-5','6-8','9-11','12-14','15-17','24-26'), labels = c(0,1,2,3,4,5,6))


#Criando novas colunas
base$left_low <- 0
base$left_low[base$breast.quad == "left_low"] <- 1

base$right_up <- 0
base$right_up[base$breast.quad == "right_up"] <- 1

base$left_up <- 0
base$left_up[base$breast.quad == "left_up"] <- 1

base$right_low <- 0
base$right_low[base$breast.quad == "right_low"] <- 1

base$central <- 0
base$central[base$breast.quad == "central"] <- 1


base$class <- as.numeric(as.character(base$class))
base$age <- as.numeric(as.character(base$age))
base$menopause <- as.numeric(as.character(base$menopause))
base$tumor.size <- as.numeric(as.character(base$tumor.size))
base$inv.nodes <- as.numeric(as.character(base$inv.nodes))
base$node.caps <- as.numeric(as.character(base$node.caps))
base$deg.malig <- as.numeric(as.character(base$deg.malig))
base$breast <- as.numeric(as.character(base$breast))
base$irradiat <- as.numeric(as.character(base$irradiat))

#Depois de dividir em 4 atributos o atributo breast.quad, consegui eliminar esse atributo
base$breast.quad = NULL


#Biblioteca para divisão da base
library(caTools)

#Porção da base de dados
set.seed(1)

#Quanto maior for o SplitRatio maior é a base de treinamento
divisao <- sample.split(base$class,SplitRatio = 0.703)

#Criando a base de teste e de treinamento
# 201 dados - 70.3% da base
base_treinamento <- subset(base,divisao == TRUE)

#85 dados - 29.7 % da base
#Criando a variável de teste
base_teste <- subset(base,divisao == FALSE)


#Normalização dos dados (re-escala ou padronização);

# Intervalos [0,1]

base_treinamento[,2:14] = scale(base_treinamento[,2:14])

base_teste[,2:14] = scale(base_teste[,2:14])

