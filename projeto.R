# 1. Definição da técnica de validação a ser utilizada 
# (cross-validation, hold-out, leave-one-out,etc);

#Meu método cross-validation


library(caTools)
library(class)


#número de divisões
n.folds <- 5
n.elements <- as.integer(nrow(base_treinamento) / n.folds)
folds <- seq(1, nrow(base_treinamento), by = n.elements)


###################################
##            KNN                ##  
###################################

#soma matriz de confusão
soma_mcf_KNN = matrix(data = c(0,0,0,0) , nrow = 2, ncol = 2)

for (i in 1:(length(folds) - 1)) {
  
  
  ###################################
  ##       Cross-validation        ##  
  ###################################
  temp.validation <- base_treinamento[folds[i]:folds[i+1],]
  temp.train <- base_treinamento[-(folds[i]:folds[i+1]),]
  

  model <- class::knn(train = temp.train[,-1], test = temp.validation[,-1], 
                      cl = temp.train$class, k = 105)

  predsVal <- as.numeric(as.character(model))
  # predVal <- ifelse(predsVal < 2 , 1, 0)
  predVal <- predsVal
  
  # predValidas <- find_KNN(temp.train,temp.validation,n.folds)
  matriz_cf <- matriz_confusao(temp.validation$class, predVal)
  
  # matriz_cf
  
  soma_mcf_KNN = soma_mcf_KNN + matriz_cf
  
  print("Matriz_CF")
  print(matriz_cf)
  print("Soma Matriz")
  print(soma_mcf_KNN)
}

###################################
##            MLP                ##
###################################

library(RSNNS)

#soma matriz de confusão
soma_mcf_mlp = matrix(data = c(0,0,0,0) , nrow = 2, ncol = 2)

for (i in 1:(length(folds) - 1)) {
  
  
  ###################################
  ##       Cross-validation        ##  
  ###################################
  temp.validation <- base_treinamento[folds[i]:folds[i+1],]
  temp.train <- base_treinamento[-(folds[i]:folds[i+1]),]
  

  model <- mlp(	x = temp.train, 
                y = temp.train$class, 
                size = 5, 
                learnFuncParams = c(0.1), 
                maxit = 40, 
                inputsTest = temp.validation, 
                targetsTest = temp.validation$class)
  predsVal <- predict(model,temp.validation)
  predVal <- ifelse (predsVal > 0.5, 1, 0)
  
  # predValidas <- find_KNN(temp.train,temp.validation,n.folds)
  matriz_cf_mlp <- matriz_confusao(temp.validation$class, predVal)
  
  # matriz_cf
  
  soma_mcf_mlp = soma_mcf_mlp + matriz_cf_mlp
  
  print("Matriz_CF")
  print(matriz_cf_mlp)
  print("Soma Matriz")
  print(soma_mcf_mlp)
  
}


###################################
##        DECISION TREE          ##
###################################

library(tree)
#soma matriz de confusão
soma_mcf_tree = matrix(data = c(0,0,0,0) , nrow = 2, ncol = 2)

for (i in 1:(length(folds) - 1)) {
  
  
  ###################################
  ##       Cross-validation        ##  
  ###################################
  temp.validation <- base_treinamento[folds[i]:folds[i+1],]
  temp.train <- base_treinamento[-(folds[i]:folds[i+1]),]
  
  #Aplicação modelo
  model <- tree(temp.train$class ~ ., temp.train)
  predsVal <- predict(model, temp.validation[,-1])
  predVal <- ifelse (predsVal > 0.5, 1 , 0)
  
  matriz_cf_tree <- matriz_confusao(temp.validation$class, predVal)

  soma_mcf_tree = soma_mcf_tree + matriz_cf_tree
  
  print("Matriz_CF")
  print(matriz_cf_tree)
  print("Soma Matriz")
  print(soma_mcf_tree)
}

###################################
##            Funções            ##
###################################


###################################
##      MATRIZ DE CONFUSÃO       ##
###################################
matriz_confusao <- function(targetTest, predVal){
  tp <- sum((targetTest == 1) & (predVal == 1))
  fp <- sum((targetTest == 0) & (predVal == 1))
  tn <- sum((targetTest == 0) & (predVal == 0))
  fn <- sum((targetTest == 1) & (predVal == 0))
  confusionMat <- matrix(c(tp, fp, fn, tn), nrow = 2, ncol = 2, dimnames = list(c("1","0"), c("1","0")))
  return (confusionMat)
}


###################################
##          Acurácia             ##
###################################

find_acuracia <- function(m_cf){
  tp = m_cf[1,1]
  tn = m_cf[2,2]
  
  fn = m_cf[1,2]
  fp = m_cf[2,1]
  
  a = (tp + tn) / (tp + tn +  fn+ fp)
  return (a)
}

###################################
##        Recall(TVP)            ##
###################################

find_recall <- function(m_cf){
  tp = m_cf[1,1]
  fn = m_cf[1,2]
  
  r = tp / (tp+fn) 
  return (r)
}

###################################
##           Precisão            ##
###################################

find_precisao <- function(m_cf){
  tp = m_cf[1,1]
  fp = m_cf[2,1]
  
  r = tp / (tp+fp) 
  return (r)
}


###################################
##  Cálculos dos modelos de AM   ##
###################################

#KNN
cat("Acurácia KNN: " , find_acuracia(soma_mcf_KNN), "\n")
cat("Recall KNN: " , find_recall(soma_mcf_KNN), "\n")
cat("Precisão KNN: " , find_precisao(soma_mcf_KNN), "\n")

#MLP
cat("Acurácia MLP: " , find_acuracia(soma_mcf_mlp), "\n")
cat("Recall MLP: " , find_recall(soma_mcf_mlp), "\n")
cat("Precisão MLP: " , find_precisao(soma_mcf_mlp), "\n")

#Árvore de decisão
cat("Acurácia Árvore de Decisão: " , find_acuracia(soma_mcf_tree), "\n")
cat("Recall Árvore de Decisão: " , find_recall(soma_mcf_tree), "\n")
cat("Precisão Árvore de Decisão: " , find_precisao(soma_mcf_tree), "\n")


###################################
##          Baseline             ##  
###################################
for (i in 1:nrow(base_teste)){
  baseline[i]<-1
}

# KNN
B_KNN <- matriz_confusao(base_teste$class, baseline)
cat("Acurácia Baseline no KNN: " , find_acuracia(B_KNN), "\n")
cat("Recall Baseline no KNN: " , find_recall(B_KNN), "\n")
cat("Precisão Baseline no KNN: " , find_precisao(B_KNN), "\n")

#MLP
B_MLP <- matriz_confusao(base_teste$class, baseline)
cat("Acurácia Baseline MLP: " , find_acuracia(B_MLP), "\n")
cat("Recall Baseline MLP: " , find_recall(B_MLP), "\n")
cat("Precisão Baseline MLP: " , find_precisao(B_MLP), "\n")

# DECISION TREE 
B_TREE <- matriz_confusao(base_teste$class, baseline)
cat("Acurácia Baseline Árvore de Decisão: " , find_acuracia(B_TREE), "\n")
cat("Recall Baseline Árvore de Decisão: " , find_recall(B_KNN), "\n")
cat("Precisão Baseline Árvore de Decisão: " , find_precisao(B_KNN), "\n")

###################################
##            Final              ##
###################################

# KNN
model_KNN <- class::knn(train = base_treinamento[,-1], test = base_teste[,-1], 
                    cl = base_treinamento$class, k = 105)
predsVal_KNN <- as.numeric(as.character(model_KNN))
predVal_KNN <- predsVal_KNN
MC_KNK <- matriz_confusao(base_teste$class, predVal_KNN)

cat("Acurácia Final KNN: " , find_acuracia(MC_KNK), "\n")
cat("Recall Final KNN: " , find_recall(MC_KNK), "\n")
cat("Precisão Final KNN: " , find_precisao(MC_KNK), "\n")

#MLP
model_MLP <- mlp(x = base_treinamento, 
              y = base_treinamento$class, 
              size = 5, 
              learnFuncParams = c(0.1), 
              maxit = 20)

predsVal_MLP <- predict(model_MLP,base_teste)
predVal_MLP <- ifelse (predsVal_MLP > 0.5, 1, 0)

MC_MLP <- matriz_confusao(base_teste$class, predVal_MLP)

cat("Acurácia Final MLP: " , find_acuracia(MC_MLP), "\n")
cat("Recall Final MLP: " , find_recall(MC_MLP), "\n")
cat("Precisão Final MLP: " , find_precisao(MC_MLP), "\n")

# DECISION TREE 

model_TREE <- tree(base_treinamento$class ~ ., base_treinamento)
predsVal_TREE <- predict(model_TREE, base_teste[,-1])
predVal_TREE <- ifelse (predsVal_TREE > 0.5, 1 , 0)

MC_TREE <- matriz_confusao(base_teste$class, predVal_TREE)

cat("Acurácia Final TREE: " , find_acuracia(MC_TREE), "\n")
cat("Recall Final TREE: " , find_recall(MC_TREE), "\n")
cat("Precisão Final TREE: " , find_precisao(MC_TREE), "\n")
