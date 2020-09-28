# Usando-Loops-para-o-valor-de-K-algoritmo-K-Nearest-Neighbors

######### Pré-processamento dos dados #########
library(readr)
library(ggplot2)
library(tidyverse)
library(caTools)
library(dplyr)
library(class)
library(e1071)
library(caret)
library(lattice)
library(gganimate)
##################################################

rm(list = ls())

base<-read_csv("algoritmo_anapolin_23.csv")

base$Candidatos = NULL
base$genero=NULL
base$escolaridade=NULL
base$civil=NULL
base$cor=NULL
base$coloridade.1=NULL
base$coloridade=NULL
base$partido =NULL
View(base)
str(base)
########### Organizando - classificador ###########   

base$classificacion= factor(base$classificacion, levels = c(0, 1,2,3), 
      labels = c(0, 0, 1, 1))
      
      ### Divisão entre treinamento e teste ### 
set.seed(23)
divisao = sample.split(base$classificacion, SplitRatio = 0.85)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

##### Algoritmo kNN ###### 

previsoes = knn(train = base_treinamento[, -7], test = base_teste[, -7],
     cl = base_treinamento$classificacion, k = 5)
     
matriz_confusao = table(base_teste$classificacion, previsoes)
print(matriz_confusao)
confusionMatrix(matriz_confusao)

########### Construindo o loop ############## 

for(i in 1:30){
  set.seed(23)
  previsoes = knn(train = base_treinamento[,-7], 
                  test= base_teste[,-7],cl= base_treinamento$classificacion,k=i)
  erro_previsao[i] =mean(base_teste$classificacion != previsoes)
}

print(erro_previsao)

################## Otimização -- Loops graficamente ################

eixo_x_values <- 1:30

error <- data.frame(erro_previsao,eixo_x_values)

graf_animation<-ggplot(error,aes(x=eixo_x_values,y=erro_previsao))+ 
geom_point()+ geom_line(color='red')+
  ylab("Erro de previsão")+
  xlab("Números de K-vizinhos mais próximos")+
  theme_minimal()+
  ggtitle("Comportamento do Erro de Previsão do kNN", 
          subtitle = "Elaboração de Luiz Paulo T. Gonçalves")+
  transition_reveal(
    along = eixo_x_values
  )
  
meu_gif<-animate(graf_animation, duration=20)

meu_gif
