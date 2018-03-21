setwd("C:/UsersMcRodolpho/Documents/Ciência dos Dados/Desafio Titanic - Kaggle")

base <- read.csv("train.csv")
base1<- na.omit(base)

View(base1)

#Retirida das colunas que não interessam 

base1$PassengerId <- NULL
base1$Cabin<- NULL
base1$Ticket<- NULL

SaveBase <- base1

base1$Name<- NULL
##########################
amostra <- sample(2, 714, replace = TRUE, prob = c(0.7, 0.3))
treinamento <- base1[amostra == 1,]
testes <- base1[amostra == 2,]
#########################
testes$Survived<-NULL

########################
View(treinamento)

colnames(base1)

M <- glm(Survived~.,family = "binomial", data = treinamento)

step(M)

Majustado<- glm(Survived ~ Pclass + Sex + Age + SibSp, family = "binomial", 
                data = treinamento)

summary(Majustado)

inferencia <- predict(Majustado, newdata= testes, type="response")

testes$sobreviventes <- inferencia  > 0.45
View(testes)

Confusio <- table(testes$Survived, testes$sobreviventes)
show(Confusio)

ACC = sum(diag(Confusio))/sum(Confusio)   

show(ACC)

precisao = Confusio[2,2]/sum(Confusio[,2])
show(precisao)
