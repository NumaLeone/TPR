library(caret)
library(dplyr)
library(randomForest)
library(e1071)
load("finalQuals.RData")
finalQuals[which(is.na(finalQuals$`Algebra exam 1`)),"Algebra exam 1"] <- 0
finalQuals[which(is.na(finalQuals$`Algebra exam 2`)),"Algebra exam 2"] <- 0
finalQuals[which(is.na(finalQuals$`Analisis exam 1`)),"Analisis exam 1"] <- 0
finalQuals[which(is.na(finalQuals$`IntroProg exam 1`)),"IntroProg exam 1"] <- 0
changeErrors <- function(str, tokenArray, fixedArray, booleanFixed = FALSE){
  newArray <- sapply(tokenArray, grepl, str, ignore.case = TRUE, fixed = booleanFixed)
  index <- which(newArray == TRUE)[1]
  if(!is.na(index)){
    fixedArray[index]
  }else{
    str
  }
}
finalQuals$Name<-NULL
error<-c("0.","1.","2.","3.","4.","5.","6.","7.","8.","9.")
correction<-c("0","1","2","3","4","5","6","7","8","9")
finalQuals$`Algebra exam 1`<-sapply(finalQuals$`Algebra exam 1`,changeErrors,error,correction)
finalQuals$`Algebra exam 2`<-sapply(finalQuals$`Algebra exam 2`,changeErrors,error,correction)
finalQuals$`Analisis exam 1`<-sapply(finalQuals$`Analisis exam 1`,changeErrors,error,correction)
finalQuals$`IntroProg exam 1`<-sapply(finalQuals$`IntroProg exam 1`,changeErrors,error,correction)
finalQuals=finalQuals %>% mutate_if(is.character, as.numeric)
finalQuals$Baja <- as.factor(finalQuals$Baja)
save(finalQuals,file = "finalQuals.Rdata")

#Train y Test
index <- createDataPartition(finalQuals$Baja, p=0.75, list=FALSE)
trainSet <- finalQuals[ index,]
testSet <- finalQuals[-index,]

control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)

outcomeName<-'Baja'

predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]

DesertorPrediction <- rfe(trainSet[,predictors],trainSet[,outcomeName],
                         rfeControl = control)

DesertorPrediction
#elije todas las 4  variables
predictors<-c("Analisis exam 1","Algebra exam 1","Algebra exam 2","IntroProg  exam 1")

# modelo Random Forest
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')

# modelo con una red neuronal
model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')

# modelo lineal generalizado
model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')

# modelo de incremento estocástico del gradiente
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
