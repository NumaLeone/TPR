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
index <- createDataPartition(finalQuals$Baja, p=0.70, list=FALSE)
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
predictors<-c("Analisis exam 1","Algebra exam 1","Algebra exam 2","IntroProg exam 1")

# modelo Random Forest
#model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')

# modelo con una red neuronal
#model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')

# modelo lineal generalizado
#model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')


fitControl <- trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 6)
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))


#gbm#fit control 4-4#rfe 4#0.7

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])



#Random Forest

model_rf3<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)


predictions<-predict.train(object=model_rf3,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])

#red neuronal
model_nnet1<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet',trControl=fitControl,tuneLength =10)
model_nnet2<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
predictions<-predict.train(object=model_nnet2,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])

#modelo lineal generalizado
model_glm1<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm',trControl=fitControl,tuneLength =15)
model_glm2<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')
predictions<-predict.train(object=model_glm1,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])


library(ROCR)
# función para graficar la curva AUROC
plotROC <- function(pred){
  perf<- performance(pred,"tpr","fpr")
  plot(perf)
  AUC<-performance(pred,"auc")@y.values[[1]]
  grid()
  text(.6,.2,sprintf("AUC=%0.3f", AUC))
  abline(0,1,col="red", lty = 2)
}

predaux<-prediction(as.numeric(predictions),testSet[,outcomeName])

perf <- performance(predaux, "auc")
perf@y.values[[1]]
plotROC(predaux)

student$`Algebra exam 1`<-input$notaAlgebra1
student$`Algebra exam 2`<-input$notaAlgebra2
student$`Analisis exam 1`<-input$notaAnalisis
student$`IntroProg exam 1`<-input$notaProg
prediction<-predict(model_nnet,student)
text<-"no dejara la carrera"
if(prediction<-0){
  text<-"baja"

   }
text

funcionPredictor<-function(notaAl1,notaAl2,notaAn,notaProg){
student<-data.frame(notaAl1,notaAl2,notaAn,notaProg)
names(student)<-c("Algebra exam 1","Algebra exam 2","Analisis exam 1","IntroProg exam 1")
return (predict(model_nnet,student)==1)
}
funcionPredictor(1,1,1,1)


funcionPredictor2<-function(notaAl1,notaAl2,notaAn,notaProg,model){
  student<-data.frame(notaAl1,notaAl2,notaAn,notaProg)
  names(student)<-c("Algebra exam 1","Algebra exam 2","Analisis exam 1","IntroProg exam 1")
  switch(model, 
         "Random Forest"={
           modelfinal=load("model_rf.RData")
         },
         "Red Neuronal"={
           modelfinal=load("model_nnet.RData")
          
         },
         "Gradient Boosting Machine"={
           modelfinal=load("model_gbm.RData")
         },
         "Modelo Lineal Generalizado"={
           modelfinal=load("PredictorDeDesertores/model_glm.RData")
         },
       

  )
  return (predict(modelfinal,student)==1)
}
text<-"Modelo Lineal Generalizado"
funcionPredictor2(1,1,1,1,text)
switch("bar", foo={"one"}, bar={"two"})

