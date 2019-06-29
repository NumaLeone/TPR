funcionPredictor<-function(notaAl1,notaAl2,notaAn,notaProg){
  student<-data.frame(notaAl1,notaAl2,notaAn,notaProg)
  load("model_nnet.RData")
  names(student)<-c("Algebra exam 1","Algebra exam 2","Analisis exam 1","IntroProg exam 1")
  return (predict(model_nnet2,student)==1)
}
save(funcionPredictor,file="funcionPredictor.RData")