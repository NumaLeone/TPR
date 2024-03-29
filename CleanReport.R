library(readxl)
library(dplyr)
library(tidyr)
library(colortools)
library(plotrix)
library(ggplot2)
library(plyr)



changeErrors <- function(str, tokenArray, fixedArray, booleanFixed = FALSE){
  newArray <- sapply(tokenArray, grepl, str, ignore.case = TRUE, fixed = booleanFixed)
  index <- which(newArray == TRUE)[1]
  if(!is.na(index)){
    fixedArray[index]
  }else{
    str
  }
}

entry19 <- read_excel("INGRESO 2019-ANON.xlsx")
entry18 <- read_excel("INGRESO 2018-ANON.xlsx")
entry19 <- entry19[,-19] # aportaba demasiado poco dato la columna de status ingenieria
entry19<- entry19[,-1]
entry19<- entry19[,-1]
entry19 <- select(entry19, -Tipo.de..Beneficio)
entry19<- select(entry19,-Porcentaje..Otorgado)
entry18<-entry18[,-1]
entry18<-entry18[,-1]
entry18 <- select(entry18, -Tipo.de..Beneficio)

colnames(entry19) <- c("Names", "Sex","Carreer","Cohort", "Entry", "CMATH","RMATH", "CPHYS", "RPHYS","MEAN", "REQBENEFIT","OBT.SCHOLARSHIP", "SCHOOLMEAN","STATUS","SCHOOL")
colnames(entry18) <- c("Names", "Sex","Carreer","Cohort", "Entry", "CMATH","RMATH", "CPHYS", "RPHYS","MEAN", "REQBENEFIT","OBT.SCHOLARSHIP", "SCHOOLMEAN","STATUS","SCHOOL")

entry<- rbind(entry18,entry19)


wrong <- c("NO","BAJA","OBTIENE" ,"BFI N/A","Si","N/A","MO","falta")
correct <- c("NO","NO","SI","SI","SI","NO","NO","NO")
entry$OBT.SCHOLARSHIP <- sapply(entry$OBT.SCHOLARSHIP, changeErrors, wrong, correct)
entry$OBT.SCHOLARSHIP[which(entry$OBT.SCHOLARSHIP != "SI")] <-"NO"

entry$OBT.SCHOLARSHIP[which(is.na(entry$OBT.SCHOLARSHIP))] <-"NO"


wrong <-c("Cuatrimestral","Febrero", "Octubre", "Septiembre","Agosto","Directo","Diciembre")
correct<-c("Cuatri.","Feb.","Oct.","Sep.","Agost.","Directo","Dic" )
entry$Entry <- sapply(entry$Entry,changeErrors,wrong,correct)

wrong<- c("NA","N/A", "-","x","AUS","AUSENTE","ausente","Es pase","Desaprobado","2","5","6","7","8","9")
correct <- c("0","0","0","0","0","0","0","4","2","2","5","6","7","8","9")
entry$CMATH <- sapply(entry$CMATH,changeErrors,wrong,correct)
entry$RMATH <- sapply(entry$RMATH,changeErrors,wrong,correct)
entry$CPHYS <- sapply(entry$CPHYS,changeErrors,wrong,correct)
entry$RPHYS <- sapply(entry$RPHYS,changeErrors,wrong,correct) 

entry$CMATH<- as.numeric(entry$CMATH)
entry$CPHYS<- as.numeric(entry$CPHYS)


wrong <- c("NO","BAJA","OBTIENE" ,"BFI N/A","Si")
correct <- c("NO","NO","SI","SI","SI")
entry$OBT.SCHOLARSHIP <- sapply(entry$OBT.SCHOLARSHIP, changeErrors, wrong, correct)

wrong <- c("Antonio Berni", "Bede","Cardenal Pironio", "Colegio del Pilar","E.E.S.T. N°2","Goethe", "Lincoln","Lasalle", "Los Robles", "Michael Ham","Northfield School
","Northlands","Oakhill","Pilgrims","San Felipe","San Jose","Santa Ines","Santa Maria","Sworn","Sowrn","St. Catherine's Moorlands","St. George´s","St. John's","St. Mary of the Hills","St. Matthew's College","Verbo Divino","Wellspring")

correct <- c("Antonio Berni", "Bede's Grammar School", "Cardenal Pironio", "Colegio Del Pilar","E.E.S.T N°2","Goethe","Lincoln","Lasalle","Los Robles","Michael Ham","Northfield School
","Northlands","Oakhill","Pilgrims","San Felipe","San Jose","Santa Ines","Santa Maria","Sworn","Sworn", "Moorlands","St. George´s","St. John's","St. Mary of the Hills","St. Matthew's College","Verbo Divino","Wellspring")

entry$SCHOOL <- sapply(entry$SCHOOL, changeErrors, wrong, correct)

barplot(table(entry$CMATH),main= "Notas Curso de Ingreso Matematica", col = rainbow(12))
pie3D(table(entry$CMATH), explode = 0.1, main="Notas Curso de Ingreso Matematica",labels = c("1","2","3","4","5","6","7","8","9","10"),labelcex = 0.9)
barplot(table(entry$CPHYS),main= "Notas Curso de Ingreso Fisica", col = rainbow(12))

approvedMath <- which(entry$CMATH > "3")
approvedPhys <- which(entry$CPHYS >"3")
approved <- intersect(approvedMath,approvedPhys)


approvedRows <- entry[approved,]

carreer<-as.factor(entry$Carreer)
sex<-as.factor(entry$Sex)
barplot(table(sex,carreer),beside=TRUE,legend.text=TRUE,col=c("hotpink","cyan"),main="Proporcion De Mujeres y Hombres Por Carrera")


pie(table(approvedRows$Sex),labels = c("Female","Male"),main = "Distribucion de aprobados sin recuperatorio entre sexos",col = c("pink","cyan"))




beca<-as.factor(entry$OBT.SCHOLARSHIP)
ingreso<-as.factor(entry$Entry)
barplot(table(beca,ingreso),legend.text=TRUE,col=c("#3B14AF","#E6399B"),ylab="Cantidad de Alumnos",xlab="Ingreso",main="Beca Por Curso De Ingreso")

#pie()#preguntar bauti sobre como hacer piechart que compare los que aprobaron todas con los que no














entry$CMATH <- as.numeric(entry$CMATH)
entry$Carreer <- as.factor(entry$Carreer)

p<-ggplot(entry, aes(x=CMATH, fill=Carreer)) +
  geom_density(alpha=0.5)
p + scale_fill_manual(values=c("#87a5ff", "#a5ff87","#ff6e92")) + theme(aspect.ratio = 0.7) + labs(title = "Notas Matematica") +   theme(plot.title = element_text(size = 35, face = "bold")) +  theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA)
)


save(entry,file = "entry.Rdata")


