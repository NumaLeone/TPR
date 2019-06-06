
#setwd("C:/Users/numal/Desktop/Facultad/TPR")
#setwd("C:/Users/Numa/Desktop/PredictorAcademico")

library(readxl)
library(dplyr)
library(tidyr)

changeErrors <- function(str, tokenArray, fixedArray, booleanFixed = FALSE){
  newArray <- sapply(tokenArray, grepl, str, ignore.case = TRUE, fixed = booleanFixed)
  index <- which(newArray == TRUE)[1]
  if(!is.na(index)){
    fixedArray[index]
  }else{
    str
  }
}

#entry19 <- read_excel("C:/Users/numal/Desktop/Facultad/TPR/INGRESO 2019-ANON.xlsx")
#entry18 <- read_excel("C:/Users/numal/Desktop/Facultad/TPR/INGRESO 2018-ANON.xlsx")
#entry1819 <- read_excel("C:/Users/numal/Desktop/Facultad/TPR/INGRESO 2018-2019-ANON.xlsx")
entry19 <- read_excel("INGRESO 2019-ANON.xlsx")
entry18 <- read_excel("INGRESO 2018-ANON.xlsx")
entry19 <- entry19[,-19] # aportaba demasiado poco dato la columna de status ingenieria
entry19<- entry19[,-1]
entry19<- entry19[,-1]
entry19 <- select(entry19, -Tipo.de..Beneficio)
entry19 <- rename(entry19, Status = 'Estado.del..Aspirante')
entry19 <- select(entry19, -TDB)
entry19<- select(entry19,-Porcentaje..Otorgado)
entry18<-entry18[,-1]
entry18<-entry18[,-1]
entry18 <- select(entry18, -Tipo.de..Beneficio)

colnames(entry19) <- c("Names", "Sex","Carreer","Cohort", "Entry", "CMATH","RMATH", "CPHYS", "RPHYS","MEAN", "REQBENEFIT","OBT.SCHOLARSHIP", "SCHOOLMEAN","STATUS","SCHOOL")
colnames(entry18) <- c("Names", "Sex","Carreer","Cohort", "Entry", "CMATH","RMATH", "CPHYS", "RPHYS","MEAN", "REQBENEFIT","OBT.SCHOLARSHIP", "SCHOOLMEAN","STATUS","SCHOOL")

entry<- rbind(entry18,entry19)


wrong <- c("NO","BAJA","OBTIENE" ,"BFI N/A","Si")
correct <- c("NO","NO","SI","SI","SI")
entry$OBT.SCHOLARSHIP <- sapply(entry$OBT.SCHOLARSHIP, changeErrors, wrong, correct)

wrong <-c("Cuatrimestral","Febrero", "Octubre", "Septiembre","Agosto","Directo","Diciembre")
correct<-c("Cuatrimestral","Febrero","Octubre","Septiembre","Agosto","Directo","Diciembre" )
entry$Entry <- sapply(entry$Entry,changeErrors,wrong,correct)

wrong<- c("NA","N/A", "-","AUS","AUSENTE","ausente","Es pase","Desaprobado")
correct <- c(NA,NA,NA,"A","A","A","Es pase","2")
entry$CMATH <- sapply(entry$CMATH,changeErrors,wrong,correct)
entry$RMATH <- sapply(entry$RMATH,changeErrors,wrong,correct)
entry$CPHYS <- sapply(entry$CPHYS,changeErrors,wrong,correct)
entry$RPHYS <- sapply(entry$RPHYS,changeErrors,wrong,correct) 


wrong <- c("NO","BAJA","OBTIENE" ,"BFI N/A","Si")
correct <- c("NO","NO","SI","SI","SI")
entry$OBT.SCHOLARSHIP <- sapply(entry$OBT.SCHOLARSHIP, changeErrors, wrong, correct)

wrong <- c("Antonio Berni", "Bede","Cardenal Pironio", "Colegio del Pilar","E.E.S.T. N°2","Goethe", "Lincoln","Lasalle", "Los Robles", "Michael Ham","Northfield School
","Northlands","Oakhill","Pilgrims","San Felipe","San Jose","Santa Ines","Santa Maria","Sworn","Sowrn","St. Catherine's Moorlands","St. George´s","St. John's","St. Mary of the Hills","St. Matthew's College","Verbo Divino","Wellspring")

correct <- c("Antonio Berni", "Bede's Grammar School", "Cardenal Pironio", "Colegio Del Pilar","E.E.S.T N°2","Goethe","Lincoln","Lasalle","Los Robles","Michael Ham","Northfield School
","Northlands","Oakhill","Pilgrims","San Felipe","San Jose","Santa Ines","Santa Maria","Sworn","Sworn", "Moorlands","St. George´s","St. John's","St. Mary of the Hills","St. Matthew's College","Verbo Divino","Wellspring")

entry$SCHOOL <- sapply(entry$SCHOOL, changeErrors, wrong, correct)

barplot(table(entry$CMATH),main= "Notas Curso de Ingreso Matematica", col = rainbow(12))



#2019
names(entry19)

#filter(entry19, TDB==NA)
entry[entry == "NA"] <- NA
entry[entry == "N/A"] <- NA
entry[entry == "AUS" | entry19 == "AUSENTE" | entry19 == "ausente"] <- "A"








# > which(entry19$Obtiene..BECA=='BAJA') con esto puedo ver donde esta lo que le paso en esa columna

entry19$Obtiene..BECA[which(entry19$Obtiene..BECA == 'BAJA')] <- NA # elimina los datos que le paso de esa columna
entry19$Obtiene..BECA[which(entry19$Obtiene..BECA == 'Si')] <- "OBTIENE"
entry19[entry19 == "N/A"] <- NA
entry19$Obtiene..BECA[which(entry19$Obtiene..BECA == 'No')] <- "NO OBTIENE"
entry19[entry19=="Cuatrimestral Part Time"] <-"Ingreso Cuatrimestral"
entry19[entry19=="Cuatrimestral Full Time"] <-"Ingreso Cuatrimestral"
entry19[entry19=="Cuatrimestral Full Time 19"] <-"Ingreso Cuatrimestral"
entry19[entry19=="Curso Febrero"] <-"Ingreso Febrero"
entry19[entry19=="Curso Febrero MAT"] <-"Ingreso Febrero"
entry19[entry19=="Febrero Libre"] <-"Ingreso Febrero"
entry19[entry19=="Curso Octubre Nordelta"] <-"Ingreso Octubre"
entry19[entry19=="Curso Octubre Pilar"] <-"Ingreso Octubre"
entry19[entry19=="Curso Septiembre"] <-"Ingreso Septiembre"
entry19[entry19=="Curso Septiembre(Libre)"] <-"Ingreso Septiembre"
entry19$Cal.MAT[entry19$Cal.MAT == "A"] <- "1"
entry19$Cal.MAT[entry19$Cal.FIS == "A"] <- "1"



#entry19<- select(entry19,-Beneficio..Solicitado....)




#2018
names(entry18)
unique(entry18[,1])#son solo numeros random asi q los elimino






entry18[entry18=="Ingreso Directo (i feb)"] <-"Ingreso Directo"
entry18[entry18=="Ingreso Directo (i sep)"] <-"Ingreso Directo"
entry18[entry18=="Ingreso Directo (i oct)"] <-"Ingreso Directo"
entry18[entry18=="Cuatrimestral Part Time"] <-"Ingreso Cuatrimestral"
entry18[entry18=="Ingreso Febrero MAT"]<- "Ingreso Febrero"
entry18[entry18=="Ingreso Octubre Pilar"]<-"Ingreso Octubre"
entry18[entry18=="Ingreso Libre Diciembre"]<-"Ingreso Diciembre"
entry18[entry18=="Cuatrimestral Full Time"]<-"Ingreso Cuatrimestral"
entry18[entry18=="Ingreso Libre Febrero"]<-"Ingreso Febrero"
entry18[entry18=="Ingreso Libre Febrero MAT"]<-"Ingreso Febrero"



#hay que definir que va a ser la "-"
entry18[entry18=="3"]<-"Desaprobado"
entry18[entry18=="2"]<-"Desaprobado"
entry18[entry18=="1"]<-"Desaprobado"
entry18[entry18=="  "]<-NA
entry18[entry18=="-"]<-NA
entry18[entry18=="Es pase"]<-""
entry18[entry18=="Es pase interno"]<-"4"
entry18[entry18=="AUS"]<-"A"
entry18[entry18=="x"]<-"-"
entry18[entry18=="MO"]<-"NO"
entry18[entry18=="falta doc y CI"]<-"NO"
entry18[entry18=="6,5"]<-"6"
entry18[entry18=="5.5"]<-"5"
entry18[entry18=="7.5"]<-"7"
entry18[entry18=="8.5"]<-"8"
entry18[entry18=="6.5"]<-"6"
entry18[entry18=="9.5"]<-"9"
#entry18<-entry18[,-14] #no tienen datos solo NA
entry18[entry18==" Instituto Alfredo R. Bufano (Mendoza)"] <-"Instituto Alfredo R. Bufano"
entry18[entry18=="Ipesmi (Misiones)"] <-"Ipesmi"
entry18[entry18=="Jose Manuel Estrada (Zarate)"]<-"Jose Manuel Estrada"
entry18[entry18=="La Salle (Florida)"]<-"La Salle"
entry18[entry18=="CIREG (Tierra del Fuego)"]<-"CIREG"
entry18[entry18=="Colegio Peruano Britanico (Peru)"]<-"Colegio Peruano Britanico"
entry18[entry18=="ESBA (San Miguel)"]<-"ESBA"
entry18[entry18=="Escuela del Alba (Lincoln)"]<-"Escuela del Alba"
entry18[entry18=="Escuela Provincial Técnica 760 Guardacostas Rio Iguazu (Comodoro Rivadavia)"]<-"Escuela Provincial Técnica 760 Guardacostas Rio Iguazu"
entry18[entry18=="Instituto Alfredo R. Bufano (Mendoza)"]<-"Instituto Alfredo R. Bufano"
entry18[entry18==" Lincoln (Del Viso) " ]<-"Lincoln"
entry18[entry18== "Lord Byron School (Peru)"]<-"Lord Byron School"
entry18[entry18=="Los Robles (Pilar)"]<-"Los Robles"
entry18[entry18=="Michael Ham (Nordelta)"]<-"Michael Ham"
entry18[entry18=="Michael Ham (Vicente Lopez)"]<-"Michael Ham"
entry18[entry18=="Northfield School (Escobar"]<-"Northfield School"
entry18[entry18=="Northlands (Nordelta)"]<-"Northlands"
entry18[entry18=="Pilgrims (Pacheco)"]<-"Pilgrims"
entry18[entry18=="Pilgrims (San Isidro)"]<-"Pilgrims"
entry18[entry18=="San Isidro Labrador (La Paz)"]<-"San Isidro Labrador"
entry18[entry18=="San Lucas (Olivos)"]<-"San Lucas"
entry18[entry18=="San Nicolas (Olivos)"]<-"San Nicolas"
entry18[entry18=="Santo Tomas (La Pampa)"]<-"Santo Tomas"
entry18[entry18=="St. Anne's School (Paraguay)"]<-"St. Anne's School"
entry18[entry18=="St. Hilda's (Hurlingam)"]<-"St. Hilda's"
entry18[entry18=="St. Paul's (Hurlingam)"]<-"St. Paul's"
entry18[entry18=="Sworn College (Moreno)"]<-"Sworn College"
entry18[entry18=="Wellspring School (Del Viso)"]<-"Wellspring School"
entry18[entry18=="E.E.S.T. N°2"]<-"E.E.S.T N°2"
entry18[entry18=="E.E.S.T. N° 3"]<-"E.E.S.T. N°3"

entry18<-entry18[,-11]


##entry1819




entry18feb<-entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Febrero"), ]
dim(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Febrero"), ])
table(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Febrero"),8])
## el 30% de los imgresantes de febrero obtuvieron beca

entry18Cuat<-entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Cuatrimestral"), ]
dim(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Cuatrimestral"), ])
table(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Cuatrimestral"),8])
## el 25% de los ingresantes cuatrimestrales obtuvieron beca

entry18Sep<-entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Septiembre"), ]
dim(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Septiembre"), ])
table(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Septiembre"),8])
## el 42% de los ingresantes de septiembre obtuvieron beca
entry18feb<-entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Octubre"), ]
dim(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Octubre"), ])
table(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Octubre"),8])
## el  10% obtuvo beca

entry18Dic<-entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Diciembre"), ]
dim(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Diciembre"), ])
table(entry18[which(entry18$Oportunidad..del.Ingreso=="Ingreso Diciembre"),8])
## Nadie obtuvo beca



entry18Muj<-entry18[which(entry18$Sexo=="F" ), ]
dim(entry18[which(entry18$Sexo=="F"& !is.na(entry18$Cal.MAT)), ])
table(entry18[which(entry18$Sexo=="F"),6])
## el 67% de las mujeres que rindieron matematica aprobaron

entry18Homb<-entry18[which(entry18$Sexo=="M" ), ]
dim(entry18[which(entry18$Sexo=="M"& !is.na(entry18$Cal.MAT)), ])
table(entry18[which(entry18$Sexo=="M"),6])
## el 60% de los hombres que rindieron matematica aprobaron

