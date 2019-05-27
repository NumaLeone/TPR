
#setwd("C:/Users/numal/Desktop/Facultad/TPR")
#setwd("C:/Users/Numa/Desktop/PredictorAcademico")

getwd()

library(readxl)
library(dplyr)
library(tidyr)


#entry19 <- read_excel("C:/Users/numal/Desktop/Facultad/TPR/INGRESO 2019-ANON.xlsx")
#entry18 <- read_excel("C:/Users/numal/Desktop/Facultad/TPR/INGRESO 2018-ANON.xlsx")
#entry1819 <- read_excel("C:/Users/numal/Desktop/Facultad/TPR/INGRESO 2018-2019-ANON.xlsx")
entry19 <- read_excel("INGRESO 2019-ANON.xlsx")
entry18 <- read_excel("INGRESO 2018-ANON.xlsx")
entry1819 <- read_excel("INGRESO 2018-2019-ANON.xlsx")


#2019
names(entry19)
entry19 <- entry19[,-19] # aportaba demasiado poco dato la columna de status ingenieria
entry19 <- rename(entry19, TDB='Tipo.de..Beneficio')
entry19 <-entry19[,-1] 
entry19 <-entry19[,-1] 

#filter(entry19, TDB==NA)
entry19[entry19 == "NA"] <- NA
entry19[entry19 == "N/A"] <- NA

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




#2018
names(entry18)
unique(entry18[,1])#son solo numeros random asi q los elimino
entry18<-entry18[,-1]
entry18<-entry18[,-1]
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

##ver campo tipo de beneficio
##ver que hacer con los campos con / en el de beneficio solicitado


