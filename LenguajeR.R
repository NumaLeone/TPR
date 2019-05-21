##mensaje<- "Hola mundo"
#fecha<- date()
#repeticiones <-5

#print (fecha)

#for(i in 1:repeticiones){
 # print(mensaje)
#}


#c <- c(1,2,3)
#d<- rbind(c(1,2,3),c(3,2,6))

setwd("C:/Users/Numa/Desktop/PredictorAcademico")

atendidos <- c(728,632,596,689,745,865,694,583,657,643,794,887)
atendidos
class(atendidos)
plot(atendidos)

#atendidos2 <- ts(atendidos,2009,2010,12)
atendidos2 <- ts(atendidos,frequency=12,start=c(2009,1))

library("plyr")


dim(d)
dim(atendidos)
atendidos
length(atendidos)
dim(d) [1]
dim(d) [2]

sexo <- c("M","H","M","H","M","H")
sexo
sexo <- factor(sexo)
sexo
sexo <- factor(sexo,levels=c("H","M"),labels=c("Hombre","Mujer"))
sexo
table(sexo)

produccion=c(120,100,132,112,95,164,172,183,155,176,110,115,122,108,120)
maquina=c(27,27,27,27,27,32,32,32,32,32,55,55,55,55,55)
dia=c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
cbind(maquina,dia,produccion)

maquina <- factor(maquina)
maquina
#as.numeric(maquina)
#as.numeric(as.character(maquina))

a<- 2
b<- 4
(a<3)
(b<3)
(b<3)|(a<3)
(b<6)&(a<3)
any(c(a==2,a==3,a==4))
all(c(a==2,b==4,a<b))

edad<- c(22,23,24,25,56,6,7,8,56,67,78)
edad[5]

x<- (1:10)
x

x<-seq(1,20,by=2)
y<-seq(1,20,length = 8)
y
edad[2:5]
edad[c(1,3,7)]
