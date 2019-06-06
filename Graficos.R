set.seed(1234)
df <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5),
                 rnorm(200, mean=65, sd=5)))
)

View(df)

library(ggplot2)

p <- ggplot(df, aes(x=weight)) + 
  geom_density()
p

p+ geom_vline(aes(xintercept=mean(weight)),
              color="blue", linetype="dashed", size=1) + geom_rug()





library(plyr)
mu <- ddply(df, "sex", summarise, grp.mean=mean(weight))

ggplot(df, aes(x=weight, fill=sex)) +
  geom_density()



library(lattice)
library(gplots)
# generamos 4 matrices de 5x10, cada una: 
y <- lapply(1:4, function(x) matrix(rnorm(50), 10, 5))
## Generaremos 4 variantes de mapas de calor:  
x1 <- levelplot(y[[1]], col.regions=colorpanel(40, "darkblue", "yellow", "white"), main="colorpanel")
x2 <- levelplot(y[[2]], col.regions=heat.colors(75), main="heat.colors")
x3 <- levelplot(y[[3]], col.regions=rainbow(75), main="rainbow")
x4 <- levelplot(y[[4]], col.regions=redgreen(75), main="redgreen")
print(x1, split=c(1,1,2,2))
print(x2, split=c(2,1,2,2), newpage=FALSE)
print(x3, split=c(1,2,2,2), newpage=FALSE)
print(x4, split=c(2,2,2,2), newpage=FALSE) 
y
heatmap(y[[1]], scale = "column")


# Peso en  kg
Mass<- c(77.0, 85.5, 63.0, 80.5, 79.5, 94.0, 66.0, 69.0, 65.0, 58.0, 69.5, 73.0, 74.0, 68.0, 80.0, 66.0, 54.5, 64.0, 84.0, 73.0, 89.0, 94.0)
# Circunferencia maxima del antebrazo en cm
Fore<-c(28.5, 29.5, 25.0, 28.5, 28.5, 30.5, 26.5, 27.0, 26.5, 26.5, 28.5, 27.5, 29.5, 25.0, 29.5, 26.5, 24.0, 25.5, 30.0, 28.0, 29.0, 31.0)
# Circunferencia maxima del bicep en cm
Bicep<- c(33.5, 36.5, 31.0, 34.0, 36.5, 38.0, 29.0, 31.0, 29.0, 31.0, 37.0, 33.0, 36.0, 30.0, 36.0, 32.5, 30.0, 28.5, 34.5, 34.5, 35.5, 33.5)
# Distancia alrededor del pecho directamente debajo de las axilas en cm
Chest<- c(100.0, 107.0, 94.0, 104.0, 107.0, 112.0, 93.0, 95.0, 93.0, 96.0, 109.5, 102.0, 101.0, 98.5, 103.0, 89.0, 92.5, 87.5, 99.0, 97.0, 106.0, 106.0)
# Distancia alrededor del cuello en cm
Neck<- c(38.5, 39.0, 36.5, 39.0, 39.0, 39.0, 35.0, 37.0, 35.0, 35.0, 39.0, 38.5, 38.5, 37.0, 40.0, 35.0, 35.5, 35.0, 40.5, 37.0, 39.0, 39.0)
# Distancia alrededor de los hombros en cm
Shoulder<- c(114.0, 119.0, 102.0, 114.0, 114.0, 121.0, 105.0, 108.0, 112.0, 103.0, 118.0, 113.0, 115.5, 108.0, 117.0, 104.5, 102.0, 109.0, 119.0, 104.0, 118.0, 120.0)
# Distancia alrededor de la cintura en cm.
Waist<- c(85.0,  90.5, 80.5, 91.5, 92.0, 101.0, 76.0, 84.0, 74.0, 76.0, 80.0, 86.0, 82.0, 82.0, 95.5, 81.0, 76.0, 84.0, 88.0, 82.0, 96.0, 99.5)
# Altura desde la parte superior a los pies en cm.
Height<- c(178.0, 187.0, 175.0, 183.0, 174.0, 180.0, 177.5, 182.5, 178.5, 168.5, 170.0, 180.0, 186.5, 188.0, 173.0, 171.0, 169.0, 181.0, 188.0, 173.0, 179.0, 184.0)
# Circunferencia máxima de la pantorrilla en cm.
Calf<- c(37.5, 40.0, 33.0, 38.0, 40.0, 39.5, 38.5, 36.0, 34.0, 35.0, 38.0, 36.0, 38.0, 37.0, 37.0, 38.0, 32.0, 35.5, 39.0, 38.0, 39.5, 42.0)
# Circunferencia del muslo en cm.
Thigh<- c(53.0, 52.0, 49.0, 50.0, 53.0, 57.5, 50.0, 49.0, 47.0, 46.0, 50.0, 49.0, 49.0, 49.5, 52.5, 48.0, 42.0, 42.0, 50.5, 49.0, 51.0, 55.0)
# Circunferencia de la cabeza en cm.
Head<- c(58.0, 59.0, 57.0, 60.0, 59.0, 59.0, 58.5, 60.0, 55.5, 58.0, 58.5, 59.0, 60.0, 57.0, 58.0, 56.5, 57.0, 58.0, 56.0, 58.0, 58.5, 57.0)

mydata<-cbind(Mass, Fore, Bicep, Chest, Neck, Shoulder, Waist, Height, Calf, Thigh, Head)
mydata<-as.data.frame(mydata)
mydata

plot(mydata)


#mass fore
#mass waist
#fore bicep
#mass calf

library(corrplot)
corrplot(cor(mydata), method= 'color')

model_mult<-lm(Mass~., data=mydata);
model_mult
 
summary(model_mult)
summary(lm(Mass~Fore+Waist+Calf+Bicep))
summary(lm(Mass~Fore + Waist))

new<-matrix(c(30.5, 28.5, 28.5, 33.5, 30.0, 31.0, 93.0, 112.0, 89.0, 39.0, 35.0, 38.5, 109.0, 105.0, 121.0, 84.0, 82.0, 96.0, 188.0, 177.5, 178.5, 37.5, 34.0, 40.0, 55.0, 42.0, 49.0, 58.0 , 59.0, 60.0),nrow=3,byrow=F)
colnames(new)<-colnames(mydata)[2:11];
new<-as.data.frame(new);

#Predicción
predict(model_mult, new, se.fit = TRUE)
predict(model_mult, new, interval="confidence", level = 0.95)
View(new)
