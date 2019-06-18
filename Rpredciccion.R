##regresion lineal simple
Age<-c(67, 40, 68, 64, 47, 71, 54, 59, 46, 52, 57, 63, 45, 55, 48, 51, 48, 77, 38, 61)
BP<-c(185, 154, 198, 189, 168, 200, 166, 190, 155, 172, 180, 173, 176, 172, 150, 176, 155, 192, 154, 185)
new <- data.frame(Age = seq(40, 75, 5)) # = 40, 45, 50, 55, 60, 65, 70, 75 (las 8 edades)
predict(lm(BP ~ Age), new, se.fit = TRUE)
pred.w.plim <- predict(lm(BP ~ Age), new, interval="prediction", level = 0.95)
pred.w.clim <- predict(lm(BP ~ Age), new, interval="confidence", level = 0.95)
matplot(new$Age,cbind(pred.w.clim, pred.w.plim[,-1]),
        lty=c(1,2,2,3,3),col=c("black","red","red","blue","blue"),type="l", ylab="predicted BP", xlab="Age")
points(Age,BP)

##regresion lineal multiple
# Weight in kg
Mass<- c(77.0, 85.5, 63.0, 80.5, 79.5, 94.0, 66.0, 69.0, 65.0, 58.0, 69.5, 73.0, 74.0, 68.0, 80.0, 66.0, 54.5, 64.0, 84.0, 73.0, 89.0, 94.0)
# Maximum circumference of forearm in cm
Fore<-c(28.5, 29.5, 25.0, 28.5, 28.5, 30.5, 26.5, 27.0, 26.5, 26.5, 28.5, 27.5, 29.5, 25.0, 29.5, 26.5, 24.0, 25.5, 30.0, 28.0, 29.0, 31.0)
# Maximum circumference of bicep in cm
Bicep<- c(33.5, 36.5, 31.0, 34.0, 36.5, 38.0, 29.0, 31.0, 29.0, 31.0, 37.0, 33.0, 36.0, 30.0, 36.0, 32.5, 30.0, 28.5, 34.5, 34.5, 35.5, 33.5)
# Distance around chest directly under the armpits in cm
Chest<- c(100.0, 107.0, 94.0, 104.0, 107.0, 112.0, 93.0, 95.0, 93.0, 96.0, 109.5, 102.0, 101.0, 98.5, 103.0, 89.0, 92.5, 87.5, 99.0, 97.0, 106.0, 106.0)
# Distance around neck, approximately halfway up in cm
Neck<- c(38.5, 39.0, 36.5, 39.0, 39.0, 39.0, 35.0, 37.0, 35.0, 35.0, 39.0, 38.5, 38.5, 37.0, 40.0, 35.0, 35.5, 35.0, 40.5, 37.0, 39.0, 39.0)
# Distance around shoulders, measured around the peak of the shoulder blades in cm
Shoulder<- c(114.0, 119.0, 102.0, 114.0, 114.0, 121.0, 105.0, 108.0, 112.0, 103.0, 118.0, 113.0, 115.5, 108.0, 117.0, 104.5, 102.0, 109.0, 119.0, 104.0, 118.0, 120.0)
# Distance around waist, approximately trouser line in cm
Waist<- c(85.0,  90.5, 80.5, 91.5, 92.0, 101.0, 76.0, 84.0, 74.0, 76.0, 80.0, 86.0, 82.0, 82.0, 95.5, 81.0, 76.0, 84.0, 88.0, 82.0, 96.0, 99.5)
# Height from top to toe in cm
Height<- c(178.0, 187.0, 175.0, 183.0, 174.0, 180.0, 177.5, 182.5, 178.5, 168.5, 170.0, 180.0, 186.5, 188.0, 173.0, 171.0, 169.0, 181.0, 188.0, 173.0, 179.0, 184.0)
# Maximum circumference of calf in cm
Calf<- c(37.5, 40.0, 33.0, 38.0, 40.0, 39.5, 38.5, 36.0, 34.0, 35.0, 38.0, 36.0, 38.0, 37.0, 37.0, 38.0, 32.0, 35.5, 39.0, 38.0, 39.5, 42.0)
# Circumference of thigh, measured halfway between the knee and the top of the leg in cm
Thigh<- c(53.0, 52.0, 49.0, 50.0, 53.0, 57.5, 50.0, 49.0, 47.0, 46.0, 50.0, 49.0, 49.0, 49.5, 52.5, 48.0, 42.0, 42.0, 50.5, 49.0, 51.0, 55.0)
# Distance around head in cm
Head<- c(58.0, 59.0, 57.0, 60.0, 59.0, 59.0, 58.5, 60.0, 55.5, 58.0, 58.5, 59.0, 60.0, 57.0, 58.0, 56.5, 57.0, 58.0, 56.0, 58.0, 58.5, 57.0)

mydata<-cbind(Mass, Fore, Bicep, Chest, Neck, Shoulder, Waist, Height, Calf, Thigh, Head)
mydata<-as.data.frame(mydata)
mydata
plot(mydata)
model_mult<-lm(Mass~Waist+Fore+Height, data=mydata)
summary(model_mult)

##vemos un  modelo con menos variables
summary(lm(Mass~Fore + Waist + Height, data = mydata))
summary(lm(Mass~Fore, data = mydata))
summary(lm(Mass~Bicep, data = mydata))
summary(lm(Mass~Fore+Bicep, data = mydata))
cor(mydata[,"Fore"],mydata[,"Bicep"])

##interacciones
summary(lm(Mass~Fore + Waist + Height+Fore:Waist, data = mydata))
############################
new<-matrix(c(30.5, 28.5, 28.5, 33.5, 30.0, 31.0, 93.0, 112.0, 89.0, 39.0, 35.0, 38.5, 109.0, 105.0, 121.0, 84.0, 82.0, 96.0, 188.0, 177.5, 178.5, 37.5, 34.0, 40.0, 55.0, 42.0, 49.0, 58.0 , 59.0, 60.0),nrow=3,byrow=F)
colnames(new)<-colnames(mydata)[2:11];
new<-as.data.frame(new);
#Predicción
predict(model_mult, new, se.fit = TRUE)
predict(model_mult, new, interval="confidence", level = 0.95)
predict(model_mult, new, interval="prediction", level = 0.95)

##arboles de decision
library(rpart)
str(cu.summary)
table(cu.summary$Reliability)
table(is.na(cu.summary$Reliability))
fit1 <- rpart(Reliability ~ Price + Country + Mileage + Type, data = cu.summary, parms = list(split = 'gini'))
library(rattle)
fancyRpartPlot(fit1)

