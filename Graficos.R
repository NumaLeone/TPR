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
