getwd()

library(readxl)
library(dplyr)
library(tidyr)



names(entry19)
entry19 <- entry19[,-19] # aportaba demasiado poco dato la columna de status ingenieria
entry19 <- rename(entry19, TDB='Tipo.de..Beneficio')

# filter(entry19, TDB==NA)
entry19[entry19 == "NA"] <- NA
entry19[entry19 == "N/A"] <- NA

# > which(entry19$Obtiene..BECA=='BAJA') con esto puedo ver donde esta lo que le paso en esa columna

entry19$Obtiene..BECA[which(entry19$Obtiene..BECA == 'BAJA')] <- NA # elimina los datos que le paso de esa columna
entry19$Obtiene..BECA[which(entry19$Obtiene..BECA == 'Si')] <- "OBTIENE"
entry19[entry19 == "N/A"] <- NA
entry19$Obtiene..BECA[which(entry19$Obtiene..BECA == 'No')] <- "NO OBTIENE"

