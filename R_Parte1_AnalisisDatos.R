#install.packages('mongolite')
#install.packages("mlbench")
#install.packages("VIM")
#install.packages("simputation")
#install.packages('DMwR2')
#install.packages('gmodels')

library(mongolite)
library(tidyverse)
library(DBI)
library(RMySQL) 

library(gmodels)
library(ggplot2)

library(VIM) 
library(mlbench) 
library(simputation)
library(stringr)
# ==============================================================================
#  Analisis de Datos - Trabajo Final - Grupo 6 - Parte 1
# ==============================================================================
# Conectarse a la base de datos MySQL creada en EC2
aws_host = "ec2-54-83-172-52.compute-1.amazonaws.com"
db <- dbConnect(RMySQL::MySQL(),
                dbname = "dbSIS",
                host = aws_host,
                user = "usuario",
                password = "User1234+",
                Port     = 3306)
dbGetQuery(db,'SET NAMES utf8')
df_sis <- dbGetQuery(db, 'SELECT * FROM T_Resumen')

df_sis
dbDisconnect(db)

# Conectarse a la base de datos MongoDB Atlas
url_path="mongodb+srv://usuario:user1234@cluster0.kzmmo.mongodb.net/dbinei"
mongo <- mongo(collection="resumen", db="dbinei", url=url_path, verbose= TRUE)
df_inei <- mongo$find('{}','{}')
df_inei
rm(mongo)
# ==============================================================================
#Analizando los datos del SIS
df_afiliados <- df_sis
colnames(df_afiliados) <- c('IdUbigeo','Departamento','Provincia','Distrito','Genero','CantidadAfiliados')
df_afiliados
df_afiliados$Genero <- factor(df_afiliados$Genero, levels = c("FEMENINO", "MASCULINO"),ordered=T)
summary(df_afiliados)

#Distribución de afiliados por genero
frec <- table(df_afiliados$Genero)                      
prop <- round(prop.table(table(df_afiliados$Genero))*100 , digits = 2)      
genero <- t(rbind(frec, prop))  
genero
CrossTable(df_afiliados$Genero, format="SPSS")

nombres <- paste(names(table(df_afiliados$Genero)), "\n", prop, "%", sep="")  
pie(prop, labels = nombres, main="Distribución de afiliados por Genero")

barplot(prop, main="Distribución de afiliados por genero", 
        xlab="Genero", col = 2:4,
        ylab="Porcentaje de Afiliados")


#Datos perdidos de SIS
ndatos_sis <- nrow(df_afiliados)
ndatos_sis
# Identificar filas que tienen valores perdidos del SIS
fidx_perd_sis <- which(rowSums(is.na(df_afiliados))!=0)
fidx_perd_sis
# Porcentaje de filas perdidas del SIS
fil_perdidas_sis <- round(100*length(fidx_perd_sis)/ndatos_sis, digits = 2)
fil_perdidas_sis

#Grafico de datos perdidos
matrixplot(df_afiliados)

# Imputacion usando una medida de tendencia central
library(DMwR2)
df_afiliados_1 <- centralImputation(df_afiliados)
df_afiliados_1
summary(df_afiliados_1)

df_afiliados_2 <- initialise(df_afiliados, method="median")
df_afiliados_2
summary(df_afiliados_2)

# ==============================================================================
#Analizando los datos del INEI
str(df_inei)
df_poblacion <- df_inei
df_poblacion$IdUbigeo=str_pad(df_inei[,1]$ubigeo_inei, width=6, pad="0") #Completar con ceros a 6 digitos
df_poblacion$Departamento=df_inei[,1]$Departamento
df_poblacion$Provincia=df_inei[,1]$Provincia
df_poblacion$Distrito=df_inei[,1]$Distrito
df_poblacion$Genero=df_inei[,1]$Sexo
df_poblacion$TotalPoblacion=df_inei[,2]
df_poblacion$total=NULL
df_poblacion[1]=NULL
str(df_poblacion)
#Actualizar el campo Genero F por Femenino y M por Masculino
df_poblacion$Genero[df_poblacion$Genero == 'F'] <- 'FEMENINO' 
df_poblacion$Genero[df_poblacion$Genero == 'M'] <- 'MASCULINO' 
#Actualizar el campo Genero a Factor
df_poblacion$Genero <- factor(df_poblacion$Genero, levels = c("FEMENINO", "MASCULINO"),ordered=T)
summary(df_poblacion)
sum(df_poblacion$TotalPoblacion)

#Merge de los resultados
df_unido=merge(x = df_afiliados_2, y = df_poblacion, by = "IdUbigeo", all = TRUE)
df_unido$PorcentajeAP=round(df_unido$CantidadAfiliados/df_unido$TotalPoblacion,2)
str(df_unido)
write.csv(df_unido,"data.csv", row.names = TRUE)
summary(df_unido)


