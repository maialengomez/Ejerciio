library("readxl")
library("dplyr")
library("tidyr")
library("naniar")
library("VIM")
library("simputation")
library("missForest")
library("nortest")
library("EnvStats")
library("discretization")
library("dummies")
library("fastDummies")
library("BBmisc")

#1 y 2
getwd()
df<-read_excel("dataset examen.xlsx", sheet="DATA")

#3
str(df)

df$Sex<-as.character(df$Sex)
df$Evaluation<-as.character(df$Evaluation)
df$School<-as.character(df$School)
df$Sports<-as.logical(df$Sports)
df$AcademicCourse<-as.integer(df$AcademicCourse)
df$`studytime`<-as.integer(df$`studytime`)
df[,8:38]<-apply(df[,8:38], 2, as.numeric)
df <- as.data.frame(df)

nas<-miss_var_summary(df)
#primero vamos a jugar con las columnas iniciales 
#para generar variables dummies
df_dummies1 <- dummy_cols(df[,-c(1)])

#escalado
df_scale <- cbind(df[,1:7], apply(df[,8:13], 2, scale))

#normalización
df_normalize <- cbind(df[,1:7], apply(df[,8:13], 2, normalize, method='range', range = c(0,10)))
df_normalize2 <- cbind(df[,1:7], apply(df[,8:13], 2, normalize))
df_normalize3 <- cbind(df[,1:7], apply(df[,8:13], 2, normalize, method='standardize'))

#dibujo histogramas para comparar los distintos escalados que he realizado
hist(x = df_scale$Grades_English, main = "Histograma de escalado")
hist(x = df_normalize$Grades_English, main = "Histograma de normalización 1")
hist(x = df_normalize2$Grades_English, main = "Histograma de normalización 2")

#vamos a discretizar la variable grades_english pero 
#antes la completo
miss_var_summary(df[,8:13])

df_imp_kNN <- as.data.frame(kNN(df, variable=names(df[,8:13]), 
                  dist_var=names(df[,c(2,3,4,5,6)]), k=5))
miss_var_summary(df_imp_kNN[,8:13])

df_imp_kNN <- mutate(df_imp_kNN, Grades_English = ifelse(Grades_English<0, 0, ifelse(Grades_English>10, 10, Grades_English)))
df_imp_kNN <- mutate(df_imp_kNN, 'Grades_Life Science' = ifelse('Grades_Life Science'<0,0,ifelse('Grades_Life Science'>10, 10, 'Grades_Life Science')))
df_imp_kNN <- mutate(df_imp_kNN, 'Grades_Earth Science' = ifelse('Grades_Earth Science'<0,0,ifelse('Grades_Earth Science'>10, 10, 'Grades_Earth Science')))
df_imp_kNN <- mutate(df_imp_kNN, 'Grades_Physical Science' = ifelse('Grades_Physical Science'<0,0,ifelse('Grades_Physical Science'>10, 10, 'Grades_Physical Science')))
df_imp_kNN <- mutate(df_imp_kNN, Grades_Geography = ifelse(Grades_Geography<0,0,ifelse(Grades_Geography>10, 10, Grades_Geography)))
df_imp_kNN <- mutate(df_imp_kNN, 'Grades_Physical Education' = ifelse('Grades_Physical Education'<0,0,ifelse('Grades_Physical Education'>10, 10, 'Grades_Physical Education')))

summary(df_imp_kNN[,8:13])

miss_var <- distinct(miss_var_summary(df_imp_kNN[,c(8,2)]))
chiM <- chiM(df_imp_kNN[,c(8,2)], alpha = 0.05)
discretizacion_chiM <- as.data.frame(chiM$Disc.data)
cortes_chiM <- as.data.frame( chiM$cutp)

cortes_inferiores <- as.data.frame(cortes_chiM[1:(nrow(cortes_chiM)-1),])
names(cortes_inferiores) <- "cortes_inf"
cortes_superiores <- as.data.frame(cortes_chiM[2:nrow(cortes_chiM),])
names(cortes_superiores) <- "cortes_sup"

cortes_def <- cbind(cortes_inferiores, cortes_superiores)

df_discretizado <-cbind(df_imp_kNN[,-c(8)], discretizacion_chiM[,1])
df_discretizado <- rename(df_discretizado, "Grades_English"="discretizacion_chiM[, 1]")

discretizacion_dummies <- dummy_cols(df_discretizado[,c(2,44)])[,-c(1)]

conteo <- discretizacion_dummies %>%
  group_by(Grades_English) %>%
  summarize_all(sum)

#otros criterios de discretización
mdlp <- mdlp(df_imp_kNN[,c(8,2)])

CAIM <- disc.Topdown(df_imp_kNN[,c(8,2)], method=1)

####Ejercicio: sustituye en el df original dos variables numéricas (Grades_English y Grades_Geography)
####por sus correspondientes discretizaciones
####supervisa mediante entropía y CAIM las variables School y studytime
