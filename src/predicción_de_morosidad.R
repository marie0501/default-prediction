##############################
# Limpiar espacio de trabajo #
##############################

rm(list=ls())

####################
####################
# Lectura de datos #
####################
####################

setwd("/Users/mariedelvalle/Documents/courses/data-science-course/data/")
train <- read.csv("datamart_trainingvalidation_CSV.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
test <- read.csv("datamart_test_CSV.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)

###########################
###########################
# 1. An?lisis descriptivo #
###########################
###########################

############################
# 1.1 Exploraci?n num?rica #
############################

# Se estudia informaci?n estad?stica b?sica de las variables.
# De forma breve y sencilla:

summary(train)

# A destacar:
# - Baja tasa de 1's en el target: : 2.523%
# - Hay missings en la variable edad

# Podemos sacar un descriptivo m?s completo de los inputs de forma manual.
# Definimos una funci?n que recibe las variables explicativas y devuelve un sencillo
# descriptivo estad?stico de cada una.

# install.packages("moments")
library(moments)

# Para usar las funciones de asimetría y curtosis

# An?lisis de las explicativas

descriptivoManual <- function(tablaExplicativas){
  
  nombresVariables <- names(tablaExplicativas)
  
  for (i in nombresVariables){
    
    print(paste0("Variable:  ",i))
    print(paste0("Tipo de variable:  ", class(tablaExplicativas[,i])))
    
    if (class(tablaExplicativas[,i])=="factor"){
      
      # La moda no viene predefinida. La calculamos:
      valores <- unique(tablaExplicativas[,i])
      moda <- valores[which.max(tabulate(match(tablaExplicativas[,i], valores)))]
      print(paste0("Moda:  ", moda))
      print(paste0("Nº de niveles:  ", length(valores)))
      print(paste0("% de missings:  ", 100*(sum(is.na(tablaExplicativas[,i]))/length(tablaExplicativas[,i])),"%"))
      
    } else{
      
      print(paste0("Mínimo:  ", min(tablaExplicativas[,i], na.rm = T)))
      print(paste0("Máximo:  ", max(tablaExplicativas[,i], na.rm = T)))
      print(paste0("Media:  ", mean(tablaExplicativas[,i], na.rm = T)))
      print(paste0("Mediana:  ", median(tablaExplicativas[,i], na.rm = T)))
      print(paste0("Desviación típica:  ", sd(tablaExplicativas[,i], na.rm = T)))
      print(paste0("Asimetría:  ", skewness(tablaExplicativas[,i], na.rm = T)))
      print(paste0("Curtosis:  ", kurtosis(tablaExplicativas[,i], na.rm = T)))
      print(paste0("% de missings:  ", 100*(sum(is.na(tablaExplicativas[,i]))/length(tablaExplicativas[,i])),"%"))
    }
    print("*********************************************")
  }
}

descriptivoManual(train[,c("ANTIGUEDAD","MESES_PERMANENCIA","FACTURACION_MEDIA","EDAD_CLIENTE","RATIO_FACTURACION_3M","NUM_RECLAMACIONES_3M")])

###########################
# 1.2 Exploraci?n gr?fica #
###########################

# Ejemplo de histograma (solo para variables continuas)

hist(train$ANTIGUEDAD, col="purple", border = F,
     main = "Histograma de antig?edad del cliente",
     xlab = "Meses", ylab = "Frecuencia")

hist(train$EDAD_CLIENTE, col="purple", border = F,
     main = "Histograma de edad del cliente",
     xlab = "Meses", ylab = "Frecuencia")

hist(train$FACTURACION_MEDIA, col="purple", border = F,
     main = "Histograma de facturaci?n media del cliente",
     xlab = "Meses", ylab = "Frecuencia")

hist(train$MESES_PERMANENCIA, col="purple", border = F,
     main = "Histograma de meses de permanencia del contrato",
     xlab = "Meses", ylab = "Frecuencia")

hist(train$NUM_RECLAMACIONES_3M, col="purple", border = F,
     main = "Histograma de n?mero de reclamaciones del cliente",
     xlab = "Meses", ylab = "Frecuencia")

hist(train$RATIO_FACTURACION_3M, col="purple", border = F,
     main = "Ratio facturaci?n por edad del cliente",
     xlab = "Meses", ylab = "Frecuencia")

# Gr?ficos Box-Plot

boxplot(train$ANTIGUEDAD, col = "purple", main = "Boxplot de antig?edad")

# Matriz de correlaci?n entre variables continuas
# para estudiar posibles multicolinealidades
# de cara a ajustar modelos de regresi?n

matrizCorrelacion<-cor(train[,c("ANTIGUEDAD","MESES_PERMANENCIA","FACTURACION_MEDIA","EDAD_CLIENTE","RATIO_FACTURACION_3M","NUM_RECLAMACIONES_3M")], method = c("pearson"))

graphics.off()

#install.packages("corrplot")
library(corrplot)

corrplot(matrizCorrelacion, method="number", type="upper",tl.cex=0.5)

# No se muestran las correlaciones asociadas a la variable EDAD
# dado que esta variable presenta observaciones con missings
# Nos restringimos a las observaciones sin missings

trainSinEdadMissing<-subset(train,EDAD_CLIENTE>0)
matrizCorrelacion<-cor(trainSinEdadMissing[,c("ANTIGUEDAD","MESES_PERMANENCIA","FACTURACION_MEDIA","EDAD_CLIENTE","RATIO_FACTURACION_3M","NUM_RECLAMACIONES_3M")], method = c("pearson"))
corrplot(matrizCorrelacion, method="number", type="upper",tl.cex=0.5)

# Suele ser interesante no estudiar ?nicamente la distribuci?n de las variables aisladamente,
# sino tambi?n verlas en relaci?n al target. Para ello se puede recurrir a gr?ficos en los 
# que se vean conjuntamente ambas cosas.

# Convertimos el target en factor para usarlo ahora en los gr?ficos.

train$TARGET <- as.factor(train$TARGET)

# La variable n?mero de interacciones es un factor (ordinal)
# La redefinimos como variable de clase

train$NUM_RECLAMACIONES_3M <- as.factor(train$NUM_RECLAMACIONES_3M)

# Ejemplo de la distribuci?n de cada clase del target en la variable:
# V?lido para variables continuas

#install.packages("ggplot2")
library(ggplot2)

qplot(ANTIGUEDAD, data=train, fill = TARGET, binwidth=2)
qplot(MESES_PERMANENCIA, data=train, fill = TARGET, binwidth=2)

# Otra manera: con la funci?n barplot

tablaFrecuencia<-table(train$TARGET, train$NUM_RECLAMACIONES_3M)
barplot(tablaFrecuencia, col = c('springgreen1','purple'), border = F)

# El problema es que no se ve bien el n?mero de 1's en las clases residuales
# Adem?s, no se puede usar para variables explicativas continuas
# porque toman demasiados valores distintos
# Si tomaran pocos valores (por ejemplop: MESES_PERMANENCIA)
# s? se puede hacer, pero es neceario convertirla a factor

MESES_PERMANENCIA_F <- as.factor(train$MESES_PERMANENCIA)

tablaFrecuencia<-table(train$TARGET, MESES_PERMANENCIA_F)
barplot(tablaFrecuencia, col = c('springgreen1','purple'), border = F)

# Otra manera: Definimos una funci?n que:
# - Para variables discretas genera una barra de longitud el valor medio del target
# - Para variables continuas, genera un histograma apilando 0's y 1's

graficoDependencia <- function(tablaExplicativas, target, numBinsHist){
  
  for (i in names(tablaExplicativas)){
    
    print(i)
    
    if (class(tablaExplicativas[,i])=="factor"){
      
      maintitle = paste0( i," vs target")
      tablaFrecuencia<-table(target, tablaExplicativas[,i])
      
      proporcion<-tablaFrecuencia[2,]/(tablaFrecuencia[1,]+tablaFrecuencia[2,])
      
      # Para que aparezcan los nombres en horizontal
      par(las=2)
      barplot(proporcion, col = c('purple'),
              border = F, main = maintitle, horiz=TRUE, cex.names=0.7)
      
    } else {
      
      xrange = seq(min(tablaExplicativas[is.na(i) == F,i], na.rm = T),max(tablaExplicativas[is.na(i) == F,i], na.rm = T),
                   (max(tablaExplicativas[is.na(i) == F,i], na.rm = T)-min(tablaExplicativas[is.na(i) == F,i], na.rm = T))/numBinsHist)
      
      conteo_1s = hist(tablaExplicativas[target==1,i], breaks=xrange, plot=F)$counts
      conteo_0s = hist(tablaExplicativas[target==0,i], breaks=xrange, plot=F)$counts
      proporcion_1s<-conteo_1s/(conteo_1s+conteo_0s)
      
      maintitle = paste0( i," vs target")
      
      barplot(rbind(conteo_0s, conteo_1s), col = c('springgreen1','purple'),
              border=F, names.arg=xrange[-1], space=1, las=1, main=maintitle)
    }
    
  }
}

graficoDependencia(train[,c("ANTIGUEDAD","MESES_PERMANENCIA","FACTURACION_MEDIA","EDAD_CLIENTE","RATIO_FACTURACION_3M","NUM_RECLAMACIONES_3M")], train$TARGET, 40)

###############
###############
# 2. Muestreo #
###############
###############

####################
# 2.1 Bajomuestreo #
####################

set.seed(12345)

# Para generar un muestreo estratificado con respecto a la variable objetivo,
# con la proporci?n deseada de 0's y 1's.

bajomuestreo <- function(datos, variableObjetivo, proporcionUnosDeseada){
  
  numUnos <- nrow(subset(datos, variableObjetivo==1))
  numCeros <- round((numUnos/proporcionUnosDeseada)-numUnos)
  
  tablaCeros <- subset(datos, variableObjetivo==0)
  ceros <- sample(nrow(tablaCeros), numCeros, replace = FALSE)
  
  return(rbind(datos[variableObjetivo==1,], tablaCeros[ceros,]))
}

bajomuestreo_0_5 <- bajomuestreo(train, train$TARGET, 1/2)
table(bajomuestreo_0_5$TARGET)
bajomuestreo_0_33 <- bajomuestreo(train, train$TARGET, 1/3)
table(bajomuestreo_0_33$TARGET)

#####################
# 2.2 Sobremuestreo #
#####################

tablaUnos <- subset(train, train$TARGET==1)
tablaCeros <- subset(train, train$TARGET==0)

# Se replican unos con reemplazamiento

replicaUnos <- sample(nrow(tablaUnos),nrow(tablaCeros),replace=TRUE)
tablaUnos <- tablaUnos[replicaUnos,]
sobremuestreo_0_5 <- rbind(tablaCeros,tablaUnos)
table(sobremuestreo_0_5$TARGET)

############################
############################
# 3. Manipulaci?n de datos #
############################
############################

###############################
# 3.1 Tratamiento de outliers #
###############################

# Si desean tratarse los outliers, lo habitual es aplicar logaritmo
# o eliminar percentiles extremos #

# Ejemplos: #

# Opci?n 1: Eliminamos las edades que superen el percentil 0.995

percentilesEdad <- quantile(train$EDAD_CLIENTE, c(0.98, 0.99, 0.995, 1), na.rm = T)
percentilesEdad

trainSinOutliers <- train[train$EDAD_CLIENTE<=percentilesEdad[[3]],]

# N?mero de registros eliminados:

(nrow(train)-nrow(trainSinOutliers))

# Opci?n 2: Aplicamos logaritmo sobre la variable

train$LOG_EDAD_CLIENTE <- log(train$EDAD_CLIENTE)
test$LOG_EDAD_CLIENTE <- log(test$EDAD_CLIENTE)

hist(train$EDAD_CLIENTE, col="purple", border = F,
     main = "Histograma de edad del cliente",
     xlab = "A?os", ylab = "Frecuencia")

hist(train$LOG_EDAD_CLIENTE, col="purple", border = F,
     main = "Histograma de log(edad) del cliente",
     xlab = "A?os", ylab = "Frecuencia")

# Para el caso de las variables categ?ricas, pueden considerarse outliers aquellas
# clases con una baja tasa de ocurrencia. En este caso, una posibilidad es agruparlas
# en una ?nica categor?a, llamada por ejemplo "RESTO".

categoriaResto <- function(tablaExplicativas, umbral){
  
  tablaSalida <- tablaExplicativas
  
  for (i in names(tablaSalida)){
    
    if (class(tablaExplicativas[,i])=="factor"){
      
      tablaFrecuencias <- as.data.frame(table(tablaExplicativas[,i]))
      names(tablaFrecuencias) <- c(i, "frecuencia")
      tablaFrecuencias$porcentaje <- tablaFrecuencias$frecuencia/length(tablaExplicativas[,i])
      
      tablaSalida <- merge(tablaSalida, tablaFrecuencias[,c(i,"porcentaje")], by = i)
      
      if (min(tablaSalida$porcentaje)<umbral){
        
        levels(tablaSalida[,i]) <- c(levels(tablaSalida[,i]), "RESTO")
        tablaSalida[tablaSalida$porcentaje < umbral, i] <- "RESTO"
        tablaSalida$frecuencia <- NULL
        tablaSalida$porcentaje <- NULL
        tablaSalida[,i] <- factor(tablaSalida[,i])
        
      }
      
    }
    
  }
  
  return(tablaSalida)
  
}

# Por ejemplo: Agrupar todas las clases cuya tasa de ocurrencia est? por debajo de 0.05
# Si llamamos a la funci?n con todas las variables, sustituye su contenido

# Para ver c?mo hace la asignaci?n de provincias a resto, no sustituiremos su valor

# Para que funcione la funci?n, la variable a tratar debe estar codificada como FACTOR

unique(train$PROVINCIA)
PROVINCIA_TRANSFORMADA <- categoriaResto(as.data.frame(train$PROVINCIA), 0.05)
unique(PROVINCIA_TRANSFORMADA)

trainCatResto <- cbind(train,PROVINCIA_TRANSFORMADA)
names(trainCatResto)[13]="PROVINCIA_TRANSFORMADA"
colnames(trainCatResto)

# Finalmente, eliminar?amos la variable original
# si tenemos intenci?n de utilizar la transformada

trainCatResto$PROVINCIA <- NULL

# Las nuevas variables quedan como:

par(las=2)

barplot(table(train$PROVINCIA), col = "purple", border = F,
        main = "Variable provincia",horiz=TRUE, cex.names=0.7)

barplot(table(PROVINCIA_TRANSFORMADA), col = "purple", border = F,
        main = "Variable provincia re-categorizada")

###############################
# 3.2 Tratamiento de missings #
###############################

# S?lo hay missings en la variable edad

summary(train)

# Opci?n sencilla: Imputar con la media

#install.packages("mlr")
library(mlr)

trainSinMissings <- 
  impute(train, classes = list(integer = imputeMean(), factor = imputeMode()),dummy.cols = c("EDAD_CLIENTE"))
trainSinMissings <- as.data.frame(trainSinMissings$data)

summary(train$EDAD_CLIENTE)
summary(trainSinMissings$EDAD_CLIENTE)

# Antes de la imputaci?n

histograma<-hist(train$EDAD_CLIENTE,
                 data=train, 
                 xlab = "Edad", 
                 ylab = "Frequency", 
                 main = "Histograma Edad", 
                 breaks=20,
                 freq=TRUE,
                 col="blue")

# Despu?s de la imputaci?n

histograma<-hist(trainSinMissings$EDAD_CLIENTE,
                 data=trainSinMissings, 
                 xlab = "Edad", 
                 ylab = "Frequency", 
                 main = "Histograma Edad", 
                 breaks=20,
                 freq=TRUE,
                 col="blue")

# Altera la distribuci?n de la variable
# Hagamos ahora la imputaci?n por rango

trainSinMissings <- impute(train, 
                           cols = list(EDAD_CLIENTE = imputeHist(),LOG_EDAD_CLIENTE = imputeHist()),
                           dummy.cols = c("EDAD_CLIENTE","LOG_EDAD_CLIENTE")
                            )

trainSinMissings <- as.data.frame(trainSinMissings$data)

# Antes de la imputaci?n

histograma<-hist(train$LOG_EDAD_CLIENTE,
                 data=train, 
                 xlab = "Edad", 
                 ylab = "Frequency", 
                 main = "Histograma Edad", 
                 breaks=20,
                 freq=TRUE,
                 col="blue")

# Despu?s de la imputaci?n

histograma<-hist(trainSinMissings$LOG_EDAD_CLIENTE,
                 data=trainSinMissings, 
                 xlab = "Edad", 
                 ylab = "Frequency", 
                 main = "Histograma Edad", 
                 breaks=20,
                 freq=TRUE,
                 col="blue")

###################
###################
# 4. Modelizaci?n #
###################
###################

###########################
# 4.1 Regresi?n log?stica #
###########################

# Ajustamos con una estrategia de selecci?n por pasos

# Modelo de partida: solo t?rmino intercept

modelo.regresionLogistica.intercept <- glm(TARGET ~ 1, data=trainSinMissings, family=binomial(link="logit") )

# Modelo m?s complejo a generar: con todas las variables

modelo.regresionLogistica.todo <- glm(TARGET ~ ANTIGUEDAD+MESES_PERMANENCIA+FACTURACION_MEDIA+PROVINCIA+LOG_EDAD_CLIENTE+RATIO_FACTURACION_3M+NUM_RECLAMACIONES_3M, data=trainSinMissings, family=binomial(link="logit") )

# Se recorren todos los modelos comprendidos entre ellos
# en ambos sentidos (permitiendo la entrada y salida de variables)
# El objetivo es minimizar el AIC

modelo.regresionLogistica.stepwise <- step(modelo.regresionLogistica.intercept, scope=list(lower=modelo.regresionLogistica.intercept, 
                                                                                           upper=modelo.regresionLogistica.todo),direction="both")
modelo.regresionLogistica.stepwise

# Observamos los coeficientes del modelo definitivo
# para validar si son significativos y responden a
# la l?gica de negocio esperada

# install.packages("lmtest")
library(lmtest)

coeftest(modelo.regresionLogistica.stepwise)

# Demasiados valores para la variable PROVINCIA
# Generamos la categor?a RESTO para provincias residuales

unique(trainSinMissings$PROVINCIA)
PROVINCIA_TRANSFORMADA <- categoriaResto(as.data.frame(trainSinMissings$PROVINCIA), 0.05)
unique(PROVINCIA_TRANSFORMADA)

trainSinMissings <- cbind(trainSinMissings,PROVINCIA_TRANSFORMADA)
names(trainSinMissings)[13]="PROVINCIA_TRANSFORMADA"

# Volvemos a ajustar el modelo de regresi?n log?stica

modelo.regresionLogistica.intercept <- glm(TARGET ~ 1, data=trainSinMissings, family=binomial(link="logit") )
modelo.regresionLogistica.todo <- glm(TARGET ~ ANTIGUEDAD+MESES_PERMANENCIA+FACTURACION_MEDIA+PROVINCIA_TRANSFORMADA+LOG_EDAD_CLIENTE+RATIO_FACTURACION_3M+NUM_RECLAMACIONES_3M, data=trainSinMissings, family=binomial(link="logit") )
modelo.regresionLogistica.stepwise <- step(modelo.regresionLogistica.intercept, scope=list(lower=modelo.regresionLogistica.intercept, 
                                                                                           upper=modelo.regresionLogistica.todo),direction="both")

modelo.regresionLogistica.stepwise

# Adem?s de las estimaciones, queremos ver su significatividad (p-valores)

coeftest(modelo.regresionLogistica.stepwise)

# MESES_PERMANENCIA -0.0577350: Compromisos de permanencia cumplidos -> Mayor prob(baja)
# LOG_EDAD_CLIENTE -0.7906494: M?s joven (menos edad) -> Mayor prob(baja)
# ANTIGUEDAD -0.0039188: Mayor antig?edad (m?s fidelidad) -> Menor prob(baja)
# FACTURACION_MEDIA 0.0003227: M?s facturaci?n (m?s paga) -> Mayor prob(baja)
# NUM_RECLAMACIONES_3M: Coeficientes crecientes. M?s reclamaciones -> Mayor prob(baja)
# La variable PROVINCIA no entra en el modelo 

##################################################
##################################################
# 5. Valoraci?n: ROC, Cobertura, Precisi?n, LIFT #
##################################################
##################################################

# Aplicamos la predicci?n sobre la tabla de test

# En primer lugar hay que aplicar el logaritmo sobre
# la variable EDAD_CLIENTE

test$LOG_EDAD_CLIENTE <- log(test$LOG_EDAD_CLIENTE)

# Tambi?n hay que imputar igualmente valores
# de edad a los missings

testSinMissings <- impute(test, 
                          cols = list(EDAD_CLIENTE = imputeHist(),LOG_EDAD_CLIENTE = imputeHist()),
                          dummy.cols = c("EDAD_CLIENTE","LOG_EDAD_CLIENTE")
)

testSinMissings <- as.data.frame(testSinMissings$data)

# Tambi?n hay que convertir a factor la variable NUM_RECLAMACIONES_3M

testSinMissings$NUM_RECLAMACIONES_3M<-as.factor(testSinMissings$NUM_RECLAMACIONES_3M)

predictTest.logisticaStepwise <- predict(modelo.regresionLogistica.stepwise, 
                                         newdata=testSinMissings, 
                                         type="response")

# Se comparan los modelos ajustados en t?rminos de curva ROC

# install.packages("gplots")
library(gplots)

# install.packages("ROCR")
library(ROCR)

# Adjuntamos a la predicci?n el dato real para contruir la curva ROC

predictTestAux.logisticaStepwise <- prediction(predictTest.logisticaStepwise, 
                                               testSinMissings$TARGET, 
                                               label.ordering = NULL)

curvaRocTest.logisticaStepwise <- performance(predictTestAux.logisticaStepwise,"tpr","fpr")
plot(curvaRocTest.logisticaStepwise,main="Curva ROC",colorize=TRUE)
# L?nea base
abline(a=0,b=1,col="black")

auc_ROC<-performance(predictTestAux.logisticaStepwise, "auc")
auc_ROC@y.values[[1]]

# AUC = 0.6358188

# Otros gr?ficos

# Gr?fico de precisi?n

precision<-performance(predictTestAux.logisticaStepwise, "prec","rpp")
plot(precision, main="Gr?fico de precisi?n", colorize=T)
prior=sum(train$TARGET==1)/length(train$TARGET)
print(prior)
# L?nea base
abline(a=prior,b=0,col="black")

# Gr?fico de cobertura

cobertura<-performance(predictTestAux.logisticaStepwise, "rec","rpp")
plot(cobertura, main="Gráfico de cobertura", colorize=T)
# L?nea base
abline(a=0,b=1,col="black")

# Gr?fico de lift

lift<-performance(predictTestAux.logisticaStepwise, "lift","rpp")
plot(lift, main="Gr?fico lift", colorize=T)
# L?nea base
abline(a=1,b=0,col="black")

#########################
# 4.2 ?rbol de decisi?n #
#########################

##################################
# Basados en test de dependencia #
##################################

# install.packages("partykit")
library(partykit)
arbolDependence.fit <- ctree(TARGET~ANTIGUEDAD+MESES_PERMANENCIA+FACTURACION_MEDIA+PROVINCIA+LOG_EDAD_CLIENTE+RATIO_FACTURACION_3M+NUM_RECLAMACIONES_3M, 
                             data=trainSinMissings,control=ctree_control(alpha=0.05,maxdepth=3,minbucket = 30))

# Dibujar el ?rbol creado

graphics.off()
plot(arbolDependence.fit)

# Si se quisiera predecir
# test.arbolDependence<-predict(arbolDependence.fit,testSinMissings)

########################################
# Basados en m?tricas de incertidumbre #
########################################

# install.packages("rpart")
library(rpart)

arbolGini.fit <- rpart(TARGET~ANTIGUEDAD+MESES_PERMANENCIA+FACTURACION_MEDIA+PROVINCIA+LOG_EDAD_CLIENTE+RATIO_FACTURACION_3M+NUM_RECLAMACIONES_3M, 
                       data=trainSinMissings, method="class",
                       parms = list(split = "gini"),control= rpart.control(minbucket = 30, maxdepth = 3, cp=0.05))
arbolGini.fit

# El ?rbol obtenido no tiene hojas

# plot(arbolGini.fit) Dar? ERROR porque solo tiene el nodo ra?z. 
# No hay ?rbol porque ha sido totalmente podado
# La poda se ha realizado porque no se elimina de un nodo padre
# al nodo hijo un m?nimo de incertidumbre dada por el par?metro cp
# Relajamos el valor de cp a 0.002 (de 0.01 valor por defecto)

arbolGini.fit <- rpart(TARGET~ANTIGUEDAD+MESES_PERMANENCIA+FACTURACION_MEDIA+PROVINCIA+LOG_EDAD_CLIENTE+RATIO_FACTURACION_3M+NUM_RECLAMACIONES_3M, 
                       data=trainSinMissings, method="class",
                       parms = list(split = "gini"),control= rpart.control(minbucket = 30, maxdepth = 3, cp=0.002))
arbolGini.fit

# Con cp=-1, saldr?a el ?rbol de mayor profundidad porque no impone restricción de reducci?n de incertidumbre (no poda)

arbolGini.fit.SinPoda <- rpart(TARGET~ANTIGUEDAD+MESES_PERMANENCIA+FACTURACION_MEDIA+PROVINCIA+LOG_EDAD_CLIENTE+RATIO_FACTURACION_3M+NUM_RECLAMACIONES_3M, 
                               data=trainSinMissings, method="class",
                               parms = list(split = "gini"),control= rpart.control(minbucket = 30, maxdepth = 3, cp=-1))
arbolGini.fit.SinPoda

# install.packages("rattle")
library(rattle)

# install.packages("rpart.plot")
library(rpart.plot)

# install.packages("RColorBrewer")
library(RColorBrewer)

# Dibujamos el ?rbol

fancyRpartPlot(arbolGini.fit,caption=NULL,palettes=c("Purples"))

##################################################
##################################################
# 5. Valoraci?n: ROC, Cobertura, Precisi?n, LIFT #
##################################################
##################################################

predictTest.arbol <-predict(arbolGini.fit, type='prob',testSinMissings) 

# devuelve la probabilidad asociada a cada clase

# Adjuntamos a la predicci?n el dato real para contruir la curva ROC

predictTestAux.arbol <- prediction(predictTest.arbol[,2],testSinMissings$TARGET)

curvaRocTest.arbol <- performance(predictTestAux.arbol,"tpr","fpr")
plot(curvaRocTest.arbol,main="Curva ROC",colorize=TRUE)

# L?nea base
abline(a=0,b=1,col="black")

auc_ROC<-performance(predictTestAux.arbol, "auc")
auc_ROC@y.values[[1]]

# AUC = 0.7057269 > 0.6358188 (Regresi?n)

# Otros gr?ficos

# Gr?fico de precisi?n

precision<-performance(predictTestAux.arbol, "prec","rpp")
plot(precision, main="Gr?fico de precisi?n", colorize=T)
prior=sum(train$TARGET==1)/length(train$TARGET)
print(prior)
# L?nea base
abline(a=prior,b=0,col="black")

# Gr?fico de cobertura

cobertura<-performance(predictTestAux.arbol, "rec","rpp")
plot(cobertura, main="Gr?fico de cobertura", colorize=T)
# L?nea base
abline(a=0,b=1,col="black")

# Gr?fico de lift

lift<-performance(predictTestAux.arbol, "lift","rpp")
plot(lift, main="Gr?fico lift", colorize=T)
# L?nea base
abline(a=1,b=0,col="black")

#########################################################
# 4.3 ?rbol de decisi?n con matriz de costes/beneficios #
#########################################################

# Inclusi?n de costes por error (matriz de costee beneficios)
# Loss matrix: 0,1 por columnas y -,+ por filas
# Si digo "-" y era 1 (pierdo una baja), mayor coste
# Si digo "+" y era 0 (hago una campa?a inncesaria), menor coste
# NOTA: Loss matrix must have zero on diagonals

lossmatrix <- matrix(c(0,360,10,0), byrow = TRUE, nrow = 2)
arbolGini_Cost.fit <- rpart(TARGET~ANTIGUEDAD+MESES_PERMANENCIA+FACTURACION_MEDIA+PROVINCIA+LOG_EDAD_CLIENTE+RATIO_FACTURACION_3M+NUM_RECLAMACIONES_3M, 
                                                   data=trainSinMissings, method="class",
                                                   parms = list(prior = c(.025,.975),split = "gini", loss=lossmatrix),
                                                   control= rpart.control(minbucket = 30, maxdepth = 3, cp=-1))
graphics.off()
fancyRpartPlot(arbolGini_Cost.fit,caption=NULL,palettes=c("Purples"))

# Los cortes han cambiado

predictTest.arbol <-predict(arbolGini_Cost.fit, type='prob',testSinMissings) 

# devuelve la probabilidad asociada a cada clase

# Adjuntamos a la predicci?n el dato real para contruir la curva ROC

predictTestAux.arbol <- prediction(predictTest.arbol[,2],testSinMissings$TARGET)

curvaRocTest.arbol <- performance(predictTestAux.arbol,"tpr","fpr")
plot(curvaRocTest.arbol,main="Curva ROC",colorize=TRUE)

# L?nea base
abline(a=0,b=1,col="black")

auc_ROC<-performance(predictTestAux.arbol, "auc")
auc_ROC@y.values[[1]]

# AUC = 0.7359177 > 0.7057269

# Otros gr?ficos

# Gr?fico de precisi?n

precision<-performance(predictTestAux.arbol, "prec","rpp")
plot(precision, main="Gr?fico de precisi?n", colorize=T)
prior=sum(train$TARGET==1)/length(train$TARGET)
print(prior)
# L?nea base
abline(a=prior,b=0,col="black")

# Gr?fico de cobertura

cobertura<-performance(predictTestAux.arbol, "rec","rpp")
plot(cobertura, main="Gr?fico de cobertura", colorize=T)
# L?nea base
abline(a=0,b=1,col="black")

# Gr?fico de lift

lift<-performance(predictTestAux.arbol, "lift","rpp")
plot(lift, main="Gr?fico lift", colorize=T)
# L?nea base
abline(a=1,b=0,col="black")

#########################################################
# 4.5 Random Forest (entrenando sin clases balanceadas) #
#########################################################

# install.packages('randomForest')
library(randomForest)

# Nota: El Random Forest, no funciona con missings. Por ello, usamos: train_sinMissings

set.seed(12345)
modelo.randomForest <- randomForest(formula=TARGET ~ MESES_PERMANENCIA+FACTURACION_MEDIA+PROVINCIA+LOG_EDAD_CLIENTE+RATIO_FACTURACION_3M+NUM_RECLAMACIONES_3M,
                                    data=trainSinMissings,
                                    ntree=100, 
                                    nodesize=30, # observaciones por hoja
                                    maxnodes=8, # 2^3 = 8 (profundidad = 3)
                                    # Para valorar la importancia de cada uno de los 500 ?rboles #
                                    importance=TRUE,
                                    keep.forest=TRUE)

graphics.off()
varImpPlot(modelo.randomForest)

predictTest.randomForest <- predict(modelo.randomForest,type='prob', testSinMissings)

predictTestAux.randomForest <- prediction(predictTest.randomForest[,2],testSinMissings$TARGET)

auc_ROC<-performance(predictTestAux.randomForest, "auc")
auc_ROC@y.values[[1]]

# 0.8087607 > 0.7359177