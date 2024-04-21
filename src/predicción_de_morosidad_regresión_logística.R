##############################
# Limpiar espacio de trabajo #
##############################

rm(list=ls())

####################
####################
# Lectura de datos #
####################
####################

setwd("/Users/mariedelvalle/Documents/courses/data-science-course/default-prediction/data/")
train <- read.csv("datamart_morosidad_training.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)
test <- read.csv("datamart_morosidad_test.csv", fileEncoding="utf-8",stringsAsFactors = TRUE)

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
# - Baja tasa de 1's en el default: : 10.47%
# - No hay missings

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

descriptivoManual(train[,c('Age', 'Income', 'Exp_Inc', 'Avgexp','Ownrent', 'Selfempl','Depndt', 'Inc_per', 'Cur_add', 'Major', 'Active', 'default')])

###########################
# 1.2 Exploraci?n gr?fica #
###########################

# Ejemplo de histograma (solo para variables continuas)

hist(train$Income, col="purple", border = F,
     main = "Histograma de ingresos del cliente",
     xlab = "Unidad monetaria", ylab = "Frecuencia")

hist(train$Age, col="purple", border = F,
     main = "Histograma de edad del cliente",
     xlab = "Años", ylab = "Frecuencia")

hist(train$Avgexp, col="purple", border = F,
     main = "Histograma de gasto mensual del cliente",
     xlab = "Unidad monetaria", ylab = "Frecuencia")

hist(train$Exp_Inc, col="purple", border = F,
     main = "Histograma de proporción de gasto mensual",
     xlab = "Unidad monetaria", ylab = "Frecuencia")

hist(train$Inc_per, col="purple", border = F,
     main = "Número de dependientes",
     xlab = "Unidad monetaria", ylab = "Frecuencia")

# Gr?ficos Box-Plot

boxplot(train$Income, col = "purple", main = "Boxplot de Variable Ingresos")

boxplot(train$Age, col = "purple", main = "Boxplot de Variable Age")

# Matriz de correlaci?n entre variables continuas
# para estudiar posibles multicolinealidades
# de cara a ajustar modelos de regresi?n

matrizCorrelacion<-cor(train[,c('Age', 'Income', 'Exp_Inc', 'Avgexp','Inc_per')], method = c("pearson"))

graphics.off()

#install.packages("corrplot")
library(corrplot)

corrplot(matrizCorrelacion, method="number", type="upper",tl.cex=0.5)


# Suele ser interesante no estudiar ?nicamente la distribuci?n de las variables aisladamente,
# sino tambi?n verlas en relaci?n al default. Para ello se puede recurrir a gr?ficos en los 
# que se vean conjuntamente ambas cosas.

# Convertimos el default en factor para usarlo ahora en los gr?ficos.

train$default <- as.factor(train$default)

# La variable n?mero de interacciones es un factor (ordinal)
# La redefinimos como variable de clase

train$Income <- as.factor(train$Income)

# Ejemplo de la distribuci?n de cada clase del default en la variable:
# V?lido para variables continuas

#install.packages("ggplot2")
library(ggplot2)

qplot(Age, data=train, fill = default, binwidth=2)
qplot(Exp_Inc, data=train, fill = default, binwidth=2)

# Otra manera: con la funci?n barplot

tablaFrecuencia<-table(train$default, train$Income)
barplot(tablaFrecuencia, col = c('springgreen1','purple'), border = F)

# El problema es que no se ve bien el n?mero de 1's en las clases residuales
# Adem?s, no se puede usar para variables explicativas continuas
# porque toman demasiados valores distintos
# Si tomaran pocos valores (por ejemplop: MESES_PERMANENCIA)
# s? se puede hacer, pero es neceario convertirla a factor


Age_f <- as.factor(train$Age)

tablaFrecuencia<-table(train$default, Age_f)
barplot(tablaFrecuencia, col = c('springgreen1','purple'), border = F)

# Otra manera: Definimos una funci?n que:
# - Para variables discretas genera una barra de longitud el valor medio del default
# - Para variables continuas, genera un histograma apilando 0's y 1's

graficoDependencia <- function(tablaExplicativas, default, numBinsHist){
  
  for (i in names(tablaExplicativas)){
    
    print(i)
    
    if (class(tablaExplicativas[,i])=="factor"){
      
      maintitle = paste0( i," vs default")
      tablaFrecuencia<-table(default, tablaExplicativas[,i])
      
      proporcion<-tablaFrecuencia[2,]/(tablaFrecuencia[1,]+tablaFrecuencia[2,])
      
      # Para que aparezcan los nombres en horizontal
      par(las=2)
      barplot(proporcion, col = c('purple'),
              border = F, main = maintitle, horiz=TRUE, cex.names=0.7)
      
    } else {
      
      xrange = seq(min(tablaExplicativas[is.na(i) == F,i], na.rm = T),max(tablaExplicativas[is.na(i) == F,i], na.rm = T),
                   (max(tablaExplicativas[is.na(i) == F,i], na.rm = T)-min(tablaExplicativas[is.na(i) == F,i], na.rm = T))/numBinsHist)
      
      conteo_1s = hist(tablaExplicativas[default==1,i], breaks=xrange, plot=F)$counts
      conteo_0s = hist(tablaExplicativas[default==0,i], breaks=xrange, plot=F)$counts
      proporcion_1s<-conteo_1s/(conteo_1s+conteo_0s)
      
      maintitle = paste0( i," vs default")
      
      barplot(rbind(conteo_0s, conteo_1s), col = c('springgreen1','purple'),
              border=F, names.arg=xrange[-1], space=1, las=1, main=maintitle)
    }
    
  }
}

graficoDependencia(train[,c('Age', 'Income', 'Exp_Inc', 'Avgexp','Ownrent', 'Selfempl','Depndt', 'Inc_per', 'Cur_add', 'Major', 'Active')], train$default, 40)


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

percentilesEdad <- quantile(train$Age, c(0,0.005,0.03,0.1,0.15,0.98, 0.99, 0.995,0.998, 1), na.rm = T)
percentilesEdad

trainSinOutliers <- train[train$Age >= percentilesEdad[[2]] & train$Age<=percentilesEdad[[9]],]

# N?mero de registros eliminados:

(nrow(train)-nrow(trainSinOutliers))

# Boxplot de variable age sin outliers

boxplot(trainSinOutliers$Age, col = "purple", main = "Boxplot de Variable Age Eliminando Outliers")


# Opci?n 2: Aplicamos logaritmo sobre la variable

train$Log_Age <- log(train$Age)

hist(train$Age, col="purple", border = F,
     main = "Histograma de Variable Age",
     xlab = "Años", ylab = "Frecuencia")

hist(train$Log_Age, col="purple", border = F,
     main = "Histograma de Variable log(Age)",
     xlab = "Años", ylab = "Frecuencia")

boxplot(train$Log_Age, col = "purple", main = "Boxplot de Variable log(Age)")

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

modelo.regresionLogistica.intercept <- glm(default ~ 1, data=trainSinOutliers, family=binomial(link="logit") )

# Modelo m?s complejo a generar: con todas las variables

modelo.regresionLogistica.todo <- glm(default ~ Age + Income + Exp_Inc + Avgexp + Ownrent + Selfempl + Depndt + Inc_per + Cur_add + Major + Active, data=trainSinOutliers, family=binomial(link="logit") )

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



##################################################
##################################################
# 5. Valoraci?n: ROC, Cobertura, Precisi?n, LIFT #
##################################################
##################################################

# Aplicamos la predicci?n sobre la tabla de test

predictTest.logisticaStepwise <- predict(modelo.regresionLogistica.stepwise, 
                                         newdata=test, 
                                         type="response")

# Se comparan los modelos ajustados en t?rminos de curva ROC

# install.packages("gplots")
library(gplots)

# install.packages("ROCR")
library(ROCR)

# Adjuntamos a la predicci?n el dato real para contruir la curva ROC

predictTestAux.logisticaStepwise <- prediction(predictTest.logisticaStepwise, 
                                               test$default, 
                                               label.ordering = NULL)

curvaRocTest.logisticaStepwise <- performance(predictTestAux.logisticaStepwise,"tpr","fpr")
plot(curvaRocTest.logisticaStepwise,main="Curva ROC",colorize=TRUE)
# L?nea base
abline(a=0,b=1,col="black")

auc_ROC<-performance(predictTestAux.logisticaStepwise, "auc")
auc_ROC@y.values[[1]]

# AUC = 0.7976889

# Otros gr?ficos

# Gr?fico de precisi?n

precision<-performance(predictTestAux.logisticaStepwise, "prec","rpp")
plot(precision, main="Gráfico de precisión", colorize=T)
prior=sum(train$default==1)/length(train$default)
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
plot(lift, main="Gráfico lift", colorize=T)
# L?nea base
abline(a=1,b=0,col="black")

