library(readr)
library (caret)
library (e1071)
library(ggplot2)
library(GGally)
data = read.table("data_banknote_authentication.txt", sep=",")
colnames(data) <- c("variance","skewness","curtosis","entropy","genuine")
summary(data)
#setEPS()
#postscript("data_banknote.eps")
#ggpairs(data, aes(colour = genuine))
#dev.off()

data$genuine = factor(data$genuine) ##Convertir del dipo data a class.
set.seed(300)
particiones <- createDataPartition (y=data$genuine, p=0.7, list=FALSE) ## se toma el 70% del conjunto de datos para el entrenamiento
Entrenamiento <- data[particiones,] ##conjunto de entrenamiento
Prueba <- data[-particiones,]  ##conjunto de prueba


objcontrol <- trainControl(method="cv", number=30) ##El método de control es validación cruzada "Cross-Validation" y se implementa treinta veces
ajuste_knn <- train(genuine ~ variance + skewness + curtosis + entropy, data = Entrenamiento, method = "knn", trControl=objcontrol, tuneLength = 20) ##Realizar el proceso de clasificación
#plot(ajuste_knn) #Imprimir la exactitud obtenida con la validación cruzada.
setEPS()
postscript("knn_cv.eps")
plot(ajuste_knn)
dev.off()
ajuste_knn
Predicciones <- predict(ajuste_knn, newdata=Prueba)
confusionMatrix(Predicciones, Prueba$genuine)


ajust_nnet <- train( genuine ~ variance + skewness + curtosis + entropy, data = Entrenamiento, method='nnet', trControl=objcontrol, linout=FALSE, trace = FALSE, tuneGrid=expand.grid(.size=c(1:20),.decay=c(0,0.001,0.1))) 
setEPS()
postscript("nnet_cv.eps")
plot(ajust_nnet)
dev.off()
ajust_nnet

Predicciones <- predict(ajust_nnet, newdata=Prueba)
confusionMatrix(Predicciones, Prueba$genuine)


