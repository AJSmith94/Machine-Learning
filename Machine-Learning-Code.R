install.packages("caret")
library(tidyverse)
library(caret)
library(rpart)
install.packages("dplyr")

banknote.data
colnames(banknote.data)<-c("Variance of Wavelet Transformed image","Skewness of Wavelet Transformed image","Kurtosis of Wavelet Transformed image","Entropy of image","Authentic")
head(banknote.data)

library(factoextra)
iris.pca <- prcomp(iris[,-5])
fviz_pca_ind(iris.pca,
             geom.ind = "point",col.ind = iris$Species,addEllipses = TRUE,legend.title = "Groups")

fviz_pca_ind(iris.pca,geom.ind="point",
                col.ind=iris$Species,addEllipses = TRUE,
                xlab="Principal component 1",ylab="Principal Component 2",
                title="Plot of first two principal components on Iris dataset")


plot(iris$Petal.Length, iris$Petal.Width, col=(bg = c("red", "blue", "green")[unclass(iris$Species)]),main="Plot of Iris dataset",xlab="Petal Length",ylab="Petal Width",pch=16)
legend("topleft", legend=levels(iris$Species), col=c("red", "blue", "green"),pch=16)

iris
plot(iris$Sepal.Length, iris$Sepal.Width)
str(iris)
install.packages("rpart")
library(rpart)
decisiontree<-rpart(Species~.,data=iris)
par(xpd=NA)
plot(decisiontree, uniform=TRUE,
     main="Classification Of Species for Iris")
text(decisiontree,digits=3)

print(decisiontree)

newsamples<-data.frame(Sepal.Length=c(5.0,5.2,6.7,5.4,6.1,6.1),Sepal.Width=c(3.2,3.5,3.2,2.7,3.0,3.2),Petal.Length=c(1.4,1.5,4.6,3.6,5.6,5.3),Petal.Width=c(0.2,0.2,1.5,1.2,4.4,2.1))
newsamples
Predicted.Species<- decisiontree %>% predict(newsamples,"class")
cbind(newsamples,Predicted.Species)
Predicted.Species


p<-predict(decisiontree,type="class")
confusionMatrix(p,iris$Species)

head(banknote.data)

decisiontreebank<-rpart(Authentic~.,banknote.data)
par(xpd=NA)
dev.off()
plot(decisiontreebank, uniform=TRUE,
     main="Classification Of Species for Iris")
text(decisiontreebank,digits=3,cex=0.45)

banknotes.pca <- prcomp(banknote.data[,-5])
library(factoextra)
fviz_pca_ind(banknotes.pca,geom.ind="point",
             col.ind=banknote.data$Authentic,addEllipses = TRUE,
             xlab="Principal component 1",ylab="Principal Component 2",
             title="Plot of first two principal components on banknotes dataset")

head(banknote.data)
dev.off()
Genuinerep<-rep("Genuine",762)
Counterfeit<-rep("Counterfeit",610)
Authentic<-c(Genuinerep,Counterfeit)
head(Authentic)
banknote.data<- cbind(banknote.data,Authentic)
banknote.data<-banknote.data[,-5]
head(banknote.data)
str(banknote.data)

install.packages("caret")
library(caret)

set.seed(999)
samplesize <- sample.int(n = nrow(banknote.data), size = floor(.75*nrow(banknote.data)), replace = F)
trainingset <- banknote.data[samplesize, ]
testset  <- banknote.data[-samplesize, ]

install.packages("rpart.plot")
library(rpart.plot)

head(banknote.data)
colnames(banknote.data)<-c("Variance","Skewness","Kurtosis","Entropy","Authentic")
banknotesdecisiontree<-rpart(Authentic~.,data=banknote.data,method="class")
par(xpd=NA)
plot(banknotesdecisiontree,main="Classification Of Authenticy for Banknotes")
text(banknotesdecisiontree,cex=0.7)
dev.off()

predictedauthenticity<-banknotesdecisiontree %>% predict(testset,type="class")
mean(predictedauthenticity==testset$Authentic)

print(banknotesdecisiontree)
library(caret)

head(banknote.data)

decisiontreebank<-rpart(Authentic~.,trainingset)
par(xpd=NA)
plot(decisiontreebank,main="Classification Of Authenticy for Banknotes")
text(decisiontreebank,cex=0.7)
banknote.data

pbank<-decisiontreebank %>% predict(testset,type="class")
confusionMatrix(pbank,testset$Authentic)

length(testset$Authentic)
length(trainingset$Authentic)

decisiontreebank2<-train(Authentic~.,data=trainingset,method="rpart",trControl=trainControl("cv",number=10),tuneLength=10)
decisiontreebank2
plot(decisiontreebank2,main="Plot of complexity parameter vs accuracy")

decisiontreebank2$bestTune
str(banknote.data)

library(randomForest)

decisiontreebankrf<-train(Authentic~.,data=trainingset,method="rf",trControl=trainControl("cv",number=10),tuneLength=10)
pbankrf<-decisiontreebankrf %>% predict(testset,type="class")
confusionMatrix(pbank,testset$Authentic)


plot(decisiontreebankrf)

decisiontreebankfull<-train(Authentic~.,data=banknote.data,method="rf",trControl=trainControl("cv",number=10),tuneLength=10)
pbankfull<-predict(decisiontreebankfull,type="prob")
confusionMatrix(pbankfull,testset$Authentic)

rfbank<-randomForest(Authentic~ ., data = trainingset, importance = TRUE)
pbankrf<-rfbank %>% predict(testset,type="class")
confusionMatrix(pbankrf,testset$Authentic)

rfbank

head(banknote.data)
tail(banknote.data)
install.packages("caret")

