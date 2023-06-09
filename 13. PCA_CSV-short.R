#Por Raul V. Ram�rez Velarde
#Agosto 2020
#De multiples fuentes
setwd("C:/Users/L00251678/Dropbox/Clases y Cursos/Analisis e Interpretacion UNIR/R")
#mtypol <- read.csv(file="C:/Users/Raul Ramirez/Dropbox/Clases y Cursos/Analisis e Interpretacion UNIR/R/Monterrey Pollution Data 2.csv", header=TRUE, sep=",")
#mtypol <- read.csv(file="C:/Users/L00251678/Dropbox/Clases y Cursos/Analisis e Interpretacion UNIR/R/Monterrey Pollution Data 2.csv", header=TRUE, sep=",")
mtypol <- read.csv(file="Monterrey Pollution Data 2.csv", header=TRUE, sep=",")
#ver el contenido 6 lineas
head(mtypol)

#Apply principle components
modelopca<-prcomp(mtypol,scale=TRUE)
#modelopca<-prcomp(mtypol,center=TRUE,scale=TRUE,na.action=na.exclude)
print(modelopca)
summary(modelopca)

#Scree plot
plot(modelopca,type="l")

#Plot Scores and Loadings
biplot(modelopca)

#Columns are eigenvectors
print(modelopca$rotation)

#Sample Scores
print(modelopca$x)

#Factor Loadings
Factors=matrix(0L, nrow = (dim(mtypol))[2], ncol = (dim(mtypol))[2])
for (i in 1:(dim(mtypol))[2]) {
  Factors[,i]<-modelopca$rotation[,i]*modelopca$sdev[i]
}
print(Factors)
view(Factors)

#Square cosines
cos2<-Factors^2
print(cos2)
View(cos2)

col_names = colnames(mtypol)
cos2df = data.frame(cos2)
cos2df = cbind(col_names,cos2df)
colnames(cos2df) = c("Vars","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","F13","F14")
View(cos2df)
