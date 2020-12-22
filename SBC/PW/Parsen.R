
colors <- c("setosa" = "darkorchid3", "versicolor" = "firebrick1", "virginica"= "aquamarine3", "unknown"="yellow")

#Функция отрисовки набора
plotIris <- function(ir=iris,label="Классификация"){
  plot(ir[, 3:4], pch = 21, bg = colors[ir$Species], col = colors[ir$Species],
       xlab="Length of petal",ylab="Width of petal",main=label,asp = 1)
}

plotIris()
#Функция метрики
EucDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}


#Функция отрисовки карты классификации алгоритма parsen
classMapParsen <- function(ir=iris,h=2,K=kE){
  for (i in seq(0,7,0.1)) {
    for (j in seq(0,2.5,0.1)){
      plotParsen(i,j,ir,h,K)
    }
  }
}

# Функции ядер
kR <- function(z) 0.5 * (abs(z)<=1) # Прямоугольное
kT <- function(z) (1 - abs(z))*(abs(z)<=1) #  Треугольное
kQ <- function(z) (15/16)*(1 - z^2)^2 * (abs(z)<=1) # Квартическое
kE <- function(z) (3/4)*(1-z^2) * (abs(z)<=1) # Епанечникова
kG <- function(z) {
  return ((2*pi)^(-0.5)*exp(-0.5*(z^2))) #Гаусс
}

#Парзеновское окно #h_opt: kR - 0.35; kT - 0.35, kQ - 0.35; kE - 0.35; kG - 0.1
parsen <- function(x, z, h, K){
  m <- dim(x)[1]
  n <- dim(x)[2]-1
  count_classes <- length(names(table(x[,n+1])))
  classes <- rep(0,count_classes)
  names(classes) <- names(table(x[,n+1]))
  for(i in 1:m){
    y <- x[i,n+1]
    dist <- EucDist(x[i,1:n],z)
    w <- K(dist/h)
    classes[y] <- classes[y] + w
  }
  if(sum(classes) > 0) class <- names(which.max(classes))
  else class <- "unknown"
  return(class)
}
LOO_parsen <- function(x,K,labels="LOO для парзеновского окна"){
  m <- dim(x)[1]
  n <- dim(x)[2] - 1
  params <- seq(0.1,2,0.05)
  mark <- rep(0,length(params))
  for (h in 1:length(params)){
    for (i in 1:m){
      x1 <- x[-i,]
      class1 <- parsen(x1,x[i,1:n],params[h],K)
      class2 <- x[i,n+1]
      if (class1 != class2) mark[h] <- mark[h] + 1/m
    }
  }
}
