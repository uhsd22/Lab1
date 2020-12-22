
colors <- c("setosa" = "darkorchid3", "versicolor" = "firebrick1", "virginica"= "aquamarine3", "unknown"="yellow")

#Ôóíêöèÿ îòðèñîâêè íàáîðà
plotIris <- function(ir=iris,label="Êëàññèôèêàöèÿ"){
  plot(ir[, 3:4], pch = 21, bg = colors[ir$Species], col = colors[ir$Species],
       xlab="Length of petal",ylab="Width of petal",main=label,asp = 1)
}

plotIris()
#Ôóíêöèÿ ìåòðèêè
EucDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}


#Ôóíêöèÿ îòðèñîâêè êàðòû êëàññèôèêàöèè àëãîðèòìà parsen
classMapParsen <- function(ir=iris,h,K=kE){
  for (i in seq(0,7,0.1)) {
    for (j in seq(0,2.5,0.1)){
      plotParsen(i,j,ir,h,K)
    }
  }
}

# Ôóíêöèè ÿäåð
kR <- function(z) 0.5 * (abs(z)<=1) # Ïðÿìîóãîëüíîå
kT <- function(z) (1 - abs(z))*(abs(z)<=1) #  Òðåóãîëüíîå
kQ <- function(z) (15/16)*(1 - z^2)^2 * (abs(z)<=1) # Êâàðòè÷åñêîå
kE <- function(z) (3/4)*(1-z^2) * (abs(z)<=1) # Åïàíå÷íèêîâà
kG <- function(z) {
  return ((2*pi)^(-0.5)*exp(-0.5*(z^2))) #Ãàóññ
}

#Ïàðçåíîâñêîå îêíî #h_opt: kR - 0.35; kT - 0.35, kQ - 0.35; kE - 0.35; kG - 0.1
parsen <- function(xx, z, h, K){
  row <- dim(xx)[1]
  col <- dim(xx)[2]-1
  count_classes <- length(names(table(xx[,col+1])))
  classes <- rep(0,count_classes)
  names(classes) <- names(table(xx[,col+1]))
  for(i in 1:row){
    y <- xx[i,col+1]
    dist <- EucDist(xx[i,1:col],z)
    w <- K(dist/h)
    classes[y] <- classes[y] + w
  }
  if(sum(classes) > 0) class <- names(which.max(classes))
  else class <- "unknown"
  return(class)
}
LOO_parsen <- function(x,K,labels="LOO äëÿ ïàðçåíîâñêîãî îêíà"){
  row <- dim(xx)[1]
  col <- dim(xx)[2] - 1
  params <- seq(0.1,2,0.05)
  mark <- rep(0,length(params))
  for (h in 1:length(params)){
    for (i in 1:row){
      x1 <- xx[-i,]
      class1 <- parsen(x1,xx[i,1:col],params[h],K)
      class2 <- xx[i,col+1]
      if (class1 != class2) mark[h] <- mark[h] + 1/row
    }
  }
}
