
colors <- c("setosa" = "darkorchid3", "versicolor" = "firebrick1", "virginica"= "aquamarine3", "unknown"="white")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

#distance

EucDist <- function(u, v){
  sqrt(sum((u - v)^2))
}

#KwNN
KWNN <- function(xx, z, k=6, w=0.5){
  rows <- dim(xx)[1]
  cols <- dim(xx)[2] - 1
  distances <- matrix(NA, rows, cols)
  for(i in rows:1){
    distances[i,] <- c(i, EucDist(xx[i, 1:cols], z))
  }
    ordered_dist <- xx[order(distances[,2])[1:k],]
    classes <- names(table(ordered_dist[,cols+1]))
    weights <- rep(0,length(classes))
    for( i in 1:k){
      weights[ordered_dist[i,3]] <- weights[ordered_dist[i,cols+1]] + w^i
    }
    class <- classes[which.max(weights)]
    return(class)
}

plotKWNN <- function(z1,z2,xx=iris,k=6,w=0.5){
  points(z1, z2, pch = 21, col = colors[KWNN(xx[,3:5],c(z1,z2),k,w)])
}
classMapKWNN <- function(ir=iris,k=6,w=0.5){
  for (i in seq(0,7,0.1)) {
    for (j in seq(0,2.5,0.1)){
      plotKWNN(i,j,ir,k)
    }
  }
}
#classMapKWNN(iris)

LOOKWNN <- function(x, k=6){
  m <- dim(x)[1]
  n <- dim(x)[2]-1
  q <- seq(0.05,0.95,0.05)
  len <- length(q)
  mark <- rep(0,len)
  for(i in 1:m){
    x1 <- x[-i,]
    x1_m <- dim(x1)[1]
    dist <- matrix(NA, x1_m, n)
    for (j in x1_m:1){
      dist[j,] <- c(j, EucDist(x1[j,1:n], x[i,1:n]))
    }  
    ordered <- x1[order(dist[,2])[1:k],]
    classes <- names(table(ordered[,n+1]))
    for(w in 1:len){
      rang <- rep(0,length(classes))
      for(j in 1:k) rang[ordered[j,n+1]] <- rang[ordered[j,n+1]] + w^q[j]
      class <- classes[which.max(rang)]
      if(class != x[i,n+1]) {
        mark[w] <- mark[w] + 1/m
      }
    }
  }
  min_point <- c(q[which.min(mark)], round(min(mark),4))
  text <- paste("k=",k," q=",min_point[1],"\nLOO=",min_point[2],sep="")
  print(mark)
  print(which.min(mark))
  plot(q,mark,type = "l",main = "LOO(k,q) алгоритма KWNN",xlab = "q", ylab="оценка")
  points(min_point[1], min_point[2], pch=4, col="red")
  legend("topleft", legend = text)
}
LOOKWNN(iris)
