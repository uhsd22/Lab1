
colors <- c("setosa" = "darkorchid3", "versicolor" = "firebrick1", "virginica"= "aquamarine3","unknown"="yellow")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

#distance

EucDist <- function(u, v){
  sqrt(sum((u - v)^2))
}

#KNN
KNN <- function(xx,z,k = 6){
  rows0 <- dim(xx)[1]
  col0  <- dim(xx)[2] - 1
  #print(rows0)
  #print(col0)
  distances <- matrix(NA, rows0, col0)
  for(i in rows0:1){
    distances[i,] <- c(i, EucDist(xx[i,1:col0], z))
  }
  #print(distances)
  classes <- xx[order(distances[,2])[1:k], col0 + 1]
  class <- names(which.max(table(classes)))
  return(class)
}
#oneNN(iris[, 3:5], c(1, 1))

#draw knn function + map
ClassMapKNN <- function(){
  for(i in seq(0,7,0.1)){
    for(j in seq(0,2.5,0.1)){
      points(i , j , pch = 22, col = colors[KNN(iris[,3:5], c(i,j))])
    }
  }
}
#ClassMapKNN()

LOOKNN <- function(xx){
  row1 <- dim(xx)[1]
  col1 <- dim(xx)[2] - 1
  score_arr <- rep(0, row1)
  for(i in 1:row1){
    tmp_xx <- xx[-i,]
    tmp_xx_cnt <- dim(tmp_xx)[1]
    dist_matrix <- matrix(NA, tmp_xx_cnt, col1)
    for(j in tmp_xx_cnt:1){
      dist_matrix[j,] <- c(j, EucDist(tmp_xx[j,1:col1], xx[i, 1:col1]))
    }
    ordered_dist_matrix <- order(dist_matrix[,2])
    for (k in 1:row1){
      class <- names(which.max(table(xx[ordered_dist_matrix[1:k],col1 + 1])))
      if (class != xx[i, col1 + 1]){
        score_arr[ k ] <- score_arr[ k ] + 1/row1
      }
    }
  }
  FinalK <- which.min(score_arr)
  FinalLOO <- round(min(score_arr),3)
  text <- paste("k=",FinalK,"\nLOO=",FinalLOO)
  plot(score_arr,type = "l",main = "LOO(k) àëãîðèòìà KNN",xlab = "k", ylab="îöåíêà")
  points(FinalK, FinalLOO, pch=4, col="red")
  legend("topleft", legend = text)
}

LOOKNN(iris[, 3:5])


