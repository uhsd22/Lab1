
colors <- c("setosa" = "darkorchid3", "versicolor" = "firebrick1", "virginica"= "aquamarine3")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

#distance

EucDist <- function(u, v){
  sqrt(sum((u - v)^2))
}

#1NN
oneNN <- function(xx,z){
rows0 <- dim(xx)[1]
col0  <- dim(xx)[2] - 1
#print(rows0)
#print(col0)
distances <- matrix(NA, rows0, col0)
for(i in rows0:1){
  distances[i,] <- c(i, EucDist(xx[i,1:col0], z))
}
#print(distances)
return(xx[order(distances[,2])[1], col0 + 1]);
}
#oneNN(iris[, 3:5], c(1, 1))

#draw 1nn function + map
ClassMapONN <- function(){
  for(i in seq(0,7,0.1)){
    for(j in seq(0,2.5,0.1)){
      points(i , j , pch = 22, col = colors[oneNN(iris[,3:5], c(i,j))])
    }
  }
}
ClassMapONN()
