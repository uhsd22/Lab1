countt <- function( x) { #плотность нормального многомерного распределения
  
  tmp <- (exp((t(x - M) %*% solve(Sigma) %*% (x - M))/(-2)))/(sqrt(det(Sigma) * (2 * pi)^dim(Sigma)[1]))

  return(tmp)
}
M <- c(5, 5)
left_x <- M[1] - 8
right_x <- M[1] + 8
bot_y <- M[2] - 8
up_y <- M[2] + 8


x <- seq(left_x, right_x, 0.05)
y <- seq(bot_y, up_y, 0.05)

par(bg = 'cadetblue', fg = 'white')
plot(left_x:right_x, bot_y:up_y,xlab = "x", ylab = "y", type = "n")


for (i in x) {
  
  for (j in y) {
    
    color <- adjustcolor("blue", countt(c(i, j))) #оттенки цвета
    points(i, j, pch = 22,col = color, bg = color)
    
  }
  
}

 z = outer(x, y, function(x, y) {
   unlist(lapply(1:length(x), function(i) countt(c(x[i], y[i])))) #двумерный вектор значений
   
 })
 #print(z)
 
 
 Sigma <- matrix(NA, 2, 2)
 # #matrix 1
 # Sigma[1, 1] <- 3
 # Sigma[2, 2] <- 3
 # Sigma[1, 2] <- 0
 # Sigma[2, 1] <- 0
 # #matrix 2
 #  Sigma[1, 1] <- 1
 #  Sigma[2, 2] <- 5
 #  Sigma[1, 2] <- 0
 #  Sigma[2, 1] <- 0
  # #matrix 3
  # Sigma[1, 1] <- 5
  # Sigma[2, 2] <- 1
  # Sigma[1, 2] <- 0
  # Sigma[2, 1] <- 0
  # #matrix 4
  # Sigma[1, 1] <- 3
  # Sigma[2, 2] <- 9
  # Sigma[1, 2] <- 3
  # Sigma[2, 1] <- 3
 #matrix 5
 Sigma[1, 1] <- 9
 Sigma[2, 2] <- 3
 Sigma[1, 2] <- 3
 Sigma[2, 1] <- 3
 contour(x,y,z,add = T)
 z <- 0