library(kernlab)

x <- iris[51:150, 3:4]
y <- c(rep(1,50), rep(-1, 50))
n <- dim(x)[2]
for (j in 1:n){
  x[ , j] <- (x[ , j] - min(x[ , j])) / (max(x[ , j]) - min(x[ , j]))
}

# SVM
d <- data.frame(x = x,y = y)
names(d) <- c("x1", "x2", "y")
svp <- ksvm(y ~ x1 + x2, data = d, type = "C-svc", C = 10, kernel = "vanilladot", scaled = c())
ymat <- ymatrix(svp)
w <- colSums(coef(svp)[[1]] * x[SVindex(svp),])
b <- b(svp)


# ROC
l <- dim(d)[1]
neg <- length(which(d[1:l, 3] == -1))
pos <- length(which(d[1:l, 3] == 1))
FRP <- vector()
TRP <- vector()
a <- matrix(0,0,2)
for (i in 1:l) {
  a <- rbind(a, c(i, as.double(w %*% as.double(d[i, 1:2]))))
}
d <- cbind(d, a[,2])
d <- d[order(d[,4], decreasing = TRUE), ]
FRP[1] <- 0
TRP[1] <- 0
AUC <- 0
for (i in 1:l) {
  if (d[i, 3] ==  -1) {
    FRP[i + 1] <- FRP[i] + 1 / neg
    TRP[i + 1] <- TRP[i]
    AUC <- AUC + TRP[i + 1] / neg
  } 
  else {
    FRP[i + 1] <- FRP[i]
    TRP[i + 1] <- TRP[i] + 1 / pos
  }
}

# draw
plot(
  c(min(x[,1]), max(x[,1])), c(min(x[,2]), max(x[,2])),
  type='n',xlab = "Длина лепестка", ylab = "Ширина лепестка", 
  main = "SVM. Линейное ядро. С = 1"
)
points(x[-SVindex(svp),1], x[-SVindex(svp),2], pch = 1, col = ifelse(ymat[-SVindex(svp)] < 0, "aquamarine4", "red"))
points(x[SVindex(svp),1], x[SVindex(svp),2], pch = 15, col = ifelse(ymat[SVindex(svp)] < 0, "aquamarine4", "red"))
abline(b/w[2], -w[1]/w[2])
abline((b + 1)/w[2], -w[1]/w[2], lty = 2)
abline((b - 1)/w[2], -w[1]/w[2], lty = 2)

plot(c(0, 1), c(0, 1), type = "n", xlab = "FRP", ylab = "TRP", main = "ROC-кривая")
points(FRP, TRP, type = "l")
points(c(0, 1), c(0, 1), type = "l")
legend("center", paste("AUC = ", AUC))

