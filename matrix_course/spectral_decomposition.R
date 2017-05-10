##' Example of spectral decomposition
##' Tuljapurkar's short course
##' 11 Oct 2016
##' 

# A matrix example
amat <- rbind(c(0.2, 0, 0, 0.5,2.1, 1.1),
              c(0.4, 0.2, 0, 0, 0.5, 1),
              c(0, 0.6, 0.2, 0, 0, 0),
              c(0, 0, 0.6, 0.2, 0, 0),
              c(0, 0, 0, 0.6, 0.2, 0),
              c(0, 0, 0, 0, 0.6, 0.3))

# Eigen-stuff

junk <- eigen(amat)
junk
lams <- junk$values
wvecs <- junk$vectors
junkt <- eigen(t(amat))
vvecs <- junkt$vectors

# Are the eigenvalues from junk and junkt the same?
matrix(c(lams, junkt$values), nrow = 2, byrow = T)

# Ranking eigenvalues
radius <- max(abs(lams))
r11 <- 1.02 * radius
plot(c(-r11, r11), c(-r11, r11), type = "n", 
     xlab = "Real(lambda)", ylab = "Im(lambda)")
points(lams, pch = "o", cex = 1.5)
abline(h = 0, v = 0, lty = 2)
radius <- abs(lams[1])
theta <- seq(0, 2 * pi, length = 200)
lines(x = radius * cos(theta), y = radius * sin(theta), lty = 4)

# Biorthogonality
sclar <- t(vvecs)%*%wvecs
sclar[abs(sclar) < 1e-12] <- 0
sclar

# Function to get integer j
projmat <- function(j) {
  wj <- wvecs[, j]
  vj <- vvecs[, j]
  sj <- matrix(sclar[j, j], nrow = 6, ncol = 6)
  projmat <- wj %*% t(vj)/sj
}

p1 <- projmat(1)
p2 <- projmat(2)
p3 <- projmat(3)
p4 <- projmat(4)
p5 <- projmat(5)
p6 <- projmat(6)

sumchk <- p1 + p2 + p3 + p4 + p5 + p6
sumchk[abs(sumchk) < 1e-12] <- 0
sumchk

# Powers
x2 <- amat %*% amat
x4 <- x2%*%x2
x8 <- x4%*%x4
x16 <- x8%*%x8
x17 <- x16%*%amat
x18 <- x17%*%amat
x18[1,1]/x17[1,1]
x17[1,1]/x16[1,1]
