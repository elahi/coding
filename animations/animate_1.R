setwd("animations/figs_animate_1")

#number of frames or plots
frames <- 50

# function for creating file name with leading zeros
# makes it easier to process them sequentially
rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}

#loop through plots
for(i in 1:frames){
  name <- rename(i)
  
  #saves the plot as a .png file in the working directory
  png(name)
  sd <- 10
  n  <- 10000
  factor <- i * 2
  m  <- 50 + factor
  x  <- rnorm(n, m, sd)
  hist(x,
       xlim=c(0,200),
       ylim=c(0,2000),
       main = paste('Histogram of rnorm() n = ', n, ' mean = ', m, ' sd = ', sd),
  )
  dev.off()
}

#run ImageMagick
my_command <- 'convert *.png -delay 3 -loop 0 animation.gif'
system(my_command)
