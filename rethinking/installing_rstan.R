# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function", 
    file = M, sep = "\n", append = TRUE)

## To find out what version of g++ I am using:
## https://arstechnica.com/civis/viewtopic.php?t=736078

## My version is 4.2

## on OS X only:
cat("\nCC=clang", "CXX=clang++ -arch x86_64 -ftemplate-depth-256", 
    file = M, sep = "\n", append = TRUE)

## Verify that configuration is correct:
cat(readLines(M), sep = "\n")

## Now install RStan

# note: omit the 's' in 'https' if you cannot handle https downloads
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)

## Verify that toolchain works:
fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
                           ' )
fx( 2L, 5 ) # should be 10

##### HOW TO USE RSTAN ######

# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-use-rstan

library("rstan")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


