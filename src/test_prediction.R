
library(MSBVAR)

source("multi_granger_test.R")

testts <- read.csv("tsdata\\usajpn2001.csv", sep=",", header=FALSE, stringsAsFactors=FALSE)
testts2 <- read.csv("tsdata\\usasau2001.csv", sep=",", header=FALSE, stringsAsFactors=FALSE)
testts3 <- read.csv("tsdata\\usaprk2001.csv", sep=",", header=FALSE, stringsAsFactors=FALSE)

#that's lazy work here :-(

y <- cbind(testts$V2, testts3$V2) #,  testts3$V2)

lag <- 3
n <- 2

    m <- ncol(y);
    results <- array(0, c(1,2));
    varnames <- dimnames(y)[[1]];
    
    yswap <- cbind(y[,1],y[,-1]);
    Y <- embed(yswap, lag + 1);
    X1 <- Y[, -(1:n)];
    X2 <- X1[, ((1:lag) * n) - (n-1)];
    restricted <- lm(Y[, 1] ~ X2);
    unrestricted <- lm(Y[, 1] ~ X1);
    ssqR <- sum(restricted$resid^2);
    ssqU <- sum(unrestricted$resid^2);

