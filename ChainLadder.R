# Chainladder

##try

##try2

install.packages('ChainLadder')
library(ChainLadder)

demo(package = 'ChainLadder')
vignette('ChainLadder', package='ChainLadder')

require(ChainLadder)
data(package = "ChainLadder")
RAA

# Plotting triangles
plot(RAA)
# Setting the argument lattice = TRUE will produce individual plots for each origin period
plot(RAA, lattice = TRUE)
?plot.triangle

# Transforming triangles between cumulative and incremental representation
# incremental representation
raa.inc <- cum2incr(RAA)
raa.inc 
# cumulative representation
raa.cum <- incr2cum(raa.inc)
raa.cum

# Chain-Ladder methods
## Basic idea
n <- 10
f <- sapply(1:(n-1),
            function(i){
              sum(RAA[c(1:(n-i)), i+1])/sum(RAA[c(1:(n-i)),i])
            })
f

## Extrapolate the development ratios, e.g. assuming a log-linear model
dev.period <- 1:(n-1)
plot(log(f-1) ~ dev.period, main = "Log-Linear extrapolation of age-to-age factors")
tail.model <- lm(log(f-1) ~ dev.period)
abline(tail.model)
co <- coef(tail.model)
co

### Extrapolate another 100 dev. period
tail <- exp(co[1] + c(n:(n+100))*co[2]) + 1
f.tail <- prod(tail)
f.tail

### Plot the expected claims development patterns
plot(100*(rev(1/cumprod(rev(c(f, tail[tail>1.0001]))))), t="b",
     main="Expected claims development pattern",
     xlab="Dev. period", ylab="Development % of ultimate loss")

f <- c(f, f.tail)
fullRAA <- cbind(RAA, Ult = rep(0,10))
for(k in 1:n){
  fullRAA[(n-k+1):n, k+1] <- fullRAA[(n-k+1):n, k]*f[k] 
}

sum(fullRAA[,11] - getLatestCumulative(RAA))

## Loss Developmental Factor Method
linkratios <- c(attr(ata(RAA), "vwtd"), tail = 1.05)
round(linkratios, 3) # display to only three decimal places

LDF <- rev(cumprod(rev(linkratios)))
names(LDF) <- colnames(RAA) # so the display matches the triangle
round(LDF, 3)

currentEval <- getLatestCumulative(RAA)

# Reverse the LDFs so the first, least mature factor [1]
# is applied to the last origin year (1990)
EstdUlt <- currentEval * rev(LDF) 

# Start with the body of the exhibit
Exhibit <- data.frame(currentEval, LDF = round(rev(LDF), 3), EstdUlt)

# Tack on a Total row
Exhibit <- rbind(Exhibit, data.frame(currentEval=sum(currentEval), LDF=NA, EstdUlt=sum(EstdUlt), row.names = "Total"))
Exhibit


lmCL <- function(i, Triangle){
  lm(y~x+0, weights=1/Triangle[,i],
     data=data.frame(x=Triangle[,i], y=Triangle[,i+1]))
}
sapply(lapply(c(1:(n-1)), lmCL, RAA), coef)

## Mack ChainLadder
mack <- MackChainLadder(RAA, est.sigma = "Mack")
mack
mack$f
mack$FullTriangle
plot(mack)
plot(mack, lattice = TRUE)

## Using a subset of the triangle
## Weights
## Example, using the last 5 calender years of the triangle
calPeriods <- (row(RAA) + col(RAA) - 1)
(weights <- ifelse(calPeriods <= 5, 0, ifelse(calPeriods > 10, NA, 1)))
mackw <- MackChainLadder(RAA, weights = weights, est.sigma = "Mack")
mackw$f






