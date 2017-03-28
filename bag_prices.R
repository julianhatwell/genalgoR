#Problem Description of Bag Prices; 
#adapted from "Modern Optimization with R" by Cortez, Paulo.
source("coelho_functions.R")

D=5 # dimension (number of prices)
MaxPrice=1000
Dim=ceiling(log(MaxPrice,2)) # size of each price (=10)
size=D*Dim # total number of bits (=50)

intbin=function(x) # convert binary to integer
{ 
  sum(2^(which(rev(x==1))-1)) 
} # explained in Chapter 3 

bintbin=function(x) # convert binary to D prices
{ # note: D and Dim need to be set outside this function
  s=vector(length=D)
  for(i in 1:D) # convert x into s:
  { ini=(i-1)*Dim+1;end=ini+Dim-1
  s[i]=intbin(x[ini:end])
  }
  return(s)
}

bprofit=function(x) # profit for binary x
{ 
  s=bintbin(x)
  s=ifelse(s>MaxPrice,MaxPrice,s) # repair!
  f= profit(s) # maximization task!
  return(f)
}

library(GA)
defaultControl <- gaControl()

k = 5
gabin_tourSelectionK <- function(object, ...) {
  gabin_tourSelection(object, k, ...)
}

gaControl("binary" = list(selection = "gabin_tourSelection2"))

genoptim <- ga(type = "binary"
               , fitness = bprofit
               , nBits = 50
               , popSize = 50
               , maxiter = 50
               , selection = gabin_tourSelectionK
               , seed = 123)

# optimum solution for ﬁve different bags (D = 5)
# is x D c.414;404;408;413;395/
# , which corresponds to an estimated proﬁt of $43,899. 

genoptim@fitnessValue
genoptim@summary
plot(genoptim)

# Tournament size 2:5
# 30 independent runs
gaControl("binary" = list(selection = "gabin_tourSelectionK"))
gaKTour30 <- list()
for (j in 2:5) {
  gabin_tourSelectionK <- function(object, ...) {
    gabin_tourSelection(object, j, ...)
  }
  
  gaKTour30[[j]] <- list()
  for (i in 1:30) {
    gaKTour30[[j]][[i]] <- ga(type = "binary"
                   , fitness = bprofit
                   , nBits = 50
                   , popSize = 50
                   , maxiter = 50)
  }
}

tours <- matrix(nrow = 50, ncol = 4)
for (j in 2:5) {
    tours[, j - 1] <- apply(
      sapply(1:30, function(x) { gaKTour30[[j]][[x]]@summary[, "mean"] }) 
      , 1, mean)
}

matplot(tours
        , type = "n"
        , xlab = "iterations"
        , ylab = "fitness / profit"
        , main = "Evolution of Fitness\nover different tournament size")
matlines(tours)
legend("bottomright", legend = 2:5, col = 1:4, lty = 1:4)