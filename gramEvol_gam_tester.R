library(lattice)
library(gramEvol)
library(gam)
library(parallel)
library(doParallel)

computer.data <- read.csv("machine.data"
                          , header = FALSE
                          , stringsAsFactors = FALSE)

names(computer.data) <- c("VendorName"
                          , "ModelName"
                          , "MYCT"
                          , "MMIN"
                          , "MMAX"
                          , "CACH"
                          , "CHMIN"
                          , "CHMAX"
                          , "PRP"
                          , "ERP")
summary(computer.data)
computer.data.plotting <- computer.data[, -c(1, 2)]
computer.data.plotting.log.y <- cbind(computer.data.plotting[, 1:6]
                                      , log(computer.data.plotting[, 7:8]))
computer.data.plotting.log.xy <- cbind(log(computer.data.plotting[, c("MYCT"
                                                                      , "MMIN"
                                                                      , "MMAX")])
                                       , log(1 + computer.data.plotting[, c("CACH"
                                                                            , "CHMIN"
                                                                            , "CHMAX")])
                                       , log(computer.data.plotting[, 7:8])
)

splom(computer.data.plotting
      , pscales = 0)
splom(computer.data.plotting.log.y
      , pscales = 0)
splom(computer.data.plotting.log.xy
      , pscales = 0)

computer.data.log <- cbind(computer.data[, c(1, 2)]
                           , computer.data.plotting.log.xy)

set.seed(124)
samples <- nrow(computer.data)
train.index <- sample(samples
                      , trunc(0.8 * samples)
                      , replace = FALSE)

train.data <- computer.data[train.index, ]
test.data <- computer.data[-train.index, ]

train.data.log <- computer.data.log[train.index, ]
test.data.log <- computer.data.log[-train.index, ]

trn <- train.data.log
tst <- test.data.log

grammarDef <- CreateGrammar(list(
  learner = grule(function(trn) {
    result <- NULL
    fmla <- eval(features)
    if (length(attr(terms(fmla), "variables")) > 2) {
      capture.output({
        result <- gam(fmla, data = trn)
      })
    }
    return(result)
  })
  , features = grule(y ~ f1 + f2 + f3 + f4 + f5 + f6)
  , y = grule(PRP)
  , f1 = grule(MYCT
               , I(1/MYCT)
               , .(poly(MYCT, n))
               , .(s(MYCT, df = n))
               , 0)
  , f2 = grule(MMIN
               , I(1/MMIN)
               , .(poly(MMIN, n))
               , .(s(MMIN, df = n))
               , 0)
  , f3 = grule(MMAX
               , I(1/MMAX)
               , .(poly(MMAX, n))
               , .(s(MMAX, df = n))
               , 0)
  , f4 = grule(CACH
               #, I(1/(0.0001 + CACH))
               , .(poly(CACH, n))
               , .(s(CACH, df = n))
               , 0)
  , f5 = grule(CHMIN
               #, I(1/(0.0001 + CHMIN))
               , .(poly(CHMIN, n))
               , .(s(CHMIN, df = n))
               , 0)
  , f6 = grule(CHMAX
               #, I(1/(0.0001 + CHMAX))
               , .(poly(CHMAX, n))
               , .(s(CHMAX, df = n))
               , 0)
  , n = grule(2, 3, 4, 5)
))

getCost <- function(expr) {
  model <- eval(expr)(trn)
  if (is.null(model)) { return(Inf) }
  test.results <- predict(model, tst)
  cost <- mean((test.results - test.data$PRP)^2)
  return(cost)
}

# set up parallel processing
p_clus <- makeCluster(detectCores() - 1)
registerDoParallel(p_clus)

ge <- GrammaticalEvolution(grammarDef
                     , getCost
                     , popSize = 30
                     , iterations = 200
                     , newPerGen = 0.1
                     , mutationChance = 0.0005
                     , elitism = 3
                     , plapply = mclapply
                     , verbose = TRUE
                     )

# close parallel processing
stopCluster(p_clus)

ge
gam.func <- eval(ge$best$expressions)
fit <- gam.func(computer.data.log)
summary(fit)
