library(lattice)
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
