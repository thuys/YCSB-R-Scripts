files <- list()
nbOfRequests <- list()
timeFrame <- 1000

files[[1]] <- paste("D:/Schooljaar 2013-2014/Thesis/Results-2/mongodb/threads-%1-2.dat")
nbOfRequests[[1]] <- c(10, 30, 50, 100, 200, 500, 750, 1000, 1300, 1600, 2000, 3000, 4000, 5000, 6000, 7000)

files[[2]] <- paste("D:/Schooljaar 2013-2014/Thesis/Results-2/postgresql/threads-%1-2.dat")
nbOfRequests[[2]] <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200)

files[[3]] <- paste("D:/Schooljaar 2013-2014/Thesis/Results-2/hbase/threads-%1-2.dat")
nbOfRequests[[3]] <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1250, 1500, 1750, 2000, 3000)
test <- tryCatch({
  plotLoadTesting(files, c("MongoDB", "PostgreSQL", "HBase"), nbOfRequests, 
                  timeFrame, c("UPDATE", "READ"), "D:/Schooljaar 2013-2014/Thesis/Results-2/")
}, error = function(e) print(paste("Problem in all graph", e)))