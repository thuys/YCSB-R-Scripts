files <- list()
nbOfRequests <- list()
timeFrame <- 1000

files[[1]] <- paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-%1-2.dat")
nbOfRequests[[1]] <- c(100, 200, 500, 1000, 2000, 5000, 10000)

files[[2]] <- paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-%1-2.dat")
nbOfRequests[[2]] <- c(10,20,50,100,200)

files[[3]] <- paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-%1-2.dat")
nbOfRequests[[3]] <- c(100,200,500,1000,2000,5000,10000)
test <- tryCatch({
  plotLoadTesting(files, c("MongoDB", "PostgreSQL", "HBase"), nbOfRequests, 
                  timeFrame, c("UPDATE", "READ"), "D:/Schooljaar 2013-2014/Thesis/Results/")
}, error = function(e) print(paste("Problem in all graph", e)))