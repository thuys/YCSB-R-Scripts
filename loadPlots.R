files <- list()
nbOfThreads <- list()
timeFrame <- 1000

files[[1]] <- c(
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-100-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-200-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-500-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-1000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-2000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-5000-2.dat"),
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-10000-2.dat")
)
nbOfThreads[[1]] <- c(100, 200, 500, 1000, 2000, 5000, 10000)

files[[2]] <- c(
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-10-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-20-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-50-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-100-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-200-2.dat")
)
nbOfThreads[[2]] <- c(10,20,50,100,200)

files[[3]] <- c(
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-100-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-200-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-500-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-1000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-2000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-5000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-10000-2.dat")
)
timeFrames[[3]] <- c(1000,1000, 1000, 1000, 1000, 1000, 1000)
nbOfThreads[[3]] <- c(100,200,500,1000,2000,5000,10000)
test <- tryCatch({
  plotLoadTesting(files, c("MongoDB", "PostgreSQL", "HBase"), nbOfThreads, 
                  timeFrame, c("UPDATE", "READ"), "D:/Schooljaar 2013-2014/Thesis/Results/")
}, error = function(e) print(paste("Problem in all graph", e)))