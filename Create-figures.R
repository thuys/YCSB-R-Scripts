for(node in 1:3){
  for(run in 1:3){
    tryCatch({
      plotAll(
        c(
          paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/all-",run,".dat", sep=""), 
          paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/one-down-",node, "-", run,".dat", sep="")
        ), 
        c("All Online", "One offline"), 
        c(1000, 1000), 
        c("UPDATE", "READ"), 
        paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/Fig/Node-", node, "/Run-", run, sep="")
      )
    },error = function(e) print(paste("Problem in mongoDB node", node, "run", run, e))
    )
  }
}



for(node in 1:2){
  for(run in 1:2){
    tryCatch({
      plotAll(
        c(
          paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/all-",run,".dat", sep=""), 
          paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/one-postgres-down-",node, "-", run,".dat", sep="")
        ), 
        c("All Online", "One postgres offline"), 
        c(1000, 1000), 
        c("UPDATE", "READ"), 
        paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/Fig/Node-", node, "/Run-", run, sep="")
      )
    },error = function(e) print(paste("Problem in Postgresql node", node, "run", run, e)))
  }
}

for(node in 2:3){
  for(run in 1:3){
    tryCatch({
    plotAll(
      c(
        paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/all-",run,".dat", sep=""), 
        paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/one-region-down-",(node), "-", run,".dat", sep="")
        #paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/one-master-down-",(node), "-", run,".dat", sep="")
      ), 
      c("All Online", "One region offline"),#, "One master offline"), 
      c(1000, 1000),#, 1000), 
      c("UPDATE", "READ"), 
      paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/Fig/Node-", (node), "/Run-", run, sep="")
    )},error = function(e) print(paste("Problem in HBase node", node, "run", run, e)))
  }
}

files <- list()
nbOfThreads <- list()
timeFrames <- list()

files[[1]] <- c(
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-100-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-200-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-500-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-1000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-2000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-5000-2.dat"),
  paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/threads-10000-2.dat")
)
timeFrames[[1]] <- c(1000,1000, 1000, 1000, 1000, 1000, 1000)
nbOfThreads[[1]] <- c(100, 200, 500, 1000, 2000, 5000, 10000)

files[[2]] <- c(
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-10-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-20-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-50-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-100-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/threads-200-2.dat")
)
timeFrames[[2]] <- c(1000,1000, 1000, 1000, 1000)
nbOfThreads[[2]] <- c(10,20,50,100,200)

files[[3]] <- c(
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-100-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-200-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-500-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-1000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-2000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-5000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-10000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-20000-2.dat"), 
  paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/threads-50000-2.dat")
)
timeFrames[[3]] <- c(1000,1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000)
nbOfThreads[[3]] <- c(100,200,500,1000,2000,5000,10000,20000,50000)
test <- tryCatch({
 plotLoadTesting(files, c("MongoDB", "PostgreSQL", "HBase"), nbOfThreads, 
                        timeFrames, c("UPDATE", "READ"), "D:/Schooljaar 2013-2014/Thesis/Results/")
}, error = function(e) print(paste("Problem in all graph", e)))