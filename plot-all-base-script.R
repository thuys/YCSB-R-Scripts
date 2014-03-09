basicDir <- "D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-03-09/"
queryTypes <- c("UPDATE", "READ", "SCAN", "INSERT")
dir.create(file.path(outputDir, ""), showWarnings = FALSE)



files <- list()
nbOfRequests <- list()
timeFrame <- 1000

files[[1]] <- paste(basicDir, "mongodb/threads-%1-2.dat", sep = "")
nbOfRequests[[1]] <- c(100, 300, 500, 700, 900, 1000, 1250, 1500, 1750, 2000, 2500, 3000, 3500, 4000, 5000, 6000)

files[[2]] <- paste(basicDir, "postgresql/threads-%1-2.dat", sep = "")
nbOfRequests[[2]] <- c(20, 50, 100, 150, 200, 250, 300, 400, 500)


files[[3]] <- paste(basicDir, "hbase/threads-%1-2.dat", sep = "")
nbOfRequests[[3]] <- c(100, 300, 500, 700, 900, 1000, 1250, 1500, 1750, 2000, 2500, 3000, 3500, 4000, 5000, 6000)
test <- tryCatch({
  plotLoadTesting(files, c("MongoDB", "PostgreSQL", "HBase"), nbOfRequests, 
                  timeFrame, queryTypes, basicDir)
}, error = function(e) print(paste("Problem in all graph", e)))



for(run in c(1,2)){
  dir.create(file.path(basicDir, "postgresql/Fig/"), showWarnings = FALSE)
  outputDir <- paste(basicDir, "postgresql/Fig/Continious-Run-", run, sep="")
  dir.create(file.path(outputDir), showWarnings = FALSE)
  tryCatch({
    plotWithInterrupts(paste(basicDir, "postgresql/continuous-vmpg%1.dat", sep = ""),
                       c(
                         paste("1-",run,sep=""), 
                         paste("2-",run,sep="")
                       ), 
                       c("Node 1", "Node 2"), 
                       1000, 
                       queryTypes, 
                       outputDir, 
                       c(300), 
                       c("All online", "Shut down", "One offline", "Power on", "All online")
    )
  },error = function(e) print(paste("Problem in postgresql continious run", run, e))
  )
}


for(run in c(1,2)){
  dir.create(file.path(basicDir, "hbase/Fig/"), showWarnings = FALSE)
  outputDir <- paste(basicDir, "hbase/Fig/Continious-Run-", run, sep="")
  dir.create(file.path(outputDir), showWarnings = FALSE)
  tryCatch({
    plotWithInterrupts(paste(basicDir, "hbase/continuous-vmhb%1.dat", sep = ""),
                       c(
                         paste("2-",run,sep=""), 
                         paste("3-",run,sep=""),
                         paste("5-",run,sep="")
                       ), 
                       c("Node 2", "Node 3", "Node 5"), 
                       1000, 
                       queryTypes, 
                       outputDir, 
                       c(300,600),  
                       c("All online", "Shut down", "One offline", "Power on", "All online")
                       
    )
  },error = function(e) print(paste("Problem in hbase continious run", run, e))
  )
}

for(run in c(1,2)){
  dir.create(file.path(basicDir, "mongodb/Fig/"), showWarnings = FALSE)
  outputDir <- paste(basicDir, "mongodb/Fig/Continious-Run-", run, sep="")
  dir.create(file.path(outputDir), showWarnings = FALSE)
  
  tryCatch({
    plotWithInterrupts(paste(basicDir, "mongodb/continuous-vmmdb%1.dat", sep = ""),
                       c(
                         paste("1-",run,sep=""), 
                         paste("2-",run,sep=""),
                         paste("3-",run,sep=""),
                         paste("4-",run,sep=""),
                         paste("5-",run,sep="")
                       ), 
                       c("Node 1", "Node 2", "Node 3", "Node 4", "Node 5"), 
                       1000, 
                       queryTypes, 
                       outputDir, 
                       c(300,600),  
                       c("All online", "Shut down", "One offline", "Power on", "All online")
                       
    )
  },error = function(e) print(paste("Problem in mongodb continious run", run, e))
  )
}