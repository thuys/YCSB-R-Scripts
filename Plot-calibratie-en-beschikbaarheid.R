source('plot.R')
basicDir <- "D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-07-17/"
queryTypes <- c("UPDATE", "READ", "SCAN", "INSERT")
dir.create(file.path(basicDir, ""), showWarnings = FALSE)

timeFrame <- 1000
dir.create(file.path(basicDir, "postgresql/Fig/"), showWarnings = FALSE)
dir.create(file.path(basicDir, "hbase/Fig/"), showWarnings = FALSE)
dir.create(file.path(basicDir, "mongodb/Fig/"), showWarnings = FALSE)


## Threads plotten
workload <- list()

workload["READ"] <- 0.4
workload["UPDATE"] <- 0.2
workload["SCAN"] <- 0.2
workload["INSERT"] <- 0.2

threads <- c(1,2,3,4,5,7,10,15,20, 30, 40, 50, 75, 100)

tryCatch({
  plotThreadTesting(paste(basicDir, "postgresql/threads-%1-2.dat", sep = ""),
                    threads, workload, timeFrame, paste(basicDir, "threads-postgresql.png", sep=""))
}, error = function(e) print(paste("Problem in threads postgresql", e)))

tryCatch({
  plotThreadTesting(paste(basicDir, "hbase/threads-%1-2.dat", sep = ""),
                    threads, workload, timeFrame, paste(basicDir, "threads-hbase.png", sep=""))
}, error = function(e) print(paste("Problem in hbase postgresql", e)))

tryCatch({
  plotThreadTesting(paste(basicDir, "mongodb/threads-%1-2.dat", sep = ""),
                    threads, workload, timeFrame, paste(basicDir, "threads-mongodb.png", sep=""))
}, error = function(e) print(paste("Problem in threads mongodb", e)))

## 
files <- list()
nbOfRequests <- list()

files[[1]] <- paste(basicDir, "hbase/target-%1-2.dat", sep = "")
nbOfRequests[[1]] <- c(20, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000,2000, 2500, 3000)

files[[2]] <- paste(basicDir, "mongodb/target-%1-2.dat", sep = "")
nbOfRequests[[2]] <- c(20, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000,2000)

files[[3]] <- paste(basicDir, "postgresql/target-%1-2.dat", sep = "")
nbOfRequests[[3]] <- c(20, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000)

labelLoad <- c("HBase", "MongoDB", "PostgreSQL")
#labelLoad <- c("PostgreSQL", "MongoDB")
test <- tryCatch({
plotLoadTesting(files, labelLoad, nbOfRequests, 
                  timeFrame, queryTypes, basicDir)
}, error = function(e) print(paste("Problem in all graph", e)))

types <- c("", "-kill", "-drop")


for(run in c(1,2)){
  for(type in types){
    outputDir <- paste(basicDir, "postgresql/Fig/Continious-Run-", run, type, sep="")
    dir.create(file.path(outputDir), showWarnings = FALSE)
    tryCatch({
      plotWithInterrupts(paste(basicDir, "postgresql/continuous-vmpg%1.dat", sep = ""),
                         c(
                           paste("1",type, "-" ,run,sep=""), 
                           paste("2",type, "-" ,run,sep="")
                         ), 
                         c("Node 1", "Node 2"), 
                         1000, 
                         queryTypes, 
                         outputDir, 
                         c(300,600), 
                         c("All online", "Shut down", "One offline", "Power on", "All online")
      )
    },error = function(e) print(paste("Problem in postgresql continious run", run, type, e))
    )
  }
}

hbaseTypes <- c("hbase", "hadoop")
for(run in c(1,2)){
  for(hbaseType in hbaseTypes){
    dir.create(file.path(basicDir, hbaseType, "/Fig/"), showWarnings = FALSE)
    for(type in types){
      outputDir <- paste(basicDir, hbaseType, "/Fig/Continious-Run-", run, type,  sep="")
      dir.create(file.path(outputDir), showWarnings = FALSE)
      tryCatch({
        plotWithInterrupts(paste(basicDir, hbaseType, "/continuous-vmhb%1.dat", sep = ""),
                           c(
                             paste("2",type, "-" ,run,sep=""), 
                             paste("3",type, "-" ,run,sep=""),
                             paste("4",type, "-" ,run,sep=""),
                             paste("5",type, "-" ,run,sep="")
                           ), 
                           c("Node 2", "Node 3", "Node 4", "Node 5"), 
                           1000, 
                           queryTypes, 
                           outputDir, 
                           c(300,600),  
                           c("All online", "Shut down", "One offline", "Power on", "All online")
                           
        )
      },error = function(e) print(paste("Problem in ", hbaseType, " continious run", run, type, e))
      )
    }
  }
}

for(run in c(1,2)){
  for(type in types){
    outputDir <- paste(basicDir, "mongodb/Fig/Continious-Run-", run, type, sep="")
    dir.create(file.path(outputDir), showWarnings = FALSE)
    
    tryCatch({
      plotWithInterrupts(paste(basicDir, "mongodb/continuous-vmmdb%1.dat", sep = ""),
                         c(
                           paste("4",type, "-" ,run,sep=""), 
                           paste("5",type, "-" ,run,sep=""),
                           paste("6",type, "-" ,run,sep="")
                         ), 
                         c("Node 4", "Node 5", "Node 6"), 
                         1000, 
                         queryTypes, 
                         outputDir, 
                         c(300,600),  
                         c("All online", "Shut down", "One offline", "Power on", "All online")
                         
      )
    },error = function(e) print(paste("Problem in mongodb continious run", run, type, e))
    )
  }
}