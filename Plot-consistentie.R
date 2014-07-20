source('./Consistency-parse.R')

dir <- "D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-07-17/mongodb-consistency/"
dirHbase <- "D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-07-14/hbase-consistency/"

writeOpps <- c("safe", "normal", "fsync_safe", "replicas_safe", "majority")
readOpps <- c("nearest", "primary", "primarypreferred", "secondary", "secondarypreferred")
readOpps <- c("nearest", "primary", "primarypreferred", "secondary")

#writeOpps <- c("none")
#readOpps <- c("nearest")
typeOpps <- c("insert", "update")
loops <- c("1", "2")
#loops <- c()
loopsHbase <- c("5", "6")

#writeOpps <- c()
dir.create(file.path(dir, "Fig/"), showWarnings = FALSE)
dir.create(file.path(dirHbase, "Fig/"), showWarnings = FALSE)

startECDF <- list()
endECDF <- list()
for(writeOpp in writeOpps){
  startECDF[[writeOpp]] <- list()
  endECDF[[writeOpp]] <- list()
  for(readOpp in readOpps){
    startECDF[[writeOpp]][[readOpp]] <- list()
    endECDF[[writeOpp]][[readOpp]] <- list()
    for(typeOpp in typeOpps){
      startECDF[[writeOpp]][[readOpp]][[typeOpp]] <- list()
      endECDF[[writeOpp]][[readOpp]][[typeOpp]] <- list()
      for(loop in loops){
        
        tryCatch({
          fileName <- paste(dir, typeOpp, "RawData-", writeOpp, "-", readOpp, "-", loop, ".dat", sep="")
          fileDir <- paste(dir, "Fig/%type%-", typeOpp, "RawData-", writeOpp, "-", readOpp, "-", loop, ".%extension%", sep="")
          parsed <- consistencyParse(fileName)
          postParsed <- consistencyPostParse(parsed$outputR)
          #consistencyPlotNb(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, 3, fileDir)
          returnValue <- consistencyPlotEachReader(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, fileDir, 2)
          startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]] <- returnValue[["startL"]]
          endECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]] <- returnValue[["stopL"]]
        }, error = function(e) print(paste("Problem in ", writeOpp, readOpp, typeOpp, loop, e)))
      }
    }
    
  }
}

fileDir <- paste(dir, "Fig/%type%.%extension%", sep="")
consistencyPlotsMongoForReads(startECDF, endECDF, writeOpps, readOpps, typeOpps, loops, parsed$readerThreads, parsed$writerThreads, fileDir)
consistencyPlotsMongoForWrites(startECDF, endECDF, writeOpps, readOpps, typeOpps, loops, parsed$readerThreads, parsed$writerThreads, fileDir)
plotWriteComparison(startECDF, endECDF, writeOpps, readOpps, typeOpps, loops, parsed$readerThreads, parsed$writerThreads, fileDir)

startHBaseECDF <- list()
endHBaseECDF <- list()
for(typeOpp in typeOpps){
  startHBaseECDF[[typeOpp]] <- list()
  endHBaseECDF[[typeOpp]] <- list()
  for(loop in loopsHbase){
    startHBaseECDF[[loop]] <- list()
    endHBaseECDF[[loop]] <- list()
    tryCatch({
      fileName <- paste(dirHbase, typeOpp, "RawData-", loop, ".dat", sep="")
      fileDir <- paste(dirHbase, "Fig/%type%-", typeOpp, "RawData-", loop, ".%extension%", sep="")
      parsed <- consistencyParse(fileName)
      postParsed <- consistencyPostParse(parsed$outputR)
      consistencyPlotNb(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, 3, fileDir)
      returnValue <-  consistencyPlotEachReader(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, fileDir, 2)
      startHBaseECDF[[typeOpp]][[loop]] <- returnValue[["startL"]]
      endHBaseECDF[[typeOpp]][[loop]] <- returnValue[["stopL"]]
    }, error = function(e) print(paste("Problem in ", typeOpp, loop, e)))
  }
}

