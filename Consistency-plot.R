dir <- "D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-03-30/mongodb-consistency/"
dirHbase <- "D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-03-30/hbase-consistency/"

writeOpps <- c("none", "safe", "normal", "fsync_safe", "replicas_safe", "majority")
readOpps <- c("nearest", "primary", "primarypreferred", "secondary", "secondarypreferred")
typeOpps <- c("insert", "update")
loops <- c("1", "2")
loopsHbase <- c("1", "2")

writeOpps <- c()
dir.create(file.path(dir, "Fig/"), showWarnings = FALSE)
dir.create(file.path(dirHbase, "Fig/"), showWarnings = FALSE)


for(writeOpp in writeOpps){
  for(readOpp in readOpps){
    for(typeOpp in typeOpps){
      for(loop in loops){
        tryCatch({
          fileName <- paste(dir, typeOpp, "RawData-", writeOpp, "-", readOpp, "-", loop, ".dat", sep="")
          fileDir <- paste(dir, "Fig/%type%-", typeOpp, "RawData-", writeOpp, "-", readOpp, "-", loop, ".%extension%", sep="")
          parsed <- consistencyParse(fileName)
          postParsed <- consistencyPostParse(parsed$outputR)
          consistencyPlotNb(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, 3, fileDir)
          consistencyPlotEachReader(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, fileDir, 2)
        }, error = function(e) print(paste("Problem in ", writeOpp, readOpp, typeOpp, loop, e)))
      }
    }
    
  }
}

for(typeOpp in typeOpps){
  for(loop in loopsHbase){
    tryCatch({
      fileName <- paste(dirHbase, typeOpp, "RawData-", loop, ".dat", sep="")
      fileDir <- paste(dirHbase, "Fig/%type%-", typeOpp, "RawData-", loop, ".%extension%", sep="")
      parsed <- consistencyParse(fileName)
      postParsed <- consistencyPostParse(parsed$outputR)
      consistencyPlotNb(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, 3, fileDir)
      consistencyPlotEachReader(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, fileDir, 2)
    }, error = function(e) print(paste("Problem in ", typeOpp, loop, e)))
  }
}

