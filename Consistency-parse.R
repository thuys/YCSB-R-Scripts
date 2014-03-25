consistencyParse <- function(fileName){
  #READ FILE
  k <- readLines(fileName, warn = FALSE)
  splitted <- strsplit(x=k, split = ",")
  outputW <- list();
  outputR <- list();
  
  RThreads <- list();
  WThreads <- list();
  
  header <- splitted[[1]]
  
    
  for(lineNb in 2:length(k)){
    line = splitted[[lineNb]]
    timing <- line[1]
    thread <- line[2]
    start <- line [3]
    if(!is.na(suppressWarnings(as.numeric(timing)))){
      
      if(length(grep("W", thread)) > 0){
        WThreads[[thread]] <- TRUE
        if(is.null(outputW[[timing]])){
          outputW[[timing]] <- list()
        }
        
        if(is.null(outputW[[timing]][[thread]])){
          outputW[[timing]][[thread]] <- list()
        }
        
        lineList <- list()
        
        lineList[["start"]] <- suppressWarnings(as.numeric(line[3]))
        lineList[["delay"]] <- suppressWarnings(as.numeric(line[4]))
        lineList[["value"]] <- suppressWarnings(as.numeric(line[5]))
        
        outputW[[timing]][[thread]][[start]] <- lineList
      }else if(length(grep("R", thread)) > 0){
        RThreads[[thread]] <- TRUE
        if(is.null(outputR[[timing]])){
          outputR[[timing]] <- list()
        }
        
        if(is.null(outputR[[timing]][[thread]])){
          outputR[[timing]][[thread]] <- list()
        }
        
        lineList <- list()
        
        lineList[["start"]] <- suppressWarnings(as.numeric(line[3]))
        lineList[["delay"]] <- suppressWarnings(as.numeric(line[4]))
        lineList[["value"]] <- suppressWarnings(as.numeric(line[5]))
        
        outputR[[timing]][[thread]][[start]] <- lineList
      }
    }
  }
  return(list(writerThreads = WThreads, readerThreads = RThreads, outputW = outputW, outputR = outputR))
}

consistencyPostParse <- function(parsedReader){
  changesInData <- list()
  
  for(key in names(parsedReader)){
    parserKey <- parsedReader[[key]]
    changesInData[[key]] <- list()
    # zoek alle veranderingen per thread
    for(thread in names(parserKey)){
      threadList <- list()
      
      parserThread <- parserKey[[thread]]
      
      #Collect all changes
      isFirst <- TRUE
      change <- FALSE
      for(start in names(parserThread)){
        if(isFirst){
          isFirst <- FALSE
          threadList[[start]] <- parserThread[[start]]
          lastValue <- parserThread[[start]]
          change <- TRUE
        }
        else if(lastValue[["value"]] != parserThread[[start]][["value"]]){
          threadList[[start]] <- parserThread[[start]]
          lastValue <- parserThread[[start]]
          change <- TRUE
        }
        if(change){
          # Try to find last read of other thread 
          change <- FALSE
          minStart <- -1000000000
          for(innerthread in names(parserKey)){ 
            for(innerstart in names(parserThread)){
              if(innerstart < start && innerstart > minStart){
                minStart <- innerstart
                minThread <- innerthread
                minValue <- parserKey[[innerthread]][[innerstart]][["value"]]
              }
            }
          }
          if(minStart > -1000000000){
            threadList[[start]]["beforeThread"] <-minThread
            threadList[[start]]["beforeStart"] <-minStart
            threadList[[start]]["beforeValue"] <-minValue
          }
        }
      }
      changesInData[[key]][[thread]] <- threadList
    }
    ## 
  }
  return(changesInData)
}

consistencyPlotNb <- function(parsedReader, parsedWriter, readerNames, writerNames, maxOnSamePlot, exportDir){
  columnNames <- names(readerNames)
  rowNames <- names(parsedReader)
  amountOfChanges <-matrix(nrow=length(rowNames), ncol=length(columnNames), dimnames = list((as.numeric(rowNames)), columnNames))
  
  for(i in 1:nrow(amountOfChanges)){
    key <- rowNames[i]
    parserKey <- parsedReader[[key]]
    # zoek alle veranderingen per thread
    for(j in 1:ncol(amountOfChanges)){
      thread <- columnNames[j]      
      parserThread <- parserKey[[thread]]
      amountOfChanges[i,j] <-length(parserThread)
    }
  }
  
  # Find all similar columns
  similarMatrix <- matrix(nrow=ncol(amountOfChanges), ncol=ncol(amountOfChanges))
  
  for(outerThread in 1:ncol(amountOfChanges)){
    for(innerThread in outerThread:ncol(amountOfChanges)){
      if(areEqualAmount(outerThread, innerThread, amountOfChanges)){
        similarMatrix[outerThread, innerThread] = 1
        similarMatrix[innerThread, outerThread] = 1
      }
      else{
        similarMatrix[outerThread, innerThread] = 0
        similarMatrix[innerThread, outerThread] = 0
      }
    }
  }
  
  labelThreads <- list()
  labelList <- list()
  labelIndex <- list()
  index = 0
  
  for(thread in 1:ncol(amountOfChanges)){
    threadNumber = getUniqueThreadID(thread, similarMatrix)
    if(threadNumber == thread){
      index <- index + 1
      labelThreads[thread] <- index
      labelList[index] <- thread
      labelIndex[index] <- thread
    }
    else{
      labelList[labelThreads[[threadNumber]]] <- paste(labelList[labelThreads[[threadNumber]]], thread, sep = ", ")
    }
  }
  
  labelArray <- unlist(labelList)
  
  for(i in 1:ceiling(index/maxOnSamePlot)){
    startNumber <- ((i-1)*maxOnSamePlot+1)
    endNumber <- (i)*maxOnSamePlot
    png(filename=paste(exportDir, "/consistency-", i, ".png", sep=""), width=figureWidth, height=figureHeight, units="px")
    
    #plot.new()
    plot(x = 0, y = 0, type="n", xlab ="Key number)",ylab = "Amount of changes",
         ylim = c(0, max(amountOfChanges)), xlim = as.numeric(c(min((rowNames)), max((rowNames)))))
    
    for(plotNb in startNumber:endNumber){
      lines(x=rowNames, y = amountOfChanges[,labelIndex[[plotNb]]],type="l",col = plotNb, pch = plotNb)
    }
    legend("topleft", labelArray[startNumber:endNumber], col = startNumber:endNumber, pch = startNumber:endNumber)
    dev.off(); 
  }
  
  
  # Make barplot
  maxAmountOfChanges <- max(amountOfChanges)
  countPlot <- matrix(ncol=length(labelArray), nrow= (maxAmountOfChanges+1))
  
  for(indexCounter in 1:index){
      thread <- labelIndex[[indexCounter]] 
      threadTable <- table(amountOfChanges[,thread])
      countPlot[, indexCounter] <- tabulate(amountOfChanges[,thread]+1, nbins = maxAmountOfChanges+1)
  }
  
  #Change label
  labelListSplitted <- strsplit(labelArray, c(", "))
  labelListMerged <- rep(0,index)
  for(indexCounter in 1:index){
    for(i in 1:length(labelListSplitted[indexCounter][[1]])){
      if(labelListMerged[indexCounter] == 0){
        labelListMerged[indexCounter] <- labelListSplitted[indexCounter][[1]][i]
      }
      else{
        labelListMerged[indexCounter] <- paste(labelListMerged[indexCounter], labelListSplitted[indexCounter][[1]][i], sep=", ")
      }
      if(i%%4 == 0){
        labelListMerged[indexCounter] <- paste(labelListMerged[indexCounter], "\n", sep="")
      }
    }
  }

  
  png(filename=paste(exportDir, "/consistency-merged.png", sep=""), width=figureWidth, height=2*figureHeight, units="px")
  oldMar <- par()$mar
  newMar <- oldMar
  newMar[2] <- newMar[2] + 2
  par(mar=newMar)
  barplot(countPlot, main="Car Distribution by Gears and VS",
        xlab="Number of changes", col = c(1:3),
        legend = c(0:max(amountOfChanges)),
        names.arg=labelListMerged, 
        horiz=TRUE)
  dev.off(); 
  par(mar=oldMar)
}

areEqualAmount <- function(thread1, thread2, matrix){
  for(row in 1:nrow(matrix)){
    if(matrix[row, thread1] != matrix[row, thread2]){
      return (FALSE)
    }
  }
  return (TRUE)
}

getUniqueThreadID <- function(thread, matrix){
  if(thread == 1){
    return (1)
  }
  for(row in 1:thread){
    if(matrix[row, thread] == 1){
      return (row)
    }
  }
}
fileName ="D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-03-24/InsertRawData"
parsed <- consistencyParse(fileName)
postParsed <- consistencyPostParse(parsed$outputR)

consistencyPlotNb(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, 3, "D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-03-24/Consistency")
