source('./plot.R')
library('xtable')
assign("consistencyEcdf", TRUE, envir = .GlobalEnv)
assign("consistencyReaders", TRUE, envir = .GlobalEnv)
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
      tryCatch({
      for(start in names(parserThread)){
        if(isFirst){
          isFirst <- FALSE
          threadList[[start]] <- parserThread[[start]]
          lastValue <- parserThread[[start]]
          change <- TRUE
        }
        else if(areDifferentNumbersWithNa(lastValue[["value"]], parserThread[[start]][["value"]])){
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
      },error = function(e) print(paste("Problem in parse", thread, start, lastValue[["value"]] , parserThread[[start]][["value"]] )))
      changesInData[[key]][[thread]] <- threadList
    }
    ## 
  }
  return(changesInData)
}

areDifferentNumbersWithNa <- function(number1, number2){
  if(is.na(number1)){
    return(!is.na(number2))
  }else if(is.na(number2)){
    return(TRUE)
  }
  return (number1 != number2)
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
      labelList[index] <- paste((thread-1), "", sep="")
      labelIndex[index] <- thread
    }
    else{
      labelList[labelThreads[[threadNumber]]] <- paste(labelList[labelThreads[[threadNumber]]], (thread-1), sep = ", ")
    }
  }
  
  labelArray <- unlist(labelList)
  
  for(i in 1:ceiling(index/maxOnSamePlot)){
    startNumber <- ((i-1)*maxOnSamePlot+1)
    endNumber <- min((i)*maxOnSamePlot, index)
    fileNameSub <- gsub("%number%", i, gsub("%type%", "consistency-%number%", gsub("%extension%", "png", exportDir)))
    png(filename=fileNameSub, width=figureWidth, height=figureHeight, units="px", res=figureRes)
    
    #plot.new()
    plot(x = 0, y = 0, type="n", xlab ="Key number)",ylab = "Amount of changes",
         ylim = c(0, max(amountOfChanges)), xlim = as.numeric(c(min((rowNames)), max((rowNames)))))
    
    for(plotNb in startNumber:endNumber){
      lines(x=rowNames, y = amountOfChanges[,labelIndex[[plotNb]]],type="l",col = plotNb, pch = plotNb)
    }
    legend("topleft", legend=labelArray[startNumber:endNumber], col = startNumber:endNumber, pch = startNumber:endNumber)
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
        labelListMerged[indexCounter] <- paste(labelListMerged[indexCounter], labelListSplitted[indexCounter][[1]][i], sep=" ")
      }
      if(i%%4 == 0){
        labelListMerged[indexCounter] <- paste(labelListMerged[indexCounter], "\n", sep="")
      }
    }
  }

  fileNameSub <- gsub("%type%", "consistency-merged", gsub("%extension%", "png", exportDir))
  
  png(filename=fileNameSub, width=figureWidth, height=2*figureHeight, units="px", res=figureRes)
  oldMar <- par()$mar
  newMar <- oldMar
  newMar[2] <- newMar[2] + 2
  par(mar=newMar)
  barplot(countPlot, main="Consistency together",
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

consistencyPlotEachReader <- function(parsedReader, parsedWriter, readerNames, writerNames, exportDir, maxRetries){
  numberOfWriters <- length(writerNames)
  plotMatrix <- matrix(nrow = length(parsedWriter), ncol = (2*length(writerNames)+2*maxRetries*length(readerNames)))
  
  densityMatrix <- matrix(nrow = length(parsedWriter), ncol = (2*length(writerNames)+2*length(readerNames)))
  densityMatrixAll <- matrix(nrow = (length(parsedWriter)*length(readerNames)), ncol = (2))
  
  rownames(plotMatrix) <- names(parsedWriter)
  rownames(densityMatrix) <- names(parsedWriter)
  
  colNamesPlot <- rep("", ncol(plotMatrix))
  densityMatrixNamesPlot<- rep("", ncol(densityMatrix))
  
  for(j in 1:length(writerNames)){
    threadName <- names(writerNames[j])[1]
    colNamesPlot[(2*j-1)] <- paste(threadName, "-START", sep="")
    colNamesPlot[(2*j)] <- paste(threadName, "-DELAY", sep="")
    
    densityMatrixNamesPlot[(2*j-1)] <- paste(threadName, "-START", sep="")
    densityMatrixNamesPlot[(2*j)] <- paste(threadName, "-DELAY", sep="")
    
  }
  
  for(j in 1:length(readerNames)){
    threadName <- names(readerNames[j])[1]
    for(k in 1:maxRetries){
      colNamesPlot[(2*numberOfWriters+2*maxRetries*(j-1)+2*k-1)] <- paste(threadName, "-", k, "-START", sep="")
      colNamesPlot[(2*numberOfWriters+2*maxRetries*(j-1)+2*k)] <- paste(threadName, "-", k, "-DELAY", sep="")
    }
    
    densityMatrixNamesPlot[(2*numberOfWriters+2*j-1)] <- paste(threadName, "-START", sep="")
    densityMatrixNamesPlot[(2*numberOfWriters+2*j)] <- paste(threadName, "-DELAY", sep="")
  }
  colnames(plotMatrix) <- colNamesPlot                     
  colnames(densityMatrix) <- densityMatrixNamesPlot   
  
  colnames(densityMatrixAll) <- c("START", "DELAY")
  
  #writer data gathering (delay)
  for(i in 1:length(parsedWriter)){
    parserKey <- parsedWriter[i]
    parserKey <- parserKey[[names(parserKey)[1]]]
    # zoek alle veranderingen per thread
    for(j in 1:length(writerNames)){
      parserThread <- parserKey[j]
      parserThread <- parserThread[[names(parserThread)[1]]]
      firstEntry <- parserThread[[names(parserThread)[1]]]
      
      plotMatrix[i,(2*j-1)] <- firstEntry[["start"]]/1000
      plotMatrix[i,(2*j)] <- firstEntry[["delay"]]/1000
      
      densityMatrix[i,(2*j-1)] <- firstEntry[["start"]]/1000
      densityMatrix[i,(2*j)] <- firstEntry[["delay"]]/1000
    }
  }
  
  for(i in 1:length(parsedReader)){
    parserKey <- parsedReader[i]
    parserKey <- parserKey[[names(parserKey)[1]]]
    # zoek alle veranderingen per thread
    for(j in 1:length(readerNames)){
      parserThread <- parserKey[j]
      parserThread <- parserThread[[names(parserThread)[1]]]
      allValue <- names(parserThread)
      startIndex <- 1
      for(k in max(length(allValue)-maxRetries+1, 1):length(allValue)){
        firstEntry <- parserThread[[allValue[k]]]
        
        if(length(firstEntry) >0){
            plotMatrix[i,(2*numberOfWriters+2*maxRetries*(j-1)+2*startIndex-1)] <- firstEntry[["start"]]/1000
            plotMatrix[i,(2*numberOfWriters+2*maxRetries*(j-1)+2*startIndex)] <- firstEntry[["delay"]]/1000
            
            densityMatrix[i,(2*numberOfWriters+2*j-1)] <- firstEntry[["start"]]/1000
            densityMatrix[i,(2*numberOfWriters+2*j)] <- firstEntry[["delay"]]/1000
            densityMatrixAll[(j-1)* length(parsedReader) + i, 1] <- firstEntry[["start"]]/1000
            densityMatrixAll[(j-1)* length(parsedReader) + i, 2]<- firstEntry[["delay"]]/1000
        }
        startIndex <- startIndex + 1
      }
      
    }
  }
  if(consistencyReaders){
    rowNames <- names(parsedWriter)
    labelPlot <- list()
    labelPlot[1] <- "Schrijver start"
    labelPlot[2] <- "Schrijver stop"
    for(k in 1:maxRetries){
      labelPlot[(2*k + 1)] <- paste("Lezer Poging ", k, " start", sep = "")
      labelPlot[(2*k + 2)] <- paste("Lezer Poging ", k, " stop", sep = "")
    }
    labelPlot <- unlist(labelPlot)
    
    for(j in 1:length(readerNames)){
      startIndex <- (2*numberOfWriters+2*maxRetries*(j-1)+1)
      endIndex <- (2*numberOfWriters+2*maxRetries*(j-1)+2*maxRetries)
      maxY = max(max(plotMatrix[,1:2],na.rm = TRUE), 
                 max(plotMatrix[,startIndex:endIndex], na.rm = TRUE) ,na.rm = TRUE)
      fileNameSub <- gsub("%type%", paste("consistency-plot-", names(readerNames)[j], sep=""), gsub("%extension%", "png", exportDir))
      png(filename=fileNameSub, width=figureWidth, height=figureHeight, units="px", res=figureRes)
      plot(x = 0, y = 0, type="n", xlab ="Sleutelnummer",ylab = "Tijd (ms)",
           ylim = c(0, 80), xlim = as.numeric(c(as.numeric(min(rowNames))+72000000, as.numeric(min(rowNames))+150000000)))
      
      #Plot writers
      lines(x=rowNames, y = plotMatrix[,1],type="l",col = 1, pch = 1)
      lines(x=rowNames, y = plotMatrix[,2],type="l",col = 2, pch = 2)
      
      for(k in 1:maxRetries){
        if(k == 1){
          plotType <- "l"
        }else{
          plotType <- "b"
        }
        lines(x=rowNames, y = plotMatrix[,startIndex + 2*(k-1)],type=plotType,col = (2*k+1), pch = (2*k+1))
        lines(x=rowNames, y = plotMatrix[,startIndex + 2*(k-1)+1],type=plotType,col = (2*k+2), pch = (2*k+2))
      }
      legend("topright", labelPlot, col = 1:(2*maxRetries+2), pch = 1:(2*maxRetries+2))
      
      dev.off(); 
      
    }
  }
  ## ECDF
  if(consistencyEcdf){
    ecdfStartList <- list()
    ecdfStopList <- list()
    startWQuery <- ecdf(densityMatrix[,1])
    stopWQuery <- ecdf(densityMatrix[,2])
    
    ecdfStartList[["w"]] <- startWQuery
    ecdfStopList[["w"]] <- stopWQuery
    
    minLezen <- quantile(startWQuery,0.01)
    maxLezen <- quantile(stopWQuery,0.90)
    for(j in 1:length(readerNames)){
      fileNameSub <- gsub("%type%", paste("ECDF-plot-", names(readerNames)[j], sep=""), gsub("%extension%", "png", exportDir))
      png(filename=fileNameSub, width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
   
      
      startQuery <- ecdf(densityMatrix[,2*numberOfWriters+2*j-1])
      stopQuery <- ecdf(densityMatrix[,2*numberOfWriters+2*j])
      plot(startWQuery, xlim=c(quantile(startQuery,0.01), quantile(stopQuery, 0.99)), ylim = c(0,1), col = 1, pch = 1, 
           do.p = FALSE, xlab =tijd,ylab = cumKans, main = NULL)
      lines(stopWQuery, col = 2, pch = 2, do.p = FALSE)
      lines(startQuery, col = 3, pch = 3, do.p = FALSE)
      lines(stopQuery, col = 4, pch = 4, do.p = FALSE)
      
      legend("bottomright", ecdfLegend, col = 1:4, pch=1:4)
      dev.off(); 
      
      minLezen = min(quantile(startQuery,0.01), minLezen, na.rm = TRUE)
      maxLezen = max(quantile(stopQuery,0.99), maxLezen, na.rm = TRUE)
      
      ecdfStartList[[toString(j)]] <- startQuery
      ecdfStopList[[toString(j)]] <- stopQuery
      
    }
    
    ## ECDF Start
    labelPlot <- list()
    labelPlot[1] <- "Schrijver"
    fileNameSub <- gsub("%type%", paste("ECDF-plot-Start", sep=""), gsub("%extension%", "png", exportDir))
    png(filename=fileNameSub, width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
    plot(startWQuery, xlim=c(minLezen, maxLezen), ylim = c(0,1), col = 1, pch = 1,  
         do.p = FALSE, xlab =tijd,ylab = cumKans, main = NULL)
    for(j in 1:length(readerNames)){
      lines(ecdf(densityMatrix[,2*numberOfWriters+2*j-1]), col = (j+1), pch = (j+1), do.p = FALSE)
      labelPlot[(1+j)] <- paste("Lezer ", j, sep = "")
    }
    labelPlot <- unlist(labelPlot)
    legend("bottomright", labelPlot, col = 1:(length(readerNames)+1), pch = 1:(length(readerNames)+1))
    
    dev.off(); 
    
    
    ## ECDF Stop
    labelPlot <- list()
    labelPlot[1] <- "Schrijver"
    fileNameSub <- gsub("%type%", paste("ECDF-plot-Stop", sep=""), gsub("%extension%", "png", exportDir))
    png(filename=fileNameSub, width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
    plot(stopWQuery, xlim=c(minLezen, maxLezen), ylim = c(0,1), col = 1, pch = 1,  
         do.p = FALSE, xlab =tijd,ylab = cumKans, main = NULL)
    for(j in 1:length(readerNames)){
      lines(ecdf(densityMatrix[,2*numberOfWriters+2*j]), col = (j+1), pch = (j+1), do.p = FALSE)
      labelPlot[(1+j)] <- paste("Lezer ", j, sep = "")
    }
    labelPlot <- unlist(labelPlot)
    legend("bottomright", labelPlot, col = 1:(length(readerNames)+1), pch = 1:(length(readerNames)+1))
    
    dev.off(); 
    
    
    ## ECDF ALL
    
    fileNameSub <- gsub("%type%", paste("ECDF-plot-all", sep=""), gsub("%extension%", "png", exportDir))
    png(filename=fileNameSub, width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
    
    
    startQuery <- ecdf(densityMatrixAll[,1])
    stopQuery <- ecdf(densityMatrixAll[,2])
    
    ecdfStartList[["all"]] <- startQuery
    ecdfStopList[["all"]] <- stopQuery
    
    plot(startWQuery, xlim=c(quantile(startQuery,0.01), quantile(stopQuery, 0.99)), ylim = c(0,1), col = 1, pch = 1, 
         do.p = FALSE, xlab = tijd,ylab = cumKans, main = NULL)
    lines(stopWQuery, col = 2, pch = 2, do.p = FALSE)
    lines(startQuery, col = 3, pch = 3, do.p = FALSE)
    lines(stopQuery, col = 4, pch = 4, do.p = FALSE)
    
    legend("bottomright", ecdfLegend, col = 1:4, pch=1:4)
    dev.off(); 
    
    temp <- list(startL = ecdfStartList, stopL = ecdfStopList)
    return (list(startL = ecdfStartList, stopL = ecdfStopList))
  }
}
consistencyPlotsMongoForWrites <- function(startECDF, stopECDF, writeOpps, readOpps, typeOpps, loops, readerThreads, writerThreads, exportDir){
  for(loop in loops){
    for(typeOpp in typeOpps){
      for(readOpp in readOpps){
        for(thread in 1:length(readerThreads)){
          xMinimum = NA
          xMaximum = NA
          for(writeOpp in writeOpps){
            xMinimum <- min(xMinimum, quantile(startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(x=thread)]],0.01), na.rm = TRUE)
            xMaximum <- max(xMaximum, quantile(stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(x=thread)]],0.97), na.rm = TRUE)
            
          }
          fileNameSub <- gsub("%type%", paste("ECDF-Write-", typeOpp, "-", readOpp, "-", thread, "-", loop, sep=""), gsub("%extension%", "png", exportDir))
          png(filename=fileNameSub, width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
          
          plot(c(0), xlim=c(xMinimum, xMaximum), ylim = c(0,1), col = 1, pch = 1, type = "n",
               xlab =tijd,ylab = cumKans, main = NULL)
               
          index <- 0
          for(writeOpp in writeOpps){
            index <- index + 1
            lines(startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(thread)]], col = index, pch = index, do.p = FALSE)
            lines(stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(thread)]], col = index, pch = index, do.p = FALSE)
           
          }
          
          legend("bottomright", writeOpps, col = 1:index, pch=1:index)
          dev.off(); 
        }
        thread = "all"
        xMinimum = NA
        xMaximum = NA
        for(writeOpp in writeOpps){
          xMinimum <- min(xMinimum, quantile(startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(thread)]],0.01), na.rm = TRUE)
          xMaximum <- max(xMaximum, quantile(stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(thread)]],0.97), na.rm = TRUE)
          
        }
        fileNameSub <- gsub("%type%", paste("ECDF-Write-", typeOpp, "-", readOpp, "-", thread, "-", loop, sep=""), gsub("%extension%", "png", exportDir))
        png(filename=fileNameSub, width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
        
        plot(c(0), xlim=c(xMinimum, xMaximum), ylim = c(0,1), col = 1, pch = 1, type = "n",
             xlab =tijd,ylab = cumKans, main = NULL)
        
        index <- 0
        for(writeOpp in writeOpps){
          index <- index + 1
          lines(startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(x=thread)]], col = index, pch = index, do.p = FALSE)
          lines(stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(x=thread)]], col = index, pch = index, do.p = FALSE)
          
        }
        
        legend("bottomright", writeOpps, col = 1:index, pch=1:index)
        dev.off(); 
        
      }
    }
    
  }
}

consistencyPlotsMongoForReads <- function(startECDF, stopECDF, writeOpps, readOpps, typeOpps, loops, readerThreads, writerThreads, exportDir){
  readLabel <- c("Schrijver start", "Schrijver stop", readOpps)
  for(loop in loops){
    for(typeOpp in typeOpps){
      for(writeOpp in writeOpps){
        for(thread in 1:length(readerThreads)){
          fileNameSub <- gsub("%type%", paste("ECDF-Reads-", typeOpp, "-", writeOpp, "-", thread, "-", loop, sep=""), gsub("%extension%", "png", exportDir))
          
          writeStart <- startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][["w"]]
          writeStop <- stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][["w"]]
          
          xMinimum = quantile(writeStart,0.01)
          xMaximum = quantile(writeStop,0.90)          
          
          for(readOpp in readOpps){
            xMinimum <- min(xMinimum, quantile(startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(thread)]],0.01), na.rm = TRUE)
            xMaximum <- max(xMaximum, quantile(stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(thread)]],0.97), na.rm = TRUE)
            
          }

          png(filename=fileNameSub, width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
          
          plot(c(0), xlim=c(xMinimum, xMaximum), ylim = c(0,1), col = 1, pch = 1, type = "n",
               xlab =tijd,ylab = cumKans, main = NULL)
          
          lines(writeStart, col = 1, pch = 1, do.p = FALSE)
          lines(writeStop, col = 2, pch = 2, do.p = FALSE)
          
          index <- 2
          for(readOpp in readOpps){
            index <- index + 1
            lines(startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(x=thread)]], col = index, pch = index, do.p = FALSE)
            lines(stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(x=thread)]], col = index, pch = index, do.p = FALSE)
            
          }
          
          legend("bottomright", readLabel, col = 1:index, pch=1:index)
          dev.off(); 
        }
        
        ## ALL
        thread = "all"
        
        writeStart <- startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][["w"]]
        writeStop <- stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][["w"]]
        
        xMinimum = quantile(writeStart,0.01)
        xMaximum = quantile(writeStop,0.90)       
        for(readOpp in readOpps){
          xMinimum <- min(xMinimum, quantile(startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(thread)]],0.01), na.rm = TRUE)
          xMaximum <- max(xMaximum, quantile(stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(thread)]],0.97), na.rm = TRUE)
          
        }
        fileNameSub <- gsub("%type%", paste("ECDF-Reads-", typeOpp, "-", writeOpp, "-", thread, "-", loop, sep=""), gsub("%extension%", "png", exportDir))
        png(filename=fileNameSub, width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
        
        plot(c(0), xlim=c(xMinimum, xMaximum), ylim = c(0,1), col = 1, pch = 1, type = "n",
             xlab =tijd ,ylab = cumKans, main = NULL)
        
        lines(writeStart, col = 1, pch = 1, do.p = FALSE)
        lines(writeStop, col = 2, pch = 2, do.p = FALSE)
        
        index <- 2
        for(readOpp in readOpps){
          index <- index + 1
          lines(startECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(x=thread)]], col = index, pch = index, do.p = FALSE)
          lines(stopECDF[[writeOpp]][[readOpp]][[typeOpp]][[loop]][[toString(x=thread)]], col = index, pch = index, do.p = FALSE)
          
        }
        
        legend("bottomright", readLabel, col = 1:index, pch=1:index)
        dev.off(); 
        
      }
    }
    
  }
}


plotWriteComparison <- function(startECDF, stopECDF, writeOpps, readOpps, typeOpps, loops, readerThreads, writerThreads, exportDir){
  readLabel <- c("Schrijver start", "Schrijver stop", readOpps)
  for(loop in loops){
    for(typeOpp in typeOpps){
      fileNameSub <- gsub("%type%", paste("ECDF-Compare-Write-", typeOpp, "-", loop, sep=""), gsub("%extension%", "png", exportDir))
      png(filename=fileNameSub, width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
      xMinimum = NA
      xMaximum = NA    
      for(writeOpp in writeOpps){
        writeStart <- startECDF[[writeOpp]][[readOpps[[1]]]][[typeOpp]][[loop]][["w"]]
        writeStop <- stopECDF[[writeOpp]][[readOpps[[1]]]][[typeOpp]][[loop]][["w"]]
        
        xMinimum = min(xMinimum, quantile(writeStart,0.01), na.rm = TRUE)
        xMaximum = max(xMaximum, quantile(writeStop,0.90), na.rm = TRUE) 
      }
      
      plot(c(0), xlim=c(xMinimum, xMaximum), ylim = c(0,1), col = 1, pch = 1, type = "n",
           xlab =tijd,ylab = cumKans, main = NULL)
      
      index <- 0
      for(writeOpp in writeOpps){
        index <- index + 1
        writeStart <- startECDF[[writeOpp]][[readOpps[[1]]]][[typeOpp]][[loop]][["w"]]
        writeStop <- stopECDF[[writeOpp]][[readOpps[[1]]]][[typeOpp]][[loop]][["w"]]
        lines(writeStart, col = index, pch = index, do.p = FALSE)
        lines(writeStop, col = index, pch = index, do.p = FALSE)
                                            
      }
      legend("bottomright", writeOpps, col = 1:index, pch=1:index)
      dev.off(); 
    }
    
  }
}

getPercentageDone <- function(dataset, readerThreads, startpunt, afstand){
  resultSet <- 1:length(readerThreads)
  for(i in 1:length(readerThreads)){
    resultSet[i] <- dataset[[toString(i)]](startpunt+afstand*i)
  }
  return (resultSet)
}

getPercentageDoneMongoDB <- function(dataset, loop, typeOpp, writeOpps, readOpps, readerThreads, startpunt, afstand, datapunten){
  resultSet <- matrix(nrow = length(writeOpps), ncol =length(readOpps))
  rownames(resultSet) <- writeOpps
  colnames(resultSet) <-readOpps
  
  i <- 0
  for(readOpp in readOpps){
    i <- i + 1
    j <- 0
    for(writeOpp in writeOpps){
      j <- j + 1
      tryCatch({
      output <- getPercentageDone(dataset[[writeOpp]][[readOpp]][[typeOpp]][[toString(loop)]], readerThreads[1:datapunten], startpunt, afstand)
      resultSet[j, i] <- toString(round(output, digits=2)*100)
      },error = function(e) TRUE)
    }
  }
  return(resultSet)
}

getLatexTableForMongoDB <- function(dataset, loop, typeOpp, writeOpps, readOpps, readerThreads, startpunt, afstand, datapunten){
  return(xtable(getPercentageDoneMongoDB(dataset, loop, typeOpp, writeOpps, readOpps, readerThreads, startpunt, afstand, datapunten)))
}
#fileDir <- paste(dir, "Fig/%type%.%extension%", sep="")
#consistencyPlotsMongoForReads(startECDF, endECDF, writeOpps, readOpps, typeOpps, loops, 1:5, 1, fileDir)
#consistencyPlotsMongoForWrites(startECDF, endECDF, writeOpps, readOpps, typeOpps, loops, 1:5, 1, fileDir)
if(debugmodus){
fileName ="D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-03-24/InsertRawData"
parsed <- consistencyParse(fileName)
postParsed <- consistencyPostParse(parsed$outputR)

consistencyPlotNb(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, 3, "D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-03-24/Consistency/%type%.%extension%")
consistencyPlotEachReader(postParsed, parsed$outputW, parsed$readerThreads, parsed$writerThreads, "D:/Schooljaar 2013-2014/Thesis/Result-Folder/2014-03-24/Consistency/%type%.%extension%", 2)
}