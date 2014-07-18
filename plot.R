assign("safetyMarginBeforeInterruptBefore", 30, envir = .GlobalEnv)
assign("safetyMarginBeforeInterruptAfter", 30, envir = .GlobalEnv)

assign("marginBeforeInterrupt", 30, envir = .GlobalEnv)
assign("marginAfterInterrupt", 60, envir = .GlobalEnv)

source('parse.R')
plotWithInterrupts = function(fileName, fileNames, plotNames, timeFrame, labels, plotDir, interruptPoints, interruptLabels){
  data <- list();
  rawData <- list();
  for(i in 1:length(fileNames)){
    returnValue = parseInput(gsub("%1", fileNames[i], fileName), timeFrame)
    data[[i]] <- returnValue
    rawData[[i]] <- returnValue$raw
    
    minY = 0
    #minY = min(rawData[[i]][, labels], na.rm = TRUE)
    maxY = max(movingAverage(rawData[[i]][(sToRemove*timeFrame/1000):(nrow(rawData[[i]])-sToRemove), labels], movingAverageFrame), na.rm = TRUE)
    
    png(filename=paste(plotDir, "/single-graph-",fileNames[i], ".png", sep=""), width=figureWidth, height=figureHeight, units="px", res=figureRes)
    plotSingleData(rawData[[i]][1:(nrow(rawData[[i]])-safetyMarginBeforeInterruptBefore),], labels, 
                   paste("Plot of", plotNames[i])
                   , sToRemove, data[[i]]$runTime/1000, minY, 1.5*maxY)
    for(interruptPoint in interruptPoints){
      abline(v = interruptPoint, col = "blue")
    }
    dev.off(); 
    
    interruptedData <- list()
    interruptedPlotNames <- c();
    previousRecord <- 1
    minX = 1
    maxX = NA
    
    for(interruptIndex in 1:length(interruptPoints)){
      previousRecordWithBeforeMargin <- previousRecord + safetyMarginBeforeInterruptAfter
      nextRecord <- interruptPoints[interruptIndex]
      nextRecordWithAfterMargin <- nextRecord - safetyMarginBeforeInterruptBefore
      
      interruptedData[[interruptIndex]] <- rawData[[i]][previousRecordWithBeforeMargin:nextRecordWithAfterMargin,]
      nbOfElements <- nextRecordWithAfterMargin-previousRecordWithBeforeMargin
      maxX = max(nbOfElements, maxX, na.rm = TRUE)
      interruptedPlotNames[interruptIndex] <- paste(interruptLabels[1+2*(interruptIndex-1)], "(", previousRecordWithBeforeMargin, "->", nextRecordWithAfterMargin, ")")
      rownames(interruptedData[[interruptIndex]]) <- seq(0, nbOfElements*timeFrame/1000, timeFrame/1000)
      
      previousRecord <-nextRecord
      
      
    }
    
    previousRecordWithBeforeMargin <- previousRecord + safetyMarginBeforeInterruptAfter
    nextRecord <- nrow(rawData[[i]])
    nextRecordWithAfterMargin <- nextRecord - safetyMarginBeforeInterruptBefore
    
    interruptedData[[interruptIndex+1]] <- rawData[[i]][previousRecord:nextRecordWithAfterMargin,]
    nbOfElements <- nextRecordWithAfterMargin-previousRecord
    maxX = max(nbOfElements, maxX, na.rm = TRUE)
    interruptedPlotNames[interruptIndex+1] <- paste(interruptLabels[1+2*(interruptIndex)], "(", previousRecordWithBeforeMargin, "->", nextRecordWithAfterMargin, ")")
    rownames(interruptedData[[interruptIndex+1]]) <- seq(0, nbOfElements*timeFrame/1000, timeFrame/1000)
    
    for(label in labels){
      minY = 0
      maxY = NA

      for(maxIndex in 1:length(interruptedData)){
        maxY = max(maxY, max(movingAverage(interruptedData[[maxIndex]][(sToRemove*timeFrame/1000):nrow(interruptedData[[maxIndex]]), label], movingAverageFrame), na.rm = TRUE), na.rm = TRUE)
      }
      png(filename=paste(plotDir, "/multiple-graph-", plotNames[i], "-" ,label, ".png", sep=""), width=figureWidth, height=figureHeight, units="px", res=figureRes)
      plotMultipleDataSingleLabel(interruptedData, label, interruptedPlotNames, 
                                  paste("Plot van", label, "for", plotNames[i], "tussen event momenten.")
                                  , sToRemove, maxX, minY, 1.5*maxY)
      
      dev.off(); 
    }
  
  
    
    ## Now around an interrupt point
    interruptedData <- list()
    
    for(interruptIndex in 1:length(interruptPoints)){
      record <- interruptPoints[interruptIndex]
      startIndex <- record - marginBeforeInterrupt
      
      endIndex <- record + marginAfterInterrupt 
      
      interruptedData[[interruptIndex]] <- rawData[[i]][startIndex:endIndex,]
      nbOfElements <- endIndex-startIndex
      minY = 0;
      maxY = NA;
      for(label in labels){
          maxY = max(maxY, max(interruptedData[[interruptIndex]][,label], na.rm = TRUE), na.rm = TRUE)
      }
      png(filename=paste(plotDir, "/multiple-graph-interrupt-", plotNames[i], "-", interruptLabels[2*(interruptIndex)], ".png", sep=""), width=figureWidth, height=figureHeight, units="px", res=figureRes)
      plotSingleData(interruptedData[[interruptIndex]], labels, 
                     paste("Plot of", plotNames[i], "on", interruptLabels[2*(interruptIndex)])
                     , startIndex, endIndex, minY, 1.5*maxY)
      abline(v = record, col = "blue")
      dev.off(); 
      
    }
    
    ## Now boxplot
    for(label in labels){
      boxplotData <- data.frame(a = rawData[[i]][150:250,label])
      boxplotData["400-500s"] <- rawData[[i]][400:500,label]
      boxplotData["700-800s"] <- rawData[[i]][700:800,label]
      
      png(filename=paste(plotDir, "/boxplot-graph-", plotNames[i], "-" ,label, ".png", sep=""), 
          width=figureWidthSquare, height=figureHeight, units="px", res=figureRes)
      
      boxplot(boxplotData, ylab="Vertraging(ms)", outline = FALSE, names=c("150-250s", "400-500s", "700-800s"))
      dev.off(); 
    }
  }
}

if(debugmodus){
  for(run in 3:3){
    tryCatch({
      plotWithInterrupts("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/continious-%1.dat",
        c(
          paste("1-",run,sep=""), 
          paste("2-",run,sep="")
        ), 
        c("Node 1", "Node 2"), 
        1000, 
        c("UPDATE", "READ"), 
        paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/Fig/Continious-Run-", run, sep=""), 
        c(301,601), 
        c("All online", "Shut down", "One offline", "Power on", "All online")
      )
    },error = function(e) print(paste("Problem in postgresql continious run", run, e))
    )
  }
  
  for(run in 3:3){
    tryCatch({
      plotWithInterrupts("D:/Schooljaar 2013-2014/Thesis/Results/hbase/continious-%1.dat",
                         c(
                           paste("2-",run,sep=""), 
                           paste("3-",run,sep=""),
                           paste("5-",run,sep="")
                         ), 
                         c("Node 2", "Node 3", "Node 5"), 
                         1000, 
                         c("UPDATE", "READ"), 
                         paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/Fig/Continious-Run-", run, sep=""), 
                         c(301,601), 
                         c("All online", "Shut down", "One offline", "Power on", "All online")
                         
      )
    },error = function(e) print(paste("Problem in hbase continious run", run, e))
    )
  }
  
  for(run in 3:3){
    tryCatch({
      plotWithInterrupts("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/continious-%1.dat",
                         c(
                           paste("1-",run,sep=""), 
                           paste("2-",run,sep=""),
                           paste("3-",run,sep="")
                         ), 
                         c("Node 1", "Node 2", "Node 3"), 
                         1000, 
                         c("UPDATE", "READ"), 
                         paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/Fig/Continious-Run-", run, sep=""), 
                         c(301,601), 
                         c("All online", "Shut down", "One offline", "Power on", "All online")
                         
      )
    },error = function(e) print(paste("Problem in mongodb continious run", run, e))
    )
  }
}