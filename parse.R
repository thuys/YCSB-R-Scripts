assign("sToRemove", 10, envir = .GlobalEnv)
assign("movingAverageFrame", 20, envir = .GlobalEnv)
assign("possibleActions", c("INSERT", "UPDATE", "READ", "CLEANUP", "SCAN"), envir = .GlobalEnv)
assign("globalElements", c("Operations", "AverageLatency(us)", "MinLatency(us)", "MaxLatency(us)", "Return=1", "Return=0", "Return=-1"), envir = .GlobalEnv)
assign("eventElements", c("ID", "MinLatency(us)", "Has started", "Has Finished", "Exit code"), envir = .GlobalEnv)

assign("figureWidth", 4096, envir = .GlobalEnv)
assign("figureHeight", 2048, envir = .GlobalEnv)
assign("figureRes", 300, envir = .GlobalEnv)
library(ggplot2) 
parseInput <- function(fileName, timeFrame){
  
  #READ FILE
  k <- readLines(fileName, warn = FALSE)
  #PARSE FILE
  lineS <- strsplit(x=k, split = ", ")
  
  #Runtime
  runTimeLine=grep("[OVERALL], RunTime(ms),", k, fixed=TRUE)
  tryCatch({
    runTime = as.numeric(lineS[[runTimeLine]][3])
  },error = function(e) print(paste("Problem in file ", fileName)))
  # Throughput
  throughPutLine = grep("[OVERALL], Throughput(ops/sec)", k, fixed=TRUE)
  throughPut = as.numeric(lineS[[throughPutLine]][3])
  
  #CREATE rawdata matrix
  rawData <- matrix(nrow=floor(runTime/timeFrame), ncol=length(possibleActions)) 
  colnames(rawData) <- possibleActions
  rownames(rawData) <- seq(0, floor(runTime/timeFrame)*timeFrame-1, timeFrame)/1000
  
  #CREATE global matrix 
  globalMatrix <- matrix(nrow = length(possibleActions), ncol = length(globalElements))
  colnames(globalMatrix) <- globalElements
  rownames(globalMatrix) <- possibleActions
  
  #CREATE event matrix 
  eventMatrix <- matrix(nrow = length(possibleActions), ncol = length(globalElements))
  #colnames(eventMatrix) <- eventMatrix
  #rownames(eventMatrix) <- eventMatrix
  
  for(action in 1:length(possibleActions)){
    linesOfAction <- grep(paste("[", possibleActions[action], "]", sep = ""), k, fixed=TRUE)
    for(number in linesOfAction){
      
      tag <- lineS[[number]][2]
      value <- as.numeric(lineS[[number]][3])
      
      typeOfTag <- match(tag, globalElements)
      if(is.na(typeOfTag)){
        if(!is.na(as.numeric(tag)) && as.numeric(tag)/timeFrame+1<floor(runTime/timeFrame)){
          rawData[(as.numeric(tag)/timeFrame+1), action] <- value/1000
        }
        #rawData[(as.numeric(tag)/timeFrame+1), action] <- value
      }
      else{
        globalMatrix[action, typeOfTag] <- value/1000
      }
    }
  }
  eventLines <- grep("EVENT", k, fixed=TRUE)
  eventMatrix <- matrix(nrow = max(0,(length(eventLines)-1)), ncol = length(eventElements))
  if(length(eventLines) > 1){
    rownames(eventMatrix) <- seq(1,(length(eventLines)-1))
    colnames(eventMatrix) <- eventElements[1:length(eventElements)]
  
    for(i in 1:(length(eventLines)-1)){
      line <- eventLines[i+1]
      lineValue <- lineS[[line]]
      oldRow <- rownames(eventMatrix)
      oldRow[i] <- as.numeric(lineValue[3])/1000
      rownames(eventMatrix) <- oldRow
      eventMatrix[i,] <- c(lineValue[2], as.numeric(lineValue[4])/1000, lineValue[5], lineValue[6], lineValue[7])
      
    }
    #rownames(eventMatrix) <- eventMatrix
  }
  return(list(raw = rawData, global =globalMatrix, runTime = runTime, throughPut = throughPut, events = eventMatrix))
}

plotSingleData = function(data, labels, title, minX, maxX, minY, maxY, showPoints=TRUE, showAverage=TRUE){
  plot.new()
  heading = paste(title) 
  plot(x = 0, y = 0, type="n", main=heading, xlab ="Time(s)",ylab = "Latency(ms)",
       xlim = c(minX, maxX), ylim = c(minY, maxY))
  colNb <- 0
  for(label in labels){
    colNb <- colNb +1
    if(showPoints){
      lines(x = rownames(data), y = data[,label], type="p",col = colNb, pch = colNb) 
    }
    if(showAverage){
      lines(x = rownames(data), y = movingAverage(data[,label],10), type="l",col = colNb, pch = colNb, lwd=2) 
    }
    
  }
  legend("topright", labels, col = 1:colNb, pch = 1:colNb)
  
}

plotMultipleDataSingleLabel = function(datas, type, labels, title, minX, maxX, minY, maxY, showPoints=TRUE, showAverage=TRUE){
  plot.new()
  heading = paste(title) 
  plot(x = 0, y = 0, type="n", main=heading, xlab ="Time(s)",ylab = "Latency(ms)",
       xlim = c(minX, maxX), ylim = c(minY, maxY))

  for(index in 1:length(datas)){
    if(showPoints){
      lines(x = rownames(datas[[index]]), y = datas[[index]][,type], type="p",col = index, pch = index) 
    }
    if(showAverage){
      lines(x = rownames(datas[[index]]), y = movingAverage(datas[[index]][,type],movingAverageFrame), type="l",col = index, pch = index, lwd=2) 
    }
  }
  legend("topright", labels, col = 1:length(datas), pch = 1:length(datas))
  
}

plotAll = function(files, fileNames, timeFrames, labels, exportDir){
  data <- list();
  rawData <- list();
  for(i in 1:length(files)){
    returnValue = parseInput(files[i], timeFrames[i])
    data[[i]] <- returnValue
    rawData[[i]] <- returnValue$raw
    
    minY = 0
    #minY = min(rawData[[i]][, labels], na.rm = TRUE)
    maxY = max(rawData[[i]][(sToRemove*timeFrames[i]/1000):nrow(rawData[[i]]), labels], na.rm = TRUE)
    png(filename=paste(exportDir, "/single-graph",fileNames[i], ".png", sep=""), width=figureWidth, height=figureHeight, units="px", res=figureRes)
    plotSingleData(rawData[[i]], labels, 
                   paste("Plot of", fileNames[i])
                   , sToRemove, data[[i]]$runTime/1000, minY, maxY)
    dev.off(); 
  }
  
  for(label in labels){
    minY = 0
    maxY = NA
    maxX = NA
    for(i in 1:length(files)){
     # minY = min(minY, min(rawData[[i]][, label], na.rm = TRUE), na.rm = TRUE)
      maxY = max(minY, max(rawData[[i]][(sToRemove*timeFrames[i]/1000):nrow(rawData[[i]]), label], na.rm = TRUE), na.rm = TRUE)
      maxX = max(minY, data[[i]]$runTime/1000, na.rm = TRUE)
    }

    png(filename=paste(exportDir, "/multiple-graph",label, ".png", sep=""), width=figureWidth, height=figureHeight, units="px", res=figureRes)
    plotMultipleDataSingleLabel(rawData, label, fileNames, 
                   paste("Plot of", label)
                   , sToRemove, maxX, minY, maxY)
    dev.off(); 
  }
}

plotSingleLoadMultipleLabels = function(data, labels, title, minX, maxX, minY, maxY){
  plot.new()
  heading = paste(title) 
  plot(x = 0, y = 0, type="n", main=heading, xlab ="Nb of requests/s",ylab = "Average Latency(ms)",
       xlim = c(minX, maxX), ylim = c(minY, maxY))
  
  for(index in 1:length(labels)){
    lines(x = rownames(data), y = data[,labels[index]], type="o",col = index, pch = index)   
  }
  legend("topright", labels, col = 1:length(labels), pch = 1:length(labels))
  
}


plotMultipleLoadSingleLabel = function(datas, type, labels, title, minX, maxX, minY, maxY){
  plot.new()
  heading = paste(title) 
  plot(x = 0, y = 0, type="n", main=heading, xlab ="Nb of requests/s",ylab = "Average Latency(ms)",
       xlim = c(minX, maxX), ylim = c(minY, maxY))
  
  for(index in 1:length(datas)){
    lines(x = rownames(datas[[index]]), y = datas[[index]][,type], type="o",col = index, pch = index)   
  }
  legend("topright", labels, col = 1:length(datas), pch = 1:length(datas))
  
}

plotMultipleLoadMultipleLabels = function(datas, types, labels, title, minX, maxX, minY, maxY){
  plot.new()
  heading = paste(title) 
  plot(x = 0, y = 0, type="n", main=heading, xlab ="Nb of requests/s",ylab = "Average Latency(ms)",
       xlim = c(minX, maxX), ylim = c(minY, maxY))
  
  legende = c(1:(length(types)*length(datas)))
  
  for(index in 1:length(datas)){
    for(type in 1:length(types)){
      lines(x = rownames(datas[[index]]), y = datas[[index]][,types[type]], type="o",
            col = ((index-1)*length(types)+type), pch = ((index-1)*length(types)+type))
      legende[(index-1)*length(types)+type] <- paste(labels[index], types[type], sep="-")
   }
  }
  legend("topright", legende, col = 1:(length(types)*length(datas)), pch = 1:(length(types)*length(datas)))
  
}

plotLoadTesting = function(files, dbNames, nbOfRequests, timeFrame, labels, exportDir){
  # Collect by possibleActions on "AverageLatency(us)" in ms
  data <- list();
  globalDatas <- list();
  averageLatency <- list();
  
  minX <- NA
  maxX <- NA
  
  minY <- 0
  maxY <- NA
  for(dbs in 1:length(nbOfRequests)){
    data[[dbs]] <- list();
    globalDatas[[dbs]] <- matrix(nrow = length(nbOfRequests[[dbs]]), ncol = length(possibleActions))
    averageLatency[[dbs]] <- matrix(nrow=length(nbOfRequests[[dbs]]), ncol=1);
    rownames(averageLatency[[dbs]]) <- nbOfRequests[[dbs]]
    
    colnames(globalDatas[[dbs]]) <- possibleActions
    rownames(globalDatas[[dbs]]) <- nbOfRequests[[dbs]]
    for(runOfDBs in 1:length(nbOfRequests[[dbs]])){
      returnValue = parseInput(sub("%1", nbOfRequests[[dbs]][runOfDBs], files[[dbs]]), timeFrame)
      
      data[[dbs]][[runOfDBs]] <- returnValue
      globalDatas[[dbs]][runOfDBs, ] <- t(returnValue$global[, "AverageLatency(us)"])
      averageLatency[[dbs]][runOfDBs, 1] = returnValue$throughPut
    }
    
    minX = min(min(nbOfRequests[[dbs]], na.rm = TRUE), minX, na.rm = TRUE)
    maxX = max(max(nbOfRequests[[dbs]], na.rm = TRUE), maxX, na.rm = TRUE)
    
    minY = min(min(globalDatas[[dbs]], na.rm = TRUE), minY, na.rm = TRUE)
    maxY = max(max(globalDatas[[dbs]], na.rm = TRUE), maxY, na.rm = TRUE)
  }
  
  png(filename=paste(exportDir, "/loadbalance-all", ".png", sep=""), width=figureWidth, height=figureHeight, units="px", res=figureRes)
  plotMultipleLoadMultipleLabels(globalDatas, labels, dbNames, "Load plot", minX, maxX, minY, maxY)
  dev.off(); 
  
  for(label in labels){
    minX <- NA
    maxX <- NA
    
    minY <- 0
    maxY <- NA
    for(dbs in 1:length(files)){
      minY = min(min(globalDatas[[dbs]][,label], na.rm = TRUE), minY, na.rm = TRUE)
      maxY = max(max(globalDatas[[dbs]][,label], na.rm = TRUE), maxY, na.rm = TRUE)
      minX = min(min(nbOfRequests[[dbs]], na.rm = TRUE), minX, na.rm = TRUE)
      maxX = max(max(nbOfRequests[[dbs]], na.rm = TRUE), maxX, na.rm = TRUE)
    }
    png(filename=paste(exportDir, "/loadbalance-label-",label, ".png", sep=""), res=figureRes)
    plotMultipleLoadSingleLabel(globalDatas, label, dbNames, paste("Plot of", label), minX, maxX, minY, maxY)
    dev.off(); 
  }
  
  for(dbs in 1:length(files)){
    minY <- 0
    maxY <- NA
    
    minX <- min(nbOfRequests[[dbs]], na.rm = TRUE)
    maxX <- max(nbOfRequests[[dbs]], na.rm = TRUE)
    
    for(label in labels){
      minY = min(min(globalDatas[[dbs]][,label], na.rm = TRUE), minY, na.rm = TRUE)
      maxY = max(max(globalDatas[[dbs]][,label], na.rm = TRUE), maxY, na.rm = TRUE)
    }
    png(filename=paste(exportDir, "/loadbalance-db-", dbNames[[dbs]], ".png", sep=""), width=figureWidth, height=figureHeight, units="px", res=figureRes)
    plotSingleLoadMultipleLabels(globalDatas[[dbs]], labels, paste("Plot for", dbNames[[dbs]]), minX, maxX, minY, maxY)
    dev.off(); 
    
    png(filename=paste(exportDir, "/loadbalance-realthroughput-db-", dbNames[[dbs]], ".png", sep=""), width=figureWidth, height=figureHeight, units="px", res=figureRes)
    plot(x = rownames(averageLatency[[dbs]]), y = (averageLatency[[dbs]]/as.numeric(rownames(averageLatency[[dbs]]))),
         type="b", main=paste("Requested vs real requests for", dbNames[[dbs]]), 
         xlab ="Requested nb of requests/s",ylab = "Real nb of requests/s / Requested nb of requests/s",  
         xlim = c(minX, maxX), ylim=c(0,1))
    dev.off(); 
  }
  
  globalDatas
}
input1 <- parseInput("example_code.txt", 1000)
input2 <- parseInput("example_code2.txt", 2000)

rawData1 = input1$raw
rawData2 = input2$raw

globalData1 = input1$global
#plotSingleData(rawData1, c("UPDATE", "READ"), "TEST PLOT", 0, input1$runTime, 0, max(rawData1[, 2], na.rm = TRUE))
#plotSingleData(rawData2, c("UPDATE", "READ"), "TEST PLOT", 0, input2$runTime, 0, max(rawData2[, 2], na.rm = TRUE))

#plotMultipleDataSingleLabel(list(rawData1, rawData2), "READ", c("1", "2"),"TEST PLOT", 0, input2$runTime, 0, max(rawData2[, 2], na.rm = TRUE))
plotAll(c("example_code.txt", "example_code2.txt"), c("file-1", "file-2"), c(200, 1000),
        c("UPDATE", "READ"), ".")

files <- list()
nbOfRequests <- list()
timeFrame <- 1000

files[[1]] <- paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/all-%1.dat")

nbOfRequests[[1]] <- c(1,2,3)

files[[2]] <- paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/all-%1.dat")

nbOfRequests[[2]] <- c(1,2)
test <- plotLoadTesting(files, c("MongoDB", "PostgreSQL"), nbOfRequests, 
                timeFrame, c("UPDATE", "READ"), ".")