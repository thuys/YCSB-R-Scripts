library(ggplot2) 
parseInput <- function(fileName, timeFrame){
  #GLOBAL VAR
  possibleActions <- c("INSERT", "UPDATE", "READ", "CLEANUP")
  globalElements <- c("Operations", "AverageLatency(us)", "MinLatency(us)", "MaxLatency(us)", "Return=1", "Return=0")
  
  #READ FILE
  k <- readLines(fileName, warn = FALSE)
  #PARSE FILE
  lineS <- strsplit(x=k, split = ", ")
  
  #Runtime
  runTimeLine=grep("[OVERALL], RunTime(ms),", k, fixed=TRUE)
  runTime = as.numeric(lineS[[runTimeLine]][3])
  # Throughput
  throughPutLine = grep("[OVERALL], Throughput(ops/sec)", k, fixed=TRUE)
  throughPut = as.numeric(lineS[[throughPutLine]][3])
  
  #CREATE rawdata matrix
  rawData <- matrix(nrow=floor(runTime/timeFrame), ncol=length(possibleActions)) 
  colnames(rawData) <- possibleActions
  rownames(rawData) <- seq(0, floor(runTime/timeFrame)*timeFrame-1, timeFrame)
  
  #CREATE global matrix 
  globalMatrix <- matrix(nrow = length(possibleActions), ncol = length(globalElements))
  colnames(globalMatrix) <- globalElements
  rownames(globalMatrix) <- possibleActions
  
  for(action in 1:length(possibleActions)){
    linesOfAction <- grep(paste("[", possibleActions[action], "]", sep = ""), k, fixed=TRUE)
    for(number in linesOfAction){
      
      tag <- lineS[[number]][2]
      value <- as.numeric(lineS[[number]][3])
      
      typeOfTag <- match(tag, globalElements)
      if(is.na(typeOfTag)){
        rawData[(as.numeric(tag)/timeFrame+1), action] <- value
      }
      else{
        globalMatrix[action, typeOfTag] <- value
      }
    }
  }
  
  
  return(list(raw = rawData, global =lineS, runTime = runTime, throughPut = throughPut))
}

plotSingleData = function(data, labels, title, minX, maxX, minY, maxY){
  plot.new()
  heading = paste(title) 
  plot(x = 0, y = 0, type="n", main=heading, xlab ="Time(ms)",ylab = "Latency(us)",
       xlim = c(minX, maxX), ylim = c(minY, maxY))
  colNb <- 0
  for(label in labels){
    colNb <- colNb +1
    lines(x = rownames(data), y = data[,label], type="o",col = colNb, pch = colNb) 

  }
  legend("topright", labels, col = 1:colNb, pch = 1:colNb)
  
}

plotMultipleDataSingleLabel = function(datas, type, labels, title, minX, maxX, minY, maxY){
  plot.new()
  heading = paste(title) 
  plot(x = 0, y = 0, type="n", main=heading, xlab ="Time(ms)",ylab = "Latency(us)",
       xlim = c(minX, maxX), ylim = c(minY, maxY))

  for(index in 1:length(datas)){
    lines(x = rownames(datas[[index]]), y = datas[[index]][,type], type="o",col = index, pch = index) 
    
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
     
    minY = min(rawData[[i]][, labels], na.rm = TRUE)
    maxY = max(rawData[[i]][, labels], na.rm = TRUE)
    png(filename=paste(exportDir, "/single-graph",fileNames[i], ".png", sep=""))
    plotSingleData(rawData[[i]], labels, 
                   paste("Plot of", fileNames[i])
                   , 0, data[[i]]$runTime, minY, maxY)
    dev.off(); 
  }
  
  for(label in labels){
    minY = NA
    maxY = NA
    maxX = NA
    for(i in 1:length(files)){
      minY = min(minY, min(rawData[[i]][, label], na.rm = TRUE), na.rm = TRUE)
      maxY = max(minY, max(rawData[[i]][, label], na.rm = TRUE), na.rm = TRUE)
      maxX = max(minY, data[[i]]$runTime, na.rm = TRUE)
    }

    png(filename=paste(exportDir, "/multiple-graph",label, ".png", sep=""))
    plotMultipleDataSingleLabel(rawData, label, fileNames, 
                   paste("Plot of", label)
                   , 0, maxX, minY, maxY)
    dev.off(); 
  }
}
input1 <- parseInput("example_code.txt", 200)
input2 <- parseInput("example_code2.txt", 2000)

rawData1 = input1$raw
rawData2 = input2$raw

#plotSingleData(rawData1, c("UPDATE", "READ"), "TEST PLOT", 0, input1$runTime, 0, max(rawData1[, 2], na.rm = TRUE))
#plotSingleData(rawData2, c("UPDATE", "READ"), "TEST PLOT", 0, input2$runTime, 0, max(rawData2[, 2], na.rm = TRUE))

#plotMultipleDataSingleLabel(list(rawData1, rawData2), "READ", c("1", "2"),"TEST PLOT", 0, input2$runTime, 0, max(rawData2[, 2], na.rm = TRUE))
plotAll(c("example_code.txt", "example_code2.txt"), c("file-1", "file-2"), c(200, 2000),
        c("UPDATE", "READ"), ".")


