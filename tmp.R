for(run in 3:3){
  tryCatch({
    plotAll(
      c(
        paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/continious-1-",run,".dat", sep=""), 
        paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/continious-2-",run,".dat", sep="")
      ), 
      c("Node 1", "Node 2"), 
      c(1000, 1000), 
      c("UPDATE", "READ"), 
      paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/Fig/Continious-Run-", run, sep="")
    )
  },error = function(e) print(paste("Problem in postgresql continious run", run, e))
  )
}