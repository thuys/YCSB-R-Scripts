
for(run in c(1,3)){
  tryCatch({
    plotWithInterrupts("D:/Schooljaar 2013-2014/Thesis/Results-2/postgresql/continious-%1.dat",
                       c(
                         paste("1-",run,sep=""), 
                         paste("2-",run,sep="")
                       ), 
                       c("Node 1", "Node 2"), 
                       1000, 
                       c("UPDATE", "READ"), 
                       paste("D:/Schooljaar 2013-2014/Thesis/Results-2/postgresql/Fig/Continious-Run-", run, sep=""), 
                       c(333,666), 
                       c("All online", "Shut down", "One offline", "Power on", "All online")
    )
  },error = function(e) print(paste("Problem in postgresql continious run", run, e))
  )
}

for(run in c(1,3)){
  tryCatch({
    plotWithInterrupts("D:/Schooljaar 2013-2014/Thesis/Results-2/hbase/continious-%1.dat",
                       c(
                         paste("2-",run,sep=""), 
                         paste("3-",run,sep=""),
                         paste("5-",run,sep="")
                       ), 
                       c("Node 2", "Node 3", "Node 5"), 
                       1000, 
                       c("UPDATE", "READ"), 
                       paste("D:/Schooljaar 2013-2014/Thesis/Results-2/hbase/Fig/Continious-Run-", run, sep=""), 
                       c(336,653),  
                       c("All online", "Shut down", "One offline", "Power on", "All online")
                       
    )
  },error = function(e) print(paste("Problem in hbase continious run", run, e))
  )
}

for(run in c(1,3)){
  tryCatch({
    plotWithInterrupts("D:/Schooljaar 2013-2014/Thesis/Results-2/mongodb/continious-%1.dat",
                       c(
                         paste("1-",run,sep=""), 
                         paste("2-",run,sep=""),
                         paste("3-",run,sep="")
                       ), 
                       c("Node 1", "Node 2", "Node 3"), 
                       1000, 
                       c("UPDATE", "READ"), 
                       paste("D:/Schooljaar 2013-2014/Thesis/Results-2/mongodb/Fig/Continious-Run-", run, sep=""), 
                       c(333,666),  
                       c("All online", "Shut down", "One offline", "Power on", "All online")
                       
    )
  },error = function(e) print(paste("Problem in mongodb continious run", run, e))
  )
}