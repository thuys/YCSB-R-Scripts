for(node in 1:3){
  for(run in 1:3){
    tryCatch({
      plotAll(
        c(
          paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/all-",run,".dat", sep=""), 
          paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/one-down-",node, "-", run,".dat", sep="")
        ), 
        c("All Online", "One offline"), 
        c(1000, 1000), 
        c("UPDATE", "READ"), 
        paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/Fig/Node-", node, "/Run-", run, sep="")
      )
    },error = function(e) print(paste("Problem in mongoDB node", node, "run", run, e))
    )
  }
}



for(node in 1:2){
  for(run in 1:2){
    tryCatch({
      plotAll(
        c(
          paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/all-",run,".dat", sep=""), 
          paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/one-postgres-down-",node, "-", run,".dat", sep="")
        ), 
        c("All Online", "One postgres offline"), 
        c(1000, 1000), 
        c("UPDATE", "READ"), 
        paste("D:/Schooljaar 2013-2014/Thesis/Results/postgresql/Fig/Node-", node, "/Run-", run, sep="")
      )
    },error = function(e) print(paste("Problem in Postgresql node", node, "run", run, e)))
  }
}

for(node in 2:3){
  for(run in 1:3){
    tryCatch({
    plotAll(
      c(
        paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/all-",run,".dat", sep=""), 
        paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/one-region-down-",(node), "-", run,".dat", sep="")
        #paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/one-master-down-",(node), "-", run,".dat", sep="")
      ), 
      c("All Online", "One region offline"),#, "One master offline"), 
      c(1000, 1000),#, 1000), 
      c("UPDATE", "READ"), 
      paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/Fig/Node-", (node), "/Run-", run, sep="")
    )},error = function(e) print(paste("Problem in HBase node", node, "run", run, e)))
  }
}