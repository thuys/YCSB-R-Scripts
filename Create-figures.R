for(node in 1:3){
  for(run in 1:3){
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
  }
}

for(node in 0:1){
  for(run in 1:3){
    plotAll(
      c(
        paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/all-",run,".dat", sep=""), 
        paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/one-region-down-",(1+2*node), "-", run,".dat", sep=""),
        paste("D:/Schooljaar 2013-2014/Thesis/Results/hbase/one-master-down-",(2+2*node), "-", run,".dat", sep="")
      ), 
      c("All Online", "One region offline", "One master offline"), 
      c(1000, 1000, 1000), 
      c("UPDATE", "READ"), 
      paste("D:/Schooljaar 2013-2014/Thesis/Results/mongodb/Fig/Node-", (node+1), "/Run-", run, sep="")
    )
  }
}