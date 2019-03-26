saveMemUse <- function(point, CSVFileName){
  
  if(!dir.exists("log")){ 
    dir.create("log") 
  }
  
  logFile = paste("log/","MemUse_",CSVFileName,".csv",sep="")
  if(!file.exists(logFile)){
    cat(paste("TimeStamp",
              "MemUse",
              "CodePart",
              sep=","), 
        file=logFile, append=FALSE, sep = "\n")
  }
  
  # write into error logfile
  cat(paste(Sys.time(),
            mem_used(),
            point,
            sep=","), 
      file=logFile, append=TRUE, sep = "\n")
  
}