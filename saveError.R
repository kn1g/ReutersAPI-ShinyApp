saveError <- function(ERRORCODE, ownError.msg, thridPartyError.msg = "", AssetID.arg = NA, ISIN.arg = NA,  CSVFileName){
  AssetID <- AssetID.arg
  ISIN    <- ISIN.arg
  
  if(!dir.exists("log")){ 
    dir.create("log") 
  }
    
  logFile = paste("log/","error_log_",md5(CSVFileName),".csv",sep="")
  if(!file.exists(logFile)){
    cat(paste("TimeStamp",
              "Error_ID",
              "Error_Msg",
              "Error_3rdParty",
              "AssetID",
              "ISIN",
              sep=","), 
        file=logFile, append=FALSE, sep = "\n")
  }
  
  # write into error logfile
  cat(paste(Sys.time(),
            ERRORCODE,
            paste('"',gsub("[\r\n]", "", gsub("\"","",ownError.msg)),'"',sep=""),
            paste('"',gsub("[\r\n]", "", gsub("\"","",thridPartyError.msg)),'"',sep=""),
            paste('"',AssetID,'"',sep=""),
            paste('"',ISIN,'"',sep=""),
            sep=","), 
      file=logFile, append=TRUE, sep = "\n")
  
}