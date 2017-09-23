saveError <- function(ERRORCODE, ownError.msg, thridPartyError.msg = "", AssetID.arg = NA, ISIN.arg = NA){
  AssetID <- AssetID.arg
  ISIN    <- ISIN.arg
  
  if(!dir.exists("log")){ 
    dir.create("log") 
  }
    
  logFile = paste("log/","error_log.csv",sep="")
  if(!file.exists(logFile)){
    cat(paste(Sys.time(),
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
            '"',gsub("[\r\n]", "", gsub("\"","",ownError.msg)),'"',
            '"',gsub("[\r\n]", "", gsub("\"","",thridPartyError.msg)),'"',
            '"',AssetID,'"',
            '"',ISIN,'"',
            sep=","), 
      file=logFile, append=TRUE, sep = "\n")
  
}