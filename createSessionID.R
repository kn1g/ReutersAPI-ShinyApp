createSessionID <- function(){
  return(paste(sample(1:999,1),"_",as.numeric(Sys.time()),sep=""))
}