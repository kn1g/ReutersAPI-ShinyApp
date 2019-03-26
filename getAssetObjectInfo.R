getAssetObjectInfo <- function(x, queriedFields){
  out <- list()
  if(is.null(dim(x$TSData))){
    out$HasData   <- F
  }else{
    out$HasData   <- ifelse(dim(x$TSData)[1] > 0,T,F)            # check if TS request returned data
  }
  out$TheISIN   <- ifelse(as.character(x$ISIN)   == "NA", NA, as.character( x$ISIN) )    # check if static req returned ISIN
  out$TheDSCD   <- ifelse(as.character(x$DSCode) == "NA", NA, as.character( x$DSCode) )  # check if static request returned anything
  out$TheSymbol <- x$AssetID2
  
  for(feld in queriedFields){
    out[[paste("Has",feld,sep="")]] <- ifelse(length(x$TSData[[feld]]) > 0,T,F)
  }

  return(out)
  
}