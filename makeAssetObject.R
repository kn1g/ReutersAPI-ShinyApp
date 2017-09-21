makeAssetObject <- function(user, SETTINGS, CSVFile, queryindices){
  ## First query and save company info (equals static request) - then query and save the time series (euqals TS request)
  # define variables
  staticReq      <- NULL     # this can be confusig. staticReq is an integer which goes up with each try to maxtries as long as the query failes. If query is successful the variable is a list
  TSReq          <- NULL     # this can be confusig. TSReq is an integer which goes up with each try to maxtries as long as the query failes. If query is successful the variable is a list
  sleeptime      <- 3        # how long it will pause to make a new request if one failed
  maxtries       <- 3        # max. tries to query the info
  
  ## IF the ID is not an ISIN it could contain an compound sign (&), which causes trouble when passing over to the xml request in ds()
  # replace & with URL encoding character
  AssetID <- gsub("&","&#38;", CSVFile$Symbol[queryindices])
  
  ## Query the company information. Exit loop if request has data Try max. 10 times
  it <- 1
  while(!is.list(staticReq) && it < maxtries){ # as long as staticReq stays a integer and not a list the query didn't succeed.
    # Try to makle the query and save in in staticReq or set staticReq to the next try 
    staticReq <- tryCatch({
      ds(user, 
         requests = paste(AssetID,"~XREF", sep = ""))
    },
    error=function(e){
      # log error 
      saveError(1, # Error Code 1 : Connection or Account problems
                ownError.msg = paste("I tried", it, "time(s) to query the static data."),
                thridPartyError.msg = e) 
      Sys.sleep(sleeptime)
      NULL # set staticReq to NULL
    },
    warning=function(w){
      print(paste("I tried", it, "time(s) to query the static data."))
      print("I got this warning: ")
      print(w)
      Sys.sleep(sleeptime)
      NULL # set staticReq to NULL
    })
    it <- it +1
  }
  if(it >= maxtries){
    # print error to console
    print(paste("Tried the static reqest ", it ,"times. Maybe there is a problem with your account or connection."))
    
    # log error 
    saveError(2, # Error Code 2 : Connection or Account problems finally jumped to the next asset
              ownError.msg = paste("Tried the static request ", it ,"times. Maybe there is a problem with your account or connection.")) 
    
    # end function and try next asset block
    return(F)
  }
  ## End of query
  
  ## if the static query returned anything, save it as an error if it has no ISIN or save the asset info 
  # firstcheck weather it has an ISIN in it if not return error 
  AssetObjects <- list()
  for(i in 1:length(staticReq["Data",])){
    if(nchar(as.character(staticReq[["Data",i]]$ISIN)) != 12){ 
      # log error 
      saveError(3, # Error Code 3 : Could not match assetID and ISIN
                ownError.msg = paste("I could not match any ISIN to the identifier:", AssetID[i]),
                AssetID.arg  = AssetID) 
      
    }else{
      # replace asset ID with ISIN if it could be matched. To query with ISIN instead of ID if possible
      AssetID[i] <- as.character(staticReq[["Data",i]]$ISIN)

    }
    AssetObjects[[i]] <- list(ISIN        = staticReq[["Data",i]]$ISIN,
                              AssetID     = AssetID[i],
                              AssetID2    = staticReq[["Data",i]]$SYMBOL, # can be removed. Just for debuggin purpose
                              DSCode      = staticReq[["Data",i]]$DSCD,
                              CName       = staticReq[["Data",i]]$NAME,
                              CountryCode = substr(staticReq[["Data",i]]$ISIN,1,2), # get the first two ISIN char
                              Currency    = staticReq[["Data",i]]$CCY,
                              Sector      = staticReq[["Data",i]]$INDM,
                              Exchange    = staticReq[["Data",i]]$EXMNEM,
                              AssetType   = staticReq[["Data",i]]$TYPE,
                              TSData      = list() # define an empty list to store the TS request
    )
    print(AssetObjects)
    
  }
    ## end defining an AssetObject from static query
    
    ## Prepare TS request
  qreq <- vector()
  for(i in 1:length(AssetID)){
    # to avoid queries over a long timespan check if startdate is available in the loaded file and get it
    if(length(CSVFile$Start.Date[i])>0){
      # THIS MUST BE BETTER - Convert German start date
      temp <- strsplit(as.character(CSVFile$Start.Date[i]),"\\.")
      temp <- paste(temp[[1]][3],"-",temp[[1]][2],"-",temp[[1]][1],sep="")
      fromDate <- temp
    }else{
      fromDate <- SETTINGS$fromDate
    }
    ## End define startdate
    
    # prepare query string
    qreq[i] <- paste(AssetID[i],"~","=",SETTINGS$fields,"~",fromDate,"~",":",SETTINGS$toDate,"~",SETTINGS$periodicity,sep="")
  }
    
    ## Query the TS information. Exit loop if request has data Try max. 10 times
    it <- 1
    while(!is.list(TSReq) && it < maxtries){ # as long as staticReq stays a integer and not a list the query didn't succeed.
      # Try to make the query and save 
      TSReq <- tryCatch({
        # Request time series
        ds(user, 
           qreq)
      },

      error=function(e){
        # log error 
        saveError(4, # Error Code 4 : Connection or Account problems in TS request
                  ownError.msg = paste("I tried", it, "time(s) to query the TS data."),
                  thridPartyError.msg = e) 
        Sys.sleep(sleeptime)
        NULL # set staticReq to NULL
      },
      
      warning=function(w){
        print(paste("I tried", it, "time(s) to query the TS data."))
        print("I got this warning: ")
        print(e)
        Sys.sleep(sleeptime)
        NULL # set staticReq to NULL
      })
      it <- it +1
    }

    # write error into object
    if(it >= maxtries){
      # log error 
      saveError(5, # Error Code 5 : Connection or Account problems (TS request) finally jumped to the next asset
                ownError.msg = paste("Tried the TS request ", it ,"times. Maybe there is a problem with your account or connection.")) 
      
      # end function and try next asset block
      return(F)
    }
    ## End of query
    
    # save data
    for(i in 1:length(AssetID)){
      AssetObjects[[i]]$TSData <- TSReq[["Data",i]]
    }
    
  # save the Assets
  lapply(AssetObjects, saveAssets)
  
  return(T)
}