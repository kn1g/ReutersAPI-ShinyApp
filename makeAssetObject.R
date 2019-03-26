makeAssetObject <- function(user, SETTINGS, CSVFile, queryindices){
  ## First query and save company info (equals static request) - then query and save the time series (euqals TS request)
  # define variables
  staticReq      <- NULL     # this can be confusig. staticReq is an integer which goes up with each try to maxtries as long as the query failes. If query is successful the variable is a list
  TSReq          <- NULL     # this can be confusig. TSReq is an integer which goes up with each try to maxtries as long as the query failes. If query is successful the variable is a list
  sleeptime      <- 3        # how long it will pause to make a new request if one failed
  maxtries       <- 3        # max. tries to query the info
  
  ## IF the ID is not an ISIN it could contain an compound sign (&), which causes trouble when passing over to the xml request in ds()
  # replace & with URL encoding character
  AssetID_staticRq <- gsub("&","&#38;", SETTINGS$securities[queryindices]) 
  AssetID_TSRq     <- AssetID_staticRq
  saveMemUse(1, SETTINGS$SessionID)
  ## Query the company information. Exit loop if request has data Try max. 10 times
  it <- 1
  while(!is.list(staticReq) && it < maxtries){ # as long as staticReq stays a integer and not a list the query didn't succeed.
    # Try to makle the query and save in in staticReq or set staticReq to the next try 
    staticReq <- tryCatch({
      ds(user, 
         requests = paste(AssetID_staticRq,"~XREF", sep = ""))
    },
    error=function(e){
      # log error 
      saveError(1, # Error Code 1 : Connection or Account problems
                ownError.msg        = paste("I tried", it, "time(s) to query the static data."),
                thridPartyError.msg = e,
                CSVFileName.arg     = SETTINGS$SessionID) 
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
    for(asset in AssetID_staticRq){
      saveError(2, # Error Code 2: Stores all AssetIDs that couldn't queried in static request (Connection or Account problems finally jumped to the next asset)
                ownError.msg    = paste("Tried the TS request ", it ,"times. Maybe there is a problem with your account or connection."),
                AssetID.arg     = asset,
                CSVFileName.arg = SETTINGS$SessionID) 
    }
    # end function and try next asset block
    return(F)
  }
  ## End of query
  saveMemUse(2, SETTINGS$SessionID)
  ## if the static query returned anything, save it as an error if it has no ISIN or save the asset info 
  # firstcheck weather it has an ISIN in it if not return error 
  AssetObjects <- list()
  for(i in 1:length(staticReq["Data",])){
    
    if(is.null(staticReq[["Data",i]]$ISIN)){
      print(paste("Hier stimmt was nicht. Symbol ist NULL:",staticReq[["Data",i]]$SYMBOL))
    }else{
      if(nchar(as.character(staticReq[["Data",i]]$ISIN)) != 12){ 
        # log error 
        saveError(3, # Error Code 3 : Could not match assetID and ISIN
                  ownError.msg    = paste("I could not match any ISIN to the identifier:", AssetID_staticRq[i]),
                  AssetID.arg     = AssetID_staticRq[i],
                  CSVFileName.arg = SETTINGS$SessionID) 
      }else{
        # replace asset ID with ISIN if it could be matched. To query with ISIN instead of ID if possible
        AssetID_TSRq[i] <- as.character(staticReq[["Data",i]]$ISIN)
        
      }
    }
    AssetObjects[[i]] <- list(ISIN        = as.character( staticReq[["Data",i]]$ISIN ),
                              AssetID     = as.character( AssetID_staticRq[i] ),
                              AssetID2    = as.character( staticReq[["Data",i]]$SYMBOL ), # can be removed. Just for debuggin purpose
                              DSCode      = as.character( staticReq[["Data",i]]$DSCD ),
                              CName       = as.character( staticReq[["Data",i]]$NAME ),
                              CountryCode = substr(staticReq[["Data",i]]$ISIN,1,2), # get the first two ISIN char
                              Currency    = staticReq[["Data",i]]$CCY,
                              Sector      = as.character( staticReq[["Data",i]]$INDM ),
                              Exchange    = staticReq[["Data",i]]$EXMNEM,
                              AssetType   = staticReq[["Data",i]]$TYPE,
                              TSData      = list() # define an empty list to store the TS request
                              )
  }
  ## end defining an AssetObject from static query
  saveMemUse(3, SETTINGS$SessionID)
  ## Prepare TS request
  qreq <- vector()
  
  # check for duplicates and skip them
  #first save the old order (In which AssetObjects are arranged)
  AssetID_TSRq.old <- AssetID_TSRq
  duplicates   <-  which(duplicated(AssetID_TSRq))
  # save the duplicates in the error log
  for(dupli in duplicates){
    saveError(7, # Error Code 7 : Duplicated Asset
              ownError.msg    = paste("These Objects where duplicates and skipped"),
              AssetID.arg     = AssetID_staticRq[dupli],
              ISIN.arg        = AssetID_TSRq.old[dupli],
              CSVFileName.arg = SETTINGS$SessionID)
  }
  # delete them
  AssetID_TSRq <- AssetID_TSRq[setdiff(seq_along(AssetID_TSRq), as.numeric(duplicates))]
  # also delete duplicated assetobjects
  # AssetObjects <- AssetObjects[setdiff(seq_along(AssetID_TSRq.old), as.numeric(duplicates))]

  
  
  
  if(length(AssetID_TSRq) > 0){
    for(i in 1:length(AssetID_TSRq)){
      
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
      qreq[i] <- paste(AssetID_TSRq[i],"~","=",SETTINGS$fields,"~",fromDate,"~",":",SETTINGS$toDate,"~",SETTINGS$periodicity,sep="")
    } # end for
    saveMemUse(4, SETTINGS$SessionID)
    ## Query the TS information. Exit loop if request has data Try max. 10 times
    it <- 1
    while(!is.list(TSReq) && it < maxtries){ # as long as staticReq stays a integer and not a list the query didn't succeed.
      # Try to make the query and save 
      TSReq <- tryCatch({
        # Request time series
        ds(user, 
           requests = qreq)
      },
      error=function(e){
        # log error 
        saveError(4, # Error Code 4 : Connection or Account problems in TS request
                  ownError.msg        = paste("I tried", it, "time(s) to query the TS data."),
                  thridPartyError.msg = e,
                  CSVFileName.arg     = SETTINGS$SessionID) 
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
      for(asset in AssetID_TSRq){
        saveError(5, # Error Code 5 : Stores all AssetIDs that couldn't queried (Connection or Account problems (TS request) finally jumped to the next asset)
                  ownError.msg    = paste("Tried the TS request ", it ,"times. Maybe there is a problem with your account or connection."),
                  AssetID.arg     = asset,
                  CSVFileName.arg = SETTINGS$SessionID) 
      }
      # end function and try next asset block
      return(F)
    }
    ## End of query
    
    # save data
    for(i in seq_along(AssetID_TSRq)){
      AssetObjects[[i]]$TSData <- TSReq[["Data",i]]
    }
  } # end if
  # save the Assets
  lapply(AssetObjects, saveAssets)
  saveMemUse(5, SETTINGS$SessionID)
  return(T)
}
