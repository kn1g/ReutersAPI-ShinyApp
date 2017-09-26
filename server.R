
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# source functions
source("makeAssetObject.R")
source("saveError.R")
source("saveAssets.R")

shinyServer(function(input, output) {
  
  observeEvent(input$req_btn,{
    # Loads the .csv sheet to get the IDs to query
    CSVFile <- tryCatch({
      inFile  <- input$IdentifierList
      read.csv(inFile$datapath, header=input$header, sep = ";")
    }, warning = function(w) {
      print(w)
      print("Warning: Something went wrong with the CSV File Uplaod")
      NULL
    }, error = function(e) {
      print(e)
      print("Error: Something went wrong with the CSV File Uplaod")
      NULL
    })
    if(!is.null(inFile)  && !is.null(CSVFile$Symbol) ){
      print("Identifier successfully loaded")
      
      ## Set user credentials
      user <- list(username = input$username, password = input$password)
      
      ## Settings
      SETTINGS <- list()
      # display number of Assets
      
      # Which securities do you want to query
      SETTINGS$securities  <- CSVFile$Symbol
      # What data do you want to query for the security
      #SETTINGS$fields      <- strsplit(gsub("\\s", "", input$fields) , ",")[[1]] # old version
      SETTINGS$fields      <- gsub("\\s", "", input$fields)
      # date in yyyy-mm-dd format from which to start series.
      SETTINGS$fromDate    <- input$dateRange[1]
      # date in yyyy-mm-dd format at which to end series.
      SETTINGS$toDate      <- input$dateRange[2]
      # check if one day is requested or a range (TODO)
      #if(SETTINGS$fromDate == SETTINGS$toDate){
      #  # date in yyyy-mm-dd format, if only one day is requested. If set, ignores fromDate, toDate and period.
      #  SETTINGS$datum       <- input$dateRange
      #}else{
      #  SETTINGS$datum       <- F
      #}
      # character describing the periodicity ('D','W','M','Q','Y'), defaults to daily
      SETTINGS$periodicity <- input$periodicity
      # boolean whether to output a dataframe in the "Data" row of the returned matrix.
      # SETTINGS$asDataFrame <- T # TODO
      # default "Datastream", useful if you want to access another Dataworks Enterprise data source. You can obtain the list of sources you have access to by using the dsSources() function in this package.
      # SETTINGS$source      <- "Datastream"
      
      # Blocksize determines how many assets will be request at one
      blocksize <- input$blocksize
      
      # Loops over each ID 
      # It could also be one request
      # lapply wäre schneller aber die Request können nicht parallelisiert werden
      # AssetObjects <- lapply(SETTINGS$securities[1:10], makeAssetObject, SETTINGS)
      # Daher for Schleife um lieber Fortschritt zu tracken und einzelnd abspeichern zu können, falls Verbindung abbricht etc.
      AssetObjects <- list()
      # in case there where connection problems and the download needs to be resumed
      if(input$resumeQuery){
        # check how many assets are already downloaded (NOTE: if anything changed like the order it will be chaos!)
        # I could doublecheck the ISINS but this costs too much time thats why I just count files
        f <- list.files(path = "Assets",pattern=".rds", full.names = T)
        # read in prvious files
        AssetObjects <- lapply(f, readRDS)
        startindex <- length(f)+1
      }else{
        startindex <- 1
      }
      n <-length(SETTINGS$securities)
      
      withProgress(message = 'Requesting data...', value = (startindex/n), {
        # create sequence
        indices <- startindex:n
        queryindices <- split(indices, as.numeric(gl(length(indices),blocksize,length(indices))))
        for(i in 1:length(queryindices)){
          # query the assets
          makeAssetObject(user, SETTINGS, CSVFile, queryindices[[i]])
          
          # Progress bar 
        incProgress(length(queryindices[[i]])/n, detail = paste("Requested", queryindices[[i]][length(queryindices[[i]])], "Assets", "out of", n))
        }
      })
      
      
      
      print("Done.")
    }else{
      print("CSV upload failed. Check format. It needs to be seperated by a ; and at least have a column named <Symbol>. Maybe you just forgot to load a file. Please try again.")
    }
    
    })

  observeEvent(input$stat_btn,{
    # output variables
    # From
    AllAssets <- 0
    noEntry <- 0
    NoData <- 0
    NoISIN <- 0
    NoDataNoISIN <- 0
    DataNoISIN <- 0
    
    # Loads the .csv sheet to get the IDs to query
    CSVFile <- tryCatch({
      inFile  <- input$IdentifierList
      read.csv(inFile$datapath, header=input$header, sep = ";")
    }, warning = function(w) {
      print(w)
      print("Warning: Something went wrong with the CSV File Uplaod")
      NULL
    }, error = function(e) {
      print(e)
      print("Error: Something went wrong with the CSV File Uplaod")
      NULL
    })
    if(!is.null(inFile)  && !is.null(CSVFile$Symbol) ){
      print("Identifier successfully loaded")}
    
    # save how many assets orignially where in the csv sheet
    AllAssets <- length(CSVFile$Symbol)
    
    # check if assets have ISIN and TS as well as static data
    f <- list.files(path = "Assets",pattern=".rds", full.names = T)
    # read in prvious files
    AssetObjects <- lapply(f, readRDS)
    AssetOverview <- lapply(AssetObjects, function(x){
      
      hasData <- ifelse(dim(x$TSData)[1] > 0,T,F) # check if TS request returned data
      theISIN <- ifelse(as.character(x$ISIN) == "NA", NA, as.character( x$ISIN) ) # check if static req returned ISIN
      theDSCD <- ifelse(as.character(x$DSCode) == "NA", NA, as.character( x$DSCode) ) # check if static request returned anything
      
      return(list(HasData = hasData,
                  TheISIN = theISIN,
                  TheSymbol = x$AssetID2,
                  TheDSCD = theDSCD))
      
    })
    
    # quick check if there is any object that has data in any way but no DSCD - I assume it it not possible because the DSCD is at least given if there is any data
    #fuckingerror <- lapply(AssetObjects, function(x){
    #  temp <- nchar(x)
    #  ifelse((temp[1] > 2 | temp[3] > 2 | temp[4] > 2 | temp[5] > 2 | temp[6] > 2 | temp[7] > 2 | temp[8] > 2 | temp[9] > 2 | temp[10] > 2),T,F)
    #})
    
    AssetOverview <- do.call(rbind, lapply(AssetOverview, data.frame, stringsAsFactors=FALSE))
    AssetOverview$TheSymbol <- as.character( AssetOverview$TheSymbol) 
    
    # I did not create an object for these symbols (maybe try to get data somewehere else)
    noEntry <- CSVFile$Symbol[(which(!(as.character( CSVFile$Symbol) %in% as.character(AssetOverview$TheSymbol))))]
    
    CSVFile$hasObje <- rep(F, length(CSVFile$Symbol))
    CSVFile$hasData <- rep(F, length(CSVFile$Symbol))
    CSVFile$theISIN <- rep(NA, length(CSVFile$Symbol))
    CSVFile$theDSCD <- rep(NA, length(CSVFile$Symbol))
    CSVFile$ChSymbl <- rep(NA, length(CSVFile$Symbol))
    
    # Append to CSV
    for(i in 1:length(AssetOverview$TheSymbol)){
      indx <- which(as.character( CSVFile$Symbol) == as.character(AssetOverview$TheSymbol[i]))
      CSVFile$hasObje[indx] <- T
      CSVFile$hasData[indx] <- AssetOverview$HasData[i]
      CSVFile$theISIN[indx] <- AssetOverview$TheISIN[i]
      CSVFile$ChSymbl[indx] <- AssetOverview$TheSymbol[i]
      CSVFile$theDSCD[indx] <- AssetOverview$TheDSCD[i]
    }
    
    
    # if file is uploaded check how many assets should be queried
    if(file.exists("log/error_log.csv")){
      errorLog <- read.csv("log/error_log.csv", header=T, sep = ",", stringsAsFactors = F)
      
      # save how man assets didn't have an ISIN
      NoISIN <- errorLog$AssetID[which(errorLog$Error_ID == 3)]
      
    }
    
    connectionProbs <- ifelse((any(errorLog$Error_ID == 2) | any(errorLog$Error_ID == 5)),"I maybe missed some assets because of connection problems", "Connection was fine all the time.") 
    
    # should be 0: length(which(AssetOverview$TheSymbol %in% noEntry))
    
    # If done offer the filte to download

    output$downloadAssetObjects_Btn <- downloadHandler(
      filename = function() {
        paste("updated_",input$IdentifierList$name, sep = "")
      },
      content = function(file) {
        write.csv(CSVFile, file, row.names = FALSE)
      }
    )
    
    output$noDataAvailable <- renderUI({
      list(
        p(paste("In total I tried to queried",length(AssetOverview$TheSymbol),"assets. Should be the same as:",length(CSVFile$Symbol))),
        p(connectionProbs),  
        # No Object for these
        p(paste("I could not get any data for:",length(noEntry))),
        # just ISIN no data
        p(paste("I found no TS data but an ISIN for so many assets:", length(which(AssetOverview$HasData == F & !is.na(AssetOverview$TheISIN))))),
        # No data for these 
        p(paste("I had an Symbol but no TS data for:",length(which(AssetOverview$HasData==F)),"assets.")),
        # No ISIN (no data + data)
        p(paste("I found no ISIN for:", length(NoISIN), "should be same as:", length(which(is.na(AssetOverview$TheISIN))))),
        # No ISIN but data
        p(paste("I created an object and had data but no ISIN for (Country code not set):",length(which(AssetOverview$HasData==T & is.na(AssetOverview$TheISIN))))),
        # No ISIN no data (Just Symbol)
        p(paste("I did not get an ISIN nor data for:", length(which(AssetOverview$HasData == F & is.na(AssetOverview$TheISIN))))),
        
        downloadButton('downloadAssetObjects_Btn', 'Download')
      )
    })
    
    
    
    # #################
    # output$AssetsNoData_D <- downloadHandler(
    #   filename = function() {
    #     paste('data-', Sys.Date(), '.csv', sep='')
    #   },
    #   content = function(con) {
    #     write.csv(
    #       CSVFile[(which(as.character( CSVFile$Symbol) %in% as.character( NotMatchedAssets))),], 
    #       con)
    #   }
    # )
    # output$noISINMatchList_D <- downloadHandler(
    #   filename = function() {
    #     paste('data-', Sys.Date(), '.csv', sep='')
    #   },
    #   content = function(con) {
    #     write.csv(CSVFile[(which(as.character( CSVFile$Symbol) %in% as.character( NotMatchedAssets))),], con)
    #   }
    # )
    # 
    # output$noISINMatchList   <- renderUI({
    #   list(
    #     p("You can download all Symbols that could not be matched to an ISIN here and try it again or elsewhere:"),
    #     downloadButton('noISINMatchList_D', 'Download')
    #   )
    # }) 
    ######################
    
  })
})


## Das ist alles zum rumexperimentieren mit der EIKON API
#########################################################
# get_symbology(list("MSFT.O", "GOOG.O", "IBM.N"), from_symbol_type="RIC", to_symbol_type="ISIN")
# 
# test <- get_timeseries(list("MSFT.O","VOD.L","IBM.N"),list("*"),"2016-01-01T15:04:05","2016-01-10T15:04:05","daily", F,raw_output = F)
# colnames(test)[8] <- "done"
# 
# ## csv File aus EIKON öffnen
# daten <- read_delim("D:/DISKSTATION/1_Projekte/1_Eigene/1_OFFEN/Forschung/Dissertation/git-gogs/New folder/EikonAPI/Stoxx600.csv", 
#                 ";", quote = "\\\"", escape_double = FALSE, 
#                  trim_ws = TRUE)
# 
# # Abfrage der Kurse
# library("rjson", lib.loc="~/R/win-library/3.4")
# set_app_id('AreYouFuckingSerious')
# fromJSON(get_timeseries(list(Stoxx600$Symbol),list("*"),"2016-01-01T15:04:05","2016-01-10T15:04:05","daily", F,raw_output = T))
