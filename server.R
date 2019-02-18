# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(devtools)
library(shiny)
library(openssl)
library(pryr)

# source functions
source("makeAssetObject.R")
source("saveError.R")
source("saveAssets.R")
source("getAssetObjectInfo.R")
source("saveMemUse.R")
source("createSessionID.R")

shinyServer(function(input, output) {
  
  observeEvent(input$req_btn,{
    # Loads the .csv sheets to get the IDs to query
    # First get the dir with the .csv sheets
    CSVFile <- tryCatch({
      inFile  <- input$IdentifierList
      read.csv(inFile$datapath, header=T, sep = ";")
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
      # Which securities do you want to query
      SETTINGS$securities  <- as.character(CSVFile$Symbol)
      # What data do you want to query for the security
      #SETTINGS$fields      <- strsplit(gsub("\\s", "", input$fields) , ",")[[1]] # old version
      SETTINGS$fields      <- gsub("\\s", "", input$fields)
      # date in yyyy-mm-dd format from which to start series.
      SETTINGS$fromDate    <- input$dateRange[1]
      # date in yyyy-mm-dd format at which to end series.
      SETTINGS$toDate      <- input$dateRange[2]
      # character describing the periodicity ('D','W','M','Q','Y'), defaults to daily
      SETTINGS$periodicity <- input$periodicity
      # default "Datastream", useful if you want to access another Dataworks Enterprise data source. You can obtain the list of sources you have access to by using the dsSources() function in this package.
      # SETTINGS$source      <- "Datastream"
      # Blocksize determines how many assets will be request at one
      SETTINGS$blocksize <- input$blocksize
      
      SETTINGS$errorLogPath <- input$errorlog$datapath
      
      # Loops over each ID. Can also be one
      # lapply wäre schneller aber die Request können nicht parallelisiert werden. Außerdem ist der Flaschenhals die response time vom server
      # AssetObjects <- lapply(SETTINGS$securities, makeAssetObject, SETTINGS)
      # Daher for Schleife um lieber Fortschritt zu tracken und einzelnd abspeichern zu können, falls Verbindung abbricht etc.
      AssetObjects <- list()
      # in case there where connection problems and the download needs to be resumed
      if(input$resumeQuery){
        ## delete already queried assets and duplicates
        # check error log to ignore the duplicates
        if(file.exists(SETTINGS$errorLogPath)){
          errorLog <- read.csv(SETTINGS$errorLogPath, header=T, sep = ",", stringsAsFactors = F)
          SETTINGS$securities <-  SETTINGS$securities[!(as.character(SETTINGS$securities) %in% as.character(errorLog$AssetID[which(errorLog$Error_ID == 7)]))]
        }
        # read in all existing assets
        f <- list.files(path = "Assets",pattern=".rds", full.names = T)
        ExistingObj  <- lapply(f, readRDS)
        # Get the symbols of the already queried objects
        SymbsAlreadyQueried <- unlist(lapply(ExistingObj, function(x){x$AssetID}))
        # delete the already existing objects
        SETTINGS$securities   <-  SETTINGS$securities[!(as.character(SETTINGS$securities) %in% as.character(SymbsAlreadyQueried))]
      }
      
      # Request the data
      # get total numbers to track progress
      n <-length(SETTINGS$securities)
      # Querieng with progress tracking
      withProgress(message = 'Requesting data...', value = (1/n), {
        # create sequence
        indices <- 1:n
        # split the query in blocks if blocks where defined and save it in queryindices as lists like this (last block could be larger or smaller):
        # $`1`
        # [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
        # [64]  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
        # 
        # $`2`
        # [1] 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163
        # [64] 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 
        queryindices <- split(indices, 
                              as.numeric(
                                gl(
                                  length(indices),
                                  SETTINGS$blocksize,
                                  length(indices)))
                              )
        # iterate over the lists (number of list should be around indieces/blocksize)
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

  
  # Stage 1 - output list with assets that could not been queried at all
  observeEvent(input$clean_CSV_btn,{
    # 
  })
  
  # Stage 2 - kill all assets that have no TS data
  observeEvent(input$clean_NoTSData_btn,{
    # all object 
  })
  observeEvent(input$clean_NoTSData_btn,{ 
    # Stage 3 - kill all assets that have not more than x datapoints
    
    # Stage 4 - 
    
    # Last Stage - Save assets with clean data.frame names and delete obsolete data
    
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
    hasPrice <- F
    hasVolume <- F
    SETTINGS <- list()
    SETTINGS$fields <- strsplit(gsub("#","_x0023_",gsub("\\s", "", input$fields)), ",")[[1]]
    
    # Loads the .csv sheet to get the IDs to query
    CSVFile <- tryCatch({
      inFile  <- input$IdentifierList
      read.csv(inFile$datapath, header=T, sep = ";")
    }, warning = function(w) {
      print(w)
      print("Warning: (Stats) Something went wrong with the CSV File Uplaod")
      NULL
    }, error = function(e) {
      print(e)
      print("Error: (Stats) Something went wrong with the CSV File Uplaod")
      NULL
    })
    if(!is.null(inFile)  && !is.null(CSVFile$Symbol) ){
      print("Identifier successfully loaded")}
    
    # save how many assets orignially where in the csv sheet
    AllAssets <- length(CSVFile$Symbol)
    
    # check if assets have ISIN and TS as well as static data
    f <- list.files(path = "Assets",pattern=".rds", full.names = T)
    # read in prvious files
    AssetObjects  <- lapply(f, readRDS)
    AssetOverview <- lapply(AssetObjects, getAssetObjectInfo,SETTINGS$fields)
    
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
    for(field in SETTINGS$fields){
      CSVFile[[field]] <- rep(NA, length(CSVFile$Symbol))
    }
    
    # Append to CSV
    for(i in 1:length(AssetOverview$TheSymbol)){
      indx <- which(as.character( CSVFile$Symbol) == as.character(AssetOverview$TheSymbol[i]))
      CSVFile$hasObje[indx] <- T
      CSVFile$hasData[indx] <- AssetOverview$HasData[i]
      CSVFile$theISIN[indx] <- AssetOverview$TheISIN[i]
      CSVFile$ChSymbl[indx] <- AssetOverview$TheSymbol[i]
      CSVFile$theDSCD[indx] <- AssetOverview$TheDSCD[i]
      for(field in SETTINGS$fields){
        CSVFile[[field]][indx] <- AssetOverview[[paste("Has",field,sep="")]][i]
      }
    }
    
    
    # if file is uploaded check how many assets should be queried
    if(file.exists("log/error_log.csv")){
      errorLog <- read.csv("log/error_log.csv", header=T, sep = ",", stringsAsFactors = F)
      
      # save how man assets didn't have an ISIN
      NoISIN <- errorLog$AssetID[which(errorLog$Error_ID == 3)]
      # get all AssetIDs that where not queried because of some error
      errorAssetIDs <-  errorLog$AssetID
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
    
    # return if the queried data is complete
    #for(i in 1:length()){
    #  p(paste("I could query"),SETTINGS$[1]," for "),
    #}
    
    output$noDataAvailable <- renderUI({
      list(
        # Statements from logs
        p(paste("The CSV File has",length(CSVFile$Symbol),"Symbols.")),
        p(paste("Because of connection problems while querying the static data I missed",length(errorLog$AssetID[which(errorLog$Error_ID == 2)]),"assets.")),
        p(paste("Because of connection problems while querying the TS data I missed",length(errorLog$AssetID[which(errorLog$Error_ID == 5)]),"assets.")),
        p(paste("There where",length(unique(errorLog$ISIN[which(errorLog$Error_ID == 7)])),"duplicates (ISIN) in the CSV File.")),
        p(paste("I got",length(AssetOverview$TheSymbol),"asset objects.")),
        p(paste("The total number of Symbols from the csv file (",length(CSVFile$Symbol),") should match the sum of the duplicates, missed objects and the objects I have (",sum(length(unique(errorLog$ISIN[which(errorLog$Error_ID == 7)])),
                                                                                                                                                                                 length(errorLog$AssetID[which(errorLog$Error_ID == 2)]),
                                                                                                                                                                                 length(errorLog$AssetID[which(errorLog$Error_ID == 5)]),
                                                                                                                                                                                 length(AssetOverview$TheSymbol)),").")),
        # Did not create an object for these many (matching symbol name)
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
        # check which assets have volume
        p(paste("I got volume data for that many assets:", length(which(AssetOverview$HasVO == T)))),
        # check which assets have price
        p(paste("I got volume data for that many assets:", length(which(AssetOverview$P_0023x_S == T)))),
        
        
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
