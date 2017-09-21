
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  observeEvent(input$req_btn,{
    # Loads the .csv sheet to get the IDs to query
    tryCatch({
      inFile  <- input$IdentifierList
      CSVFile <- read.csv(inFile$datapath, header=input$header, sep = ";")
    }, warning = function(w) {
      print(w)
      print("Warning: Something went wrong with the CSV File Uplaod")
    }, error = function(e) {
      print(e)
      print("Error: Something went wrong with the CSV File Uplaod")
    })
    if(!is.null(inFile)){
      print("Identifier successfully loaded")
      
      ## Set user credentials
      user <- list(username = input$username, password = input$password)
      print(user)
      
      ## Settings
      SETTINGS <- list()
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
      
      # If done offer the filte to download
      output$downloadAssetObjects <- renderUI({
        downloadButton('downloadAssetObjects_Btn', 'Download the Data')
      })
      output$downloadAssetObjects_Btn <- downloadHandler(
        filename = "AssetObjects.RData",
        content = function(file) {
          save(AssetObjects, file=file)
        }
      )
      
      print("Done.")
    }else{
      print("CSV upload failed. Please try again.")
    }
    
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