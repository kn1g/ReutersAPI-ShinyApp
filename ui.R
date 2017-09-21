
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Reuters Api"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("username","Username"),
      passwordInput("password","Password"),
      checkboxInput('header', 'Header', TRUE),
      fileInput("IdentifierList","List of IDs (ISIN, RIC)",
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv')
               ),
      textInput("fields", "Fields to query (see manual)", value = c("P#S, MV, VO")), # make a selectInput someday
      dateRangeInput("dateRange","Date Range"),
      selectInput("periodicity", "Periodicity", c("daily" = 'D',"weekly" = 'W',"monthly" = 'M',"quaterly" = 'Q',"yearly" = 'Y'), selected	= "M"),
      numericInput("blocksize","How many Asset do you want to query in one query? (More Assets more risk something crashes) but faster",value=10),
      checkboxInput("resumeQuery", "Do you want to resume a query?"),
      actionButton("req_btn","Query data"),
      uiOutput("downloadAssetObjects")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # plotOutput("")
    )
  )
))
