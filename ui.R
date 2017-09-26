
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  navbarPage("Reuters Data API",
             tabsetPanel(
                         tabPanel("Query Data",
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
                           actionButton("req_btn","Query data")
                         ),
                         tabPanel("Data Statistics",
                                  p("If query is finished you can get the stats here."),
                                  actionButton("stat_btn","Yeah, get me the stats"),
                                  p(),
                                  hr(),
                                  textOutput("nOofAssetsQueried"),
                                  
                                  textOutput("noISINmatch"),

                                  uiOutput("noISINMatchList"),
                                  
                                  uiOutput("noDataAvailable")
                                  # 
                                  # p(),
                                  # span("Reuters did not provide price for:"),
                                  # textOutput("noDataAvailable", inline = T),
                                  # span("Assets"),
                                  # 
                                  # p(),
                                  # span("Reuters did not provide volume for:"),
                                  # textOutput("noDataAvailable", inline = T),
                                  # span("Assets"),
                                  # 
                                  # p(),
                                  # span("Reuters did not provide MV for:"),
                                  # textOutput("noDataAvailable", inline = T),
                                  # span("Assets")
                         )
             )
             
             
             
  )
)
