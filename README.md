# Shiny APP to query Reuters API (DS only at the moment)
Just because Excel sucks. This App provides a userinterface to query data from Reuters DS. 

## You need
* An Reuters Datastream Account (costs money)
* R
* RStudio (recommended)
* The following packages:
     
     ```
     install.packages("shiny")
     install.packages("XML")
     install.packages("RCurl")
     install.packages("devtools")
     devtools::install_github("fcocquemas/RDataStream")
     ```
     
If you have R you can copy paste the packges. In RStudio yust press the RUN APP button. In R type shiny::runApp()

## How to

* Get your DS Account login credentials and enter them into the form
* Get a list with all assets you want to query. It needs to be a .csv (";" spererated) with at least a column named "Symbol" (Start.Date column would also be fine but not required)
* Tick the "header" checkbox if you .csv file has a header row
* Enter the fields you want to query
* Enter the data range and Periodicity
* Specify the blocksize (it determines how many assets will be requested at once. I actually do not really know if it improves the speed. It should...)
* Tick the "resume" checkbox if you had a query before that crashed or lost the connection. 

## Just saying...

* The app has only limited error handling. 
* The app is taylored to my specific needs. Extensions/Contributions appreciated. 

## Will there be updates

Yes. I will add a feature to check the data quality.
