ReadInConstituents <- function(inFile){
  tryCatch({
    read.csv(inFile$datapath, T, sep = ";")
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
  }
}