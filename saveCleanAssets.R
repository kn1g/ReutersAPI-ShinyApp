saveCleanAssets <- function(AssetObject){
  # Abspeichern der Datei um bei Verbindungsproblemen nicht alles zu verlieren
  if(!dir.exists("Assets")){ 
    dir.create("Assets") 
  }
  if(!dir.exists("Assets/clean")){ 
    dir.create("Assets/clean") 
  }
  if(is.na(AssetObject$ISIN) || AssetObject$ISIN == "NA"){
    saveRDS(AssetObject, file= paste("Assets/clean/","NoISIN_",as.character(AssetObject$DSCode),"-",sample(1:1000000,1),".rds", sep= ""))
  }else{
    saveRDS(AssetObject, file= paste("Assets/clean/",as.character(AssetObject$ISIN),".rds", sep= ""))
  }
}