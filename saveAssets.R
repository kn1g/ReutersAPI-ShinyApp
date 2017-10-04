saveAssets <- function(AssetObject){
  # Abspeichern der Datei um bei Verbindungsproblemen nicht alles zu verlieren
  if(!dir.exists("Assets")){ 
    dir.create("Assets") 
  }
    if(is.na(AssetObject$ISIN) || AssetObject$ISIN == "NA"){
      saveRDS(AssetObject, file= paste("Assets/","NoISIN_",as.character(AssetObject$DSCode),"-",sample(1:1000000,1),".rds", sep= ""))
    }else{
      saveRDS(AssetObject, file= paste("Assets/",as.character(AssetObject$ISIN),".rds", sep= ""))
    }
}