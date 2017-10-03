saveAssets <- function(AssetObject){
  # Abspeichern der Datei um bei Verbindungsproblemen nicht alles zu verlieren
  if(!dir.exists("Assets")){ 
    dir.create("Assets") 
  }
    if(is.na(AssetObject$ISIN) || AssetObject$ISIN == "NA"){
      saveRDS(AssetObject, file= paste("Assets/","NoISIN_",sample(1:10000,1),sample(1:10000,1),"_",as.character(AssetObject$DSCode),".rds", sep= ""))
    }else{
      saveRDS(AssetObject, file= paste("Assets/",as.character(AssetObject$ISIN),".rds", sep= ""))
    }
}