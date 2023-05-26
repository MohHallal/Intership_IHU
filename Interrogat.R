library(RMySQL)
# # Etablissement de la connection avec la BD
con = dbConnect(RMySQL::MySQL(),
                dbname="metadata2",
                host='localhost',
                port=3306,
                user='mohamed',
                password='')

Interrogating <- function(Sample_name="", Genus="", Species="", Arthropod_type="", Body_part="", Developmental_stage="", Protocol_ID=0, Location_ID=0, Family=""){
  query <- "SELECT S.Sample_ID,M.Sample_name, PD.Family,P.Genus, P.Species, MI.Arthropod_type, MI.Body_part, MI.Developmental_stage, Protocol_ID, Location_ID 
  FROM Sample AS S JOIN Phylogeny_short AS P ON S.Phylo_ID=P.Phylo_ID JOIN Mass AS M ON M.Sample_ID=S.Sample_ID JOIN Morphological_infos AS MI 
  ON S.Sample_ID=MI.Sample_ID JOIN Phylogeny_detailed AS PD ON PD.Phylo_ID=P.Phylo_ID" # La requete initial
  changed <- FALSE # Changed est FALSE lorsque la requete initial n'est pas changé, et devient TRUE lorsque on apporte une modification
  if (Family != ""){ # Si l'utilisateur à préciser une Family à filtrer, ce bloc va étre executer (pareil pour les autres parametres)
    query <- sprintf("%s WHERE PD.Family='%s'",query,Family)
    changed = TRUE
  }
  if (Sample_name != ""){
    if (changed){ # La requete initial est modifié selon si elle est modifié ou pas encore modifié
      query <- sprintf("%s AND M.Sample_name='%s'",query,Sample_name)
    }else{ 
    query <- sprintf("%s WHERE M.Sample_name='%s'",query,Sample_name)
    changed <- TRUE
    }
  }
  if (Genus != ""){
    if (changed){
      query <- sprintf("%s AND P.Genus='%s'",query,Genus)
    }else{
      query <- sprintf("%s WHERE P.Genus='%s'",query,Genus)
      changed <- TRUE
    }
  }
  if (Species != ""){
    if (changed){
      query <- sprintf("%s AND P.Species='%s'",query,Species)
    }else{
      query <- sprintf("%s WHERE P.Species='%s'",query,Species)
      changed <- TRUE
    }
  }
  if (Arthropod_type != ""){
    if (changed){
      query <- sprintf("%s AND MI.Arthropod_type='%s'",query,Arthropod_type)
    }else{
      query <- sprintf("%s WHERE MI.Arthropod_type='%s'",query,Arthropod_type)
      changed <- TRUE
    }
  }
  if (Body_part != ""){
    if (changed){
      query <- sprintf("%s AND MI.Body_part='%s'",query,Body_part)
    }else{
      query <- sprintf("%s WHERE MI.Body_part='%s'",query,Body_part)
      changed <- TRUE
    }
  }
  if (Developmental_stage != ""){
    if (changed){
      query <- sprintf("%s AND MI.Developmental_stage='%s'",query,Developmental_stage)
    }else{
      query <- sprintf("%s WHERE MI.Developmental_stage='%s'",query,Developmental_stage)
      changed <- TRUE
    }
  }
  if (Protocol_ID != 0){
    if (changed){
      query <- sprintf("%s AND S.Protocol_ID=%d",query,as.numeric(Protocol_ID))
    }else{
      query <- sprintf("%s WHERE S.Protocol_ID=%d",query,as.numeric(Protocol_ID))
      changed <- TRUE
    }
  }
  if (Location_ID != 0){
    if (changed){
      query <- sprintf("%s AND S.Location_ID=%d",query,as.numeric(Location_ID))
    }else{
      query <- sprintf("%s WHERE S.Location_ID=%d",query,as.numeric(Location_ID))
      changed <- TRUE
    }
  }
  data <- dbGetQuery(conn = con, statement = query)
  return(data)
}
data <- Interrogating()


