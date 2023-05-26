# Ce script est utilisé pour supprimer un échantillon de la base des données
library(RMySQL)
# Etablissement de la connection avec la BD
con = dbConnect(RMySQL::MySQL(),
                dbname="metadata2",
                host='localhost',
                port=3306,
                user='mohamed',
                password='')
deleter <- function(Sample_name){
  query_check_existence <- sprintf("SELECT COUNT(*) FROM Mass WHERE Sample_name='%s'",Sample_name)
  check_existence <- dbGetQuery(conn = con, statement = query_check_existence)
  # à partir du résultats de check_existence on peut vérifier si l'echantillon qu'on veut supprimer existe vraimant dans la base des données
  if (check_existence[1,1]!=0){ # Le cas ou il existe vraiment dans la BD
    delete_sample(Sample_name) # L'appel à la fonction qui fait la suppression
    message <- sprintf("The sample: '%s' has been deleted with success from the database", Sample_name)
    return(message)
  } else{ # Les cas ça n'existe pas
    message <- sprintf("The sample: '%s' doesn't exist in the database", Sample_name)
    return(message)
  }
}
delete_sample <- function(Sample_name){
  query_sample_id <- sprintf("SELECT Sample_ID FROM Mass WHERE Sample_name='%s'",Sample_name)
  Sample_ID <- dbGetQuery(conn = con, statement = query_sample_id) # Ici on a recupéré le SampleID de l'echantillon qu'on veut supprimer
  delete_from_main_tables(Sample_ID[1,1]) # La suppression de l'échantillon des tableaux qui necessite pas vérification
  # La récupération des autres IDs de l'echantillons qu'on veut supprimer
  query_Location_id <- sprintf("SELECT Location_ID FROM Sample WHERE Sample_ID='%d'",Sample_ID[1,1])
  Location_ID <- dbGetQuery(conn = con, statement = query_Location_id)
  query_protocol_id <- sprintf("SELECT Protocol_ID FROM Sample WHERE Sample_ID='%d'",Sample_ID[1,1])
  Protocol_ID <- dbGetQuery(conn = con, statement = query_protocol_id)
  query_phylo_id <- sprintf("SELECT Phylo_ID FROM Sample WHERE Sample_ID='%d'",Sample_ID[1,1])
  Phylo_ID <- dbGetQuery(conn = con, statement = query_phylo_id)
  # Dans la partie qui suive, on vérifie si le Location_ID, Protocol_Id et Phylo_ID sont associé seulemnt à l'échantillon qu'on veut supprimer 
  # parceque dans ce cas on va supprimer les lignes correspodante à ces Protol_ID, Location_ et Phylo_ID sinon on les garde 
  # (parceque y'a d'autres echantillons associés à ces mémes Protocl_Id, Phylo_Id et Location_ID)
  nbr_samples_with_same_location_id_query <- sprintf("SELECT COUNT(*) FROM Sample WHERE Location_ID='%d'",Location_ID[1,1])
  nbr_samples_with_same_location_id <- dbGetQuery(conn = con, statement = nbr_samples_with_same_location_id_query)
  if (nbr_samples_with_same_location_id[1,1]==1){
    delete_from_Geography(Location_ID[1,1])
  }
  nbr_samples_with_same_phylo_id_query <- sprintf("SELECT COUNT(*) FROM Sample WHERE Phylo_ID='%d'",Phylo_ID[1,1])
  nbr_samples_with_same_phylo_id <- dbGetQuery(conn = con, statement = nbr_samples_with_same_phylo_id_query)
  if (nbr_samples_with_same_phylo_id[1,1]==1){
    delete_from_Phylo(Phylo_ID[1,1])
  }
  nbr_samples_with_same_protocol_id_query <- sprintf("SELECT COUNT(*) FROM Sample WHERE Protocol_ID='%d'",Protocol_ID[1,1])
  nbr_samples_with_same_protocol_id <- dbGetQuery(conn = con, statement = nbr_samples_with_same_protocol_id_query)
  if (nbr_samples_with_same_protocol_id[1,1]==1){
    delete_from_protocol(Protocol_ID[1,1])
  }
  query_Sample <- sprintf("DELETE FROM Sample WHERE Sample_ID='%d'",Sample_ID[1,1])
  dbSendQuery(conn = con, statement = query_Sample)
  print("Deleted from Sample with success!")
}
delete_from_main_tables <- function(Sample_ID){
  query_genes <- sprintf("DELETE FROM Genes WHERE Sample_ID='%d'",Sample_ID)
  dbSendQuery(conn = con, statement = query_genes)
  print("Deleted from Genes with success!")
  query_literature <- sprintf("DELETE FROM Literature WHERE Sample_ID='%d'",Sample_ID)
  dbSendQuery(conn = con, statement = query_literature)
  print("Deleted from Literature with success!")
  query_Museum <- sprintf("DELETE FROM Museum WHERE Sample_ID='%d'",Sample_ID)
  dbSendQuery(conn = con, statement = query_Museum)
  print("Deleted from Museum with success")
  query_Morphological_infos <- sprintf("DELETE FROM Morphological_infos WHERE Sample_ID='%d'",Sample_ID)
  dbSendQuery(conn = con, statement = query_Morphological_infos)
  print("Deleted from Morphological_infos with success")
  query_Others <- sprintf("DELETE FROM Others WHERE Sample_ID='%d'",Sample_ID)
  dbSendQuery(conn = con, statement = query_Others)
  print("Deleted from Others with success")
  query_Mass <- sprintf("DELETE FROM Mass WHERE Sample_ID='%d'",Sample_ID)
  dbSendQuery(conn = con, statement = query_Mass)
  print("Deleted from Mass with success")
}
delete_from_Geography <- function(Location_ID){
  query_location <- sprintf("DELETE FROM Geography WHERE Location_ID='%d'",Location_ID)
  dbSendQuery(conn = con, statement = query_location)
  print("Deleted from Geography with success!")
}
delete_from_Phylo <- function(Phylo_ID){
  query_phylo_detailed <- sprintf("DELETE FROM Phylogeny_detailed WHERE Phylo_ID='%d'",Phylo_ID)
  dbSendQuery(conn = con, statement = query_phylo_detailed)
  print("Deleted from Phylogeny_detailed with success!")
  query_phylo_short <- sprintf("DELETE FROM Phylogeny_short WHERE Phylo_ID='%d'",Phylo_ID)
  dbSendQuery(conn = con, statement = query_phylo_short)
  print("Deleted from Phylogeny_short with success!")
}
delete_from_protocol <- function(Protocol_ID){
  query_protocol <- sprintf("DELETE FROM Protocol_infos WHERE Protocol_ID='%d'",Protocol_ID)
  dbSendQuery(conn = con, statement = query_protocol)
  print("Deleted from Protocol with success!")
}
dbDisconnect(con)

