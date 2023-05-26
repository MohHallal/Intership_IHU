library(RMySQL)
library(myTAI) # C'est le package utilisé pour faire la génération automatique de la classification taxonomique
library(dplyr)
source('Add_sample_mysql.R')
add_set <- function(file){
  result <- character(0)
  set <- read.csv(file = file)
  for (i in 1:nrow(set)){
    print(set[i,16]) # On fait ce print pour vérifier que l'ajout est en train de fonctionner 
                    # (parcequ'il prend longtemps à cause de la recherche de la classificiatio taxonomique automatique)
    message <- Add_to_DB(arbirary_name = set[i,1],date_creation = set[i,2],MS_system_used = set[i,3],Institut = set[i,4],status = set[i,5],genus = set[i,11],
              species = set[i,13],strain = set[i,15],Sample_Name = set[i,16],type = set[i,17],body_part = set[i,18],dvlpt_stage = set[i,19],
              country = set[i,20],city = set[i,21],homogeneization_appartus = set[i,22],homogeneization_medium = set[i,23],storing_mode = set[i,24],
              storing_duration = set[i,25],matrix = set[i,26],nbr_of_spectra_used = set[i,27],gene = set[i,28],nbr_access = set[i,29],identity = set[i,30],
              cover = set[i,31],museumID = set[i,32],PMID = set[i,33],Available = set[i,34],contact = set[i,35],comments = set[i,36])
    result <- paste(result, message, sep = "\n")
  }
  return(substring(result,2))
}
