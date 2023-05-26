 # Ce script est utilisé pour ajouter un échantillon à la base des données
 library(RMySQL)
 library(myTAI) # C'est le package utilisé pour faire la génération automatique de la classification taxonomique
 library(dplyr)
# # Etablissement de la connection avec la BD
 con = dbConnect(RMySQL::MySQL(),
                             dbname="metadata2",
                             host='localhost',
                             port=3306,
                             user='mohamed',
                             password='Azerty217!')

Add_to_DB<- function(Sample_Name,type,body_part,dvlpt_stage,homogeneization_appartus,homogeneization_medium,storing_mode,matrix,genus,species,
                     arbirary_name=NA,MS_system_used=NA,Institut=NA,nbr_of_spectra_used=NA,strain=NA,gene=NA,nbr_access=NA,cover=NA,identity=NA,
                     museumID=NA,PMID=NA,date_creation=NA,status=NA,contact=NA,comments=NA,country=NA,city=NA,storing_duration=NA,Available=NA){ 
  # Cette fonction fait l'appel a toutes les fonctions de l'ajout d'un échantillon et donne a chaque table ses propre paramétres necessaires
  # Premiérement on vérifie que cette échantillon n'existe pas déja dans la base de donnée
  query <- sprintf("SELECT COUNT(Sample_name) FROM Mass WHERE Sample_name='%s'", Sample_Name)
  get_nbr_names <- dbGetQuery(conn = con, statement = query)
  if (get_nbr_names[1,1]!=0){
    message <- sprintf("The sample '%s' already exists in the database!",Sample_Name)
    return(message)
  }
  # Deuxiément on vérifie que l'utilisateur à de la connexion internet (parceque la fonction taxonomy_finder doit avoir la connexion pour fonctionner) d'un coté, 
  # et de l'autre coté on vérifie que l'utilisateur n'a pas fait des faute d'orthographe dans la saisi du genre et de l'espéce
  taxon <- taxonomy_finder(genus,species,"ncbi") 
  if (nrow(taxon)==0){
    message <- sprintf("Check the spelling of the genus and/or species ('%s %s') of your sample: '%s' or your internet connection please.",genus ,species,Sample_Name)
    return(message)
  }
  # Ici on commence à ajouter les information sur la base des données, table par table
  New_id <- Add_new_sample_mass(MS_system_used, Institut, Sample_Name, nbr_of_spectra_used) # Appel a la fonction qui fait l'ajout à la table "Mass"
  Add_new_sample_Morphological_info(New_id, type, body_part, dvlpt_stage) # Appel à la fonction qui fait l'ajout à la table "Morphological_infos
  Add_new_sample_genes(New_id, gene, nbr_access, identity, cover) # ...
  Add_new_sample_museum(New_id, museumID)
  Add_new_sample_literature(New_id, PMID)
  Add_new_sample_others(New_id, arbirary_name, date_creation, status, contact, comments)
  Location_ID <- Add_new_sample_location(country, city) # Ici on fait l'ajout de "country" et "city" et en méme temp on récupére leurs 
                                                        #Location_ID dans le return (parceque ça sera utilisé dans l'ajout dans la table Sample)
  Protocol_ID <- Add_new_sample_protocol(homogeneization_appartus, homogeneization_medium, storing_mode, storing_duration, matrix) # Pareil
  PhyloID <- Add_new_sample_phylogeny_short(genus, species, strain) # Pareil
  # Dans les 5 prochaines lignes de cette fonction, si le genre et l'espéce n'existait pas déja dans la table, on fait la complétion automatique 
  # de la classification automatique et on la insére dans la table "Phylogeny_detailed"
  phylogeny_short <- dbGetQuery(conn = con, statement =  "SELECT * FROM Phylogeny_short") 
  phylogeny_detailed <- dbGetQuery(conn = con, statement =  "SELECT * FROM Phylogeny_detailed")
  if (nrow(phylogeny_short)>nrow(phylogeny_detailed)){ 
    # Si le nombre de lignes de la table "Phylogeny_short" est supérieure aux nombres de ligne de la table "Phylogeny_detailed", 
    # ça veut dire que "species" et "genus" sont nouvellement ajouté dans la table "Phylogeny short" 
    #(n'esistait pas avant) du coup on doit cherhcer son classification automataique et la mettre dans la table "Phylogeny_detailed"
    Add_new_sample_classification(PhyloID, genus ,species)
  }
  Add_new_sample(New_id, Available, Protocol_ID, PhyloID, Location_ID) 
  # Appel à la fonction qui fait l'ajout à la table "Sample" qui utilise SampleID, ProtocolID, PhyloID et LocationID généré
  message <- sprintf("The sample '%s' has been added with success to the database!",Sample_Name)
  return(message)
}
Add_new_sample_mass <- function(MS, institut, Name, nbr_spectra){
  # Cette fonction fait l'ajout spécifiquement à la table "Mass"
  query<-sprintf("INSERT INTO Mass (MS_system_used, Institut, Sample_Name, nbr_of_spectra_used) VALUES ('%s','%s','%s','%s')",MS,institut,Name,nbr_spectra)
  dbSendQuery(con, query)
  print("New_ID ajouté avec succés")
  query_new_id<-sprintf("SELECT Sample_ID From Mass WHERE Sample_Name = '%s' ",Name) # Recuperation du Sample_Id de ce nouveau Sample
  New_id <- dbGetQuery(conn = con, statement = query_new_id)
  New_id <- as.numeric(New_id[1,1]) 
  return(New_id)
}
Add_new_sample_Morphological_info <- function(id, type, body_part, dvlpt_stage){
  # Cette fonction fait l'ajout spécifiquement à la table "Morphological_infos"
  query<-sprintf("INSERT INTO Morphological_infos (Sample_ID,Arthropod_type,Body_part,Developmental_stage) VALUES (%d,'%s','%s','%s')",id, type, body_part, dvlpt_stage)
  dbSendQuery(con, query)
  print("added to morphology with success")
}
Add_new_sample_genes <- function(id, gene, nbr_access, identity, cover){
  # Cette fonction fait l'ajout spécifiquement à la table "genes"
  query<-sprintf("INSERT INTO Genes (Sample_ID,Gene_sequenced,Accession_number,Identity,Coverage) VALUES (%d,'%s','%s','%s','%s')",id ,gene, nbr_access, identity, cover)
  dbSendQuery(con, query)
  print("added to genes with success")
}
Add_new_sample_museum <- function(id, museumID){
  # Cette fonction fait l'ajout spécifiquement à la table "Museum"
  query<-sprintf("INSERT INTO Museum (Sample_ID,Museum_ID) VALUES (%d,'%s')",id , museumID)
  dbSendQuery(con, query)
  print("added to Museum with success")
}
Add_new_sample_literature <- function(id, PMID){
  # Cette fonction fait l'ajout spécifiquement à la table "Literature"
  query<-sprintf("INSERT INTO Literature (Sample_ID,PMID) VALUES (%d,'%s')",id , PMID)
  dbSendQuery(con, query)
  print("added to literature with success")
}
Add_new_sample_others <- function(id, arbirary_name, date_creation, status, contact, comments){
  # Cette fonction fait l'ajout spécifiquement à la table "Others"
  query<-sprintf("INSERT INTO Others (Sample_ID,Sample_arbitrary_N, Date_of_creation, Status, Contact, Comments) VALUES (%d,'%s','%s','%s','%s','%s')"
                 ,id , arbirary_name, date_creation, status, contact, comments)
  dbSendQuery(con, query)
  print("added to Others with success")
}
Add_new_sample_location <- function(country, city){
  # Cette fonction fait l'ajout spécifiquement à la table "Geography"
  if (is.na(country)){
    country <- 'NA'
  }
  if (is.na(city)){
    city<-'NA'
  }
  geography <- dbGetQuery(conn = con, statement =  "SELECT * FROM Geography")
  for (i in 1:nrow(geography)){ 
    # Ici on vérifie que country et city n'existent pas déja dans la table "Geography", et si ça existe, on récupére son Location_ID 
    # sans faire un nouveau ajout du méme "countre" et "city" (éviter la redandance)
    if (as.character(country) == geography[i,2] & as.character(city) == geography[i,3]){
      return(geography[i,1])
    }
  }
  # Le reste de la fonction fait l'ajout des nouveaux "city" et "country"
  query<-sprintf("INSERT INTO Geography (Geographical_origin_country, Geographical_origin_city_area) VALUES ('%s','%s')",country , city)
  dbSendQuery(con, query)
  query_get_location_id <- sprintf("SELECT Location_ID FROM Geography WHERE Geographical_origin_country='%s' AND Geographical_origin_city_area='%s'",country, city)
  get_location_id <- dbGetQuery(conn = con, statement = query_get_location_id)
  print("Added to Geography with success!")
  return(get_location_id[1,1]) # On récupére le Location_ID de de la "city" et "country" nouvellement ajouté
}
Add_new_sample_protocol <- function(appartus, medium, storing_mode, storing_duration, matrix){
  # Cette fonction fait l'ajout spécifiquement à la table "Protocol"
  if (is.na(storing_duration)){
    storing_duration <- 'NA'
  }
  protocols <- dbGetQuery(conn = con, statement =  "SELECT * FROM Protocol_infos")
  for (i in 1:nrow(protocols)){  # Ici on vérifie que ce protocol n'existe pas déja dans la table "Sample_preparation", 
                                # et si ça existe, on récupére son Protocol_ID sans faire un nouveau ajout du méme protocol (éviter la redandance)
    if (appartus==protocols[i,2] & medium==protocols[i,3] & storing_mode==protocols[i,4] & storing_duration==protocols[i,5] & matrix==protocols[i,6]){
      return(protocols[i,1])
    }
  }
  # Le reste de la fonction fait l'ajout du nouveau Protocol
  query <- sprintf("INSERT INTO Protocol_infos (Homogeneization_mode_apparatus,Homogeneization_mode_medium,Storing_mode,Storing_duration_months,Matrix) VALUES ('%s','%s','%s','%s','%s')",appartus, medium, storing_mode, storing_duration, matrix)
  dbSendQuery(con, query)
  query_get_protocol_id <- sprintf("SELECT Protocol_ID FROM Protocol_infos WHERE Homogeneization_mode_apparatus='%s' AND Homogeneization_mode_medium='%s' AND Storing_mode='%s' AND Storing_duration_months='%s' AND Matrix='%s'",
                                   appartus, medium, storing_mode, storing_duration, matrix)
  get_protocol_id <- dbGetQuery(conn = con, statement = query_get_protocol_id)
  print("Added to protocols_infos with success!")
  return(get_protocol_id[1,1]) # On récupére le Protocol_ID du protocol nouvellement ajouté
}
Add_new_sample_phylogeny_short <- function(genus, species, strain){
  print(genus)
  print(species)
  # Cette fonction fait l'ajout spécifiquement à la table "Phylogeny_short"
  phylogeny_short <- dbGetQuery(conn = con, statement =  "SELECT * FROM Phylogeny_short")
  for (i in 1:nrow(phylogeny_short)){ 
    # Ici on vérifie que le "genus" et "species" n'esiste pas déja dans la table "Phylogeny_short", sinon, on récupére son PhyloID sans refaire l'ajout
    if (genus == phylogeny_short[i,2] & species == phylogeny_short[i,3]){
      return(phylogeny_short[i,1])
    }
  }
  # Les 2 lignes suivantes font l'ajout les nouveaux "species" et "genus" à la table Phylogeny_short
  query<-sprintf("INSERT INTO Phylogeny_short (Genus,Species,strain) VALUES ('%s','%s','%s')",genus , species, strain)
  print(query)
  dbSendQuery(con, query)
  # Les lignes restent de cette fonction font la récupération de nouveau Location_ID
  query_get_phloID <- sprintf("SELECT Phylo_ID FROM Phylogeny_short WHERE Genus='%s' AND Species='%s'", genus, species)
  get_phyloID <- dbGetQuery(conn = con, statement = query_get_phloID)
  print("Added to phylogeny_short with success!")
  return(get_phyloID[1,1])
}
Add_new_sample_classification <- function(PhyloID, genus,species){
  # Cette fonction fait l'ajout spécifiquement à la table "Phylogeny_detailed"
  taxon <- taxonomy_finder(genus,species,"ncbi") 
  # Ici on génére la classification taxonomique par la fonction taxonomy_finder definit en haut (en utilisant la base des données NCBI comme référence)
  query_add <- sprintf("INSERT INTO Phylogeny_detailed (Phylo_ID, Kingdom, Phylum, Class, Ordre, Family, Genus, Species) VALUES (%d,'%s','%s','%s','%s','%s','%s','%s')", PhyloID,taxon[1,1],taxon[2,1],taxon[3,1],taxon[4,1],taxon[5,1],taxon[6,1],taxon[7,1])
  print(query_add)
  dbSendQuery(con, query_add)
  print("Added to phylogeny_detailed with success!")
}
Add_new_sample <- function(New_id, Available, Protocol_ID, PhyloID, Location_ID){
  # Cette fonction fait l'ajout spécifiquement à la table "Sample"
  query_add <- sprintf("INSERT INTO Sample (Sample_ID,Availabale,Protocol_ID,Phylo_ID,Location_ID) VALUES (%d,'%s',%d,%d,%d)",
                       New_id , Available, Protocol_ID, PhyloID, Location_ID)
  dbSendQuery(con, query_add)
  print("Added to Sample with success!")
}
taxonomy_finder <- function(genus,species,DB){
  # C'est la fonction qui récupére la classification taxonomique compléte d'un espéce à partir de son genre et son éspéce
  query <- paste(genus, species)
  taxon <- taxonomy(query, db = DB, output = "classification")
  taxon <- taxon[taxon$rank %in% c("kingdom","phylum", "class", "order","family","genus","species"), ]
  taxon <- data.frame(taxon)
  return(taxon)
}

                     