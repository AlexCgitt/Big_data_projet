# Projet big data

"
Sujet : Concevoir et développer une application d’étude du patrimoine arboré


Dans cette partie la on analyse et on traite les données

stp
1. Exploration donné
6. Régression
7. Export pour l'IA
8. Livrable
"

"
---------------------------------------------------------------------------------
1. Exploration donné
	- Description du jeu de données 
	- Conversion des types de données (des caractère en numéric) X
	- Corriger l'encodage de UTF-8 X
	- Nettoyer les données (valeurs manquantes, valeurs aberrantes, doublons)
	- Analyse exploratoires (les graphes)
"


"
Importation des données
    - Read the CSV file Patrimoine_Arbore.csv into the 'data' variable
    - header: TRUE to indicate that the CSV file has a header row
    - sep: , to specify the separator used in the CSV file
"
data <- read.csv("Patrimoine_Arbore.csv", header = TRUE, sep = ",")


"
Corriger l'encodage de UTF-8
    - fonction utf8
        - Convertir les données de latin à UTF-8
        - Utiliser la fonction iconv pour convertir les données
        - Boucle sur chaque colonne de data et convertir les données de latin à UTF-8
"
utf8 <- function(data) {
    for (col in colnames(data)) {
        data[[col]] <- iconv(data[[col]], from = "latin1", to = "UTF-8")
    }
    return(data)
}

"
permet d'afficher un beau tableau
"
data_utf8 = utf8(data)
print(head(data_utf8))
View(data_utf8)

"
Description du jeu de données
    
"


data$X <- as.numeric(data$X) #a converti la colonne X en numérique
data$Y <- as.numeric(data$Y) #a converti la colonne Y en numérique
data$OBJECTID <- as.numeric(data$OBJECTID) #a converti la colonne OBJECTID en numérique
data$created_date <- as.Date(data$created_date, format = "%Y-%m-%d %H:%M:%S%z") #a converti la colonne created_date en date
data$created_user <- as.character(data$created_user) #a converti la colonne created_user en caractère
data$src_geo <- as.character(data$src_geo) #a converti la colonne src_geo en caractère
data$clc_quartier <- as.character(data$clc_quartier) #a converti la colonne clc_quartier en caractère
data$clc_secteur <- as.character(data$clc_secteur) #a converti la colonne clc_secteur en caractère
data$id_arbre <- as.numeric(data$id_arbre) #a converti la colonne id_arbre en numérique
data$haut_tot <- as.numeric(data$haut_tot) #a converti la colonne haut_tot en numérique
data$haut_tronc <- as.numeric(data$haut_tronc) #a converti la colonne haut_tronc en numérique
data$tronc_diam <- as.numeric(data$tronc_diam) #a converti la colonne tronc_diam en numérique
data$fk_arb_etat <- as.character(data$fk_arb_etat) #a converti la colonne fk_arb_etat en caractère
data$fk_stadedev <- as.character(data$fk_stadedev) #a converti la colonne fk_stadedev en caractère
data$fk_port <- as.character(data$fk_port) #a converti la colonne fk_port en caractère
data$fk_pied <- as.character(data$fk_pied) #a converti la colonne fk_pied en caractère
data$fk_situation <- as.character(data$fk_situation) #a converti la colonne fk_situation en caractère
data$fk_revetement <- as.character(data$fk_revetement) #a converti la colonne fk_revetement en caractère
data$commentaire_environnement <- as.character(data$commentaire_environnement) #a converti la colonne commentaire_environnement en caractère
data$dte_plantation <- as.Date(data$dte_plantation, format = "%Y-%m-%d %H:%M:%S%z") #a converti la colonne dte_plantation en date
date$age_estim <- as.numeric(data$age_estim) #a converti la colonne age_estim en numérique
date$fk_prec_estim <- as.numeric(data$fk_prec_estim) #a converti la colonne fk_prec_estim en numérique
date$clc_nbr_diag <- as.numeric(data$clc_nbr_diag) #a converti la colonne clc_nbr_diag en numérique




# #Conversion des types de données
# print(head(data$EditDate)) #a affiché la structure de data
# data$EditDate <- as.Date(data$EditDate, format = "%Y-%m-%d") #a converti la colonne EditDate en date
# print(head(data$EditDate)) #a affiché la structure de data

na_indices <- which(is.na(data), arr.ind = TRUE)
na_df <- data.frame(Ligne = na_indices[, 1], Colonne = colnames(data)[na_indices[, 2]])
print(na_df)