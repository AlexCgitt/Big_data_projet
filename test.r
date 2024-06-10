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
	- Conversion des types de données (des caractère en numéric) 
	- Corriger l'encodage de UTF-8
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
data = utf8(data)
# print(head(data))
# View(data)

"
Description du jeu de données
"
# print(table(data$X))
# print(table(data$Y))
# print(table(data$OBJECTID))
# print(table(data$created_date))
# print(table(data$created_user))
# print(table(data$src_geo))
# print(table(data$clc_quartier))
# print(table(data$clc_secteur))
# print(table(data$id_arbre))
# print(table(data$haut_tot))
# print(table(data$haut_tronc))
# print(table(data$tronc_diam))
# print(table(data$fk_arb_etat))
# print(table(data$fk_stadedev))
# print(table(data$fk_port))
# print(table(data$fk_pied))
# print(table(data$fk_situation))
# print(table(data$fk_revetement))
# print(table(data$commentaire_environnement))
# print(table(data$dte_plantation))
# print(table(data$age_estim))
# print(table(data$fk_prec_estim))
# print(table(data$clc_nbr_diag))
# print(table(data$dte_abattage))
# print(table(data$fk_nomtech))
# print(table(data$last_edited_user))
# print(table(data$last_edited_date))
# print(table(data$villeca))
# print(table(data$nomfrancais))
# print(table(data$nomlatin))
# print(table(data$GlobalID))
# print(table(data$CreationDate))
# print(table(data$Creator))
# print(table(data$EditDate))
# print(table(data$Editor))
# print(table(data$Editor))
# print(table(data$Editor))
# print(table(data$Editor))
# print(table(data$Editor))
# print(table(data$feuillage))
# print(table(data$feuillage))
# print(table(data$feuillage))
# print(table(data$feuillage))
# print(table(data$feuillage))
# print(table(data$remarquable))




"
Conversion des types de données (des caractère en numéric)
utilisation de la fonction as.numeric pour convertir les données de caractère en numérique
utilisation de la fonction as.Date pour convertir les données de caractère en date
utilisation de la fonction as.character pour convertir les données de numérique en caractère
"
data$X <- as.numeric(data$X) #a converti la colonne X en numérique
data$Y <- as.numeric(data$Y) #a converti la colonne Y en numérique
data$OBJECTID <- as.numeric(data$OBJECTID) #a converti la colonne OBJECTID en numérique
data$created_date <- as.Date(data$created_date, format = "%Y/%m/%d") #a converti la colonne created_date en date
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
data$dte_plantation <- as.Date(data$dte_plantation, format = "%Y/%m/%d") #a converti la colonne dte_plantation en date
data$age_estim <- as.numeric(data$age_estim) #a converti la colonne age_estim en numérique
data$fk_prec_estim <- as.numeric(data$fk_prec_estim) #a converti la colonne fk_prec_estim en numérique
data$clc_nbr_diag <- as.numeric(data$clc_nbr_diag) #a converti la colonne clc_nbr_diag en numérique
data$dte_abattage <- as.Date(data$dte_abattage, format = "%Y/%m/%d") #a converti la colonne dte_abattage en date
data$fk_nomtech <- as.character(data$fk_nomtech) #a converti la colonne fk_nomtech en caractère
data$last_edited_user <- as.character(data$last_edited_user) #a converti la colonne last_edited_user en caractère
data$last_edited_date <- as.Date(data$last_edited_date, format = "%Y/%m/%d") #a converti la colonne last_edited_date en date
data$villeca <- as.character(data$villeca) #a converti la colonne villeca en caractère
data$nomfrancais <- as.character(data$nomfrancais) #a converti la colonne nomfrancais en caractère
data$nomlatin <- as.character(data$nomlatin) #a converti la colonne nomlatin en caractère
data$GlobalID <- as.character(data$GlobalID) #a converti la colonne GlobalID en caractère  
data$CreationDate <- as.Date(data$CreationDate, format = "%Y/%m/%d") #a converti la colonne CreationDate en date
data$Creator <- as.character(data$Creator) #a converti la colonne Creator en caractère
data$EditDate <- as.Date(data$EditDate, format = "%Y/%m/%d") #a converti la colonne EditDate en date
data$Editor <- as.character(data$Editor) #a converti la colonne Editor en caractère
data$feuillage <- as.character(data$feuillage) #a converti la colonne feuillage en caractère
data$remarquable <- as.character(data$remarquable) #a converti la colonne remarquable en caractère

#affiche type de data$X
#permet de verifier si la conversion a bien été faite
print("data$X")
print(class(data$X))
print("data$Y")
print(class(data$Y))
print("data$OBJECTID")
print(class(data$OBJECTID))
print("data$created_date")
print(class(data$created_date))
print("data$created_user")
print(class(data$created_user))
print("data$src_geo")
print(class(data$src_geo))
print("data$clc_quartier")
print(class(data$clc_quartier))
print("data$clc_secteur")
print(class(data$clc_secteur))
print("data$id_arbre")
print(class(data$id_arbre))
print("data$haut_tot")
print(class(data$haut_tot))
print("data$haut_tronc")
print(class(data$haut_tronc))
print("data$tronc_diam")
print(class(data$tronc_diam))
print("data$fk_arb_etat")
print(class(data$fk_arb_etat))
print("data$fk_stadedev")
print(class(data$fk_stadedev))
print("data$fk_port")
print(class(data$fk_port))
print("data$fk_pied")
print(class(data$fk_pied))
print("data$fk_situation")
print(class(data$fk_situation))
print("data$fk_revetement")
print(class(data$fk_revetement))
print("data$commentaire_environnement")
print(class(data$commentaire_environnement))
print("data$dte_plantation")
print(class(data$dte_plantation))
print("data$age_estim")
print(class(data$age_estim))
print("data$fk_prec_estim")
print(class(data$fk_prec_estim))
print("data$clc_nbr_diag")
print(class(data$clc_nbr_diag))
print("data$dte_abattage")
print(class(data$dte_abattage))
print("data$fk_nomtech")
print(class(data$fk_nomtech))
print("data$last_edited_user")
print(class(data$last_edited_user))
print("data$last_edited_date")
print(class(data$last_edited_date))
print("data$villeca")
print(class(data$villeca))
print("data$nomfrancais")
print(class(data$nomfrancais))
print("data$nomlatin")
print(class(data$nomlatin))
print("data$GlobalID")
print(class(data$GlobalID))
print("data$CreationDate")
print(class(data$CreationDate))
print("data$Creator")
print(class(data$Creator))
print("data$EditDate")
print(class(data$EditDate))
print("data$Editor")
print(class(data$Editor))
print("data$feuillage")
print(class(data$feuillage))
print("data$remarquable")
print(class(data$remarquable))

# print(head(data))
# print(summary(data))
# View(data)


"
Nettoyer les données
    - Valeurs manquantes
    - Valeurs aberrantes
    - Doublons
"

na_indices <- which(is.na(data), arr.ind = TRUE)
na_df <- data.frame(Ligne = na_indices[, 1], Colonne = colnames(data)[na_indices[, 2]])
#print(na_df)