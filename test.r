# Projet big data

"
Sujet : Concevoir et développer une application d’étude du patrimoine arboré


Dans cette partie la on analyse et on traite les données

TACHES A FAIRE :

1. Exploration donné
2. Visualisation des données sur des graphiques
3. Visualisation des données sur une carte
4. Etude des corrélations
5. Prédiction de la variable « Age estimé »
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
    # boucle sur chaque colonnes de data et convertit les données de latin à UTF-8
    for (col in colnames(data)) {
        data[[col]] <- iconv(data[[col]], from = "latin1", to = "UTF-8")
    }
    return(data)
}

data_utf8 = utf8(data)
print(head(data_utf8))

# #Conversion des types de données
# print(head(data$EditDate)) #a affiché la structure de data
# data$EditDate <- as.Date(data$EditDate, format = "%Y-%m-%d") #a converti la colonne EditDate en date
# print(head(data$EditDate)) #a affiché la structure de data
