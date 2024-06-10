# Projet big data

"
Sujet : Concevoir et développer une application d’étude du patrimoine arboré


Dans cette partie la on analyse et on traite les données

stp
1. Exploration donné

"

# Importation des données
# Read the CSV file "Patrimoine_Arbore.csv" into the 'data' variable
# Parameters:
#   - header: TRUE to indicate that the CSV file has a header row
#   - encoding: "UTF-8" to specify the character encoding of the file
#   - sep: "," to specify the separator used in the CSV file
# test
data <- read.csv("Patrimoine_Arbore.csv", header = TRUE, sep = ",")

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
