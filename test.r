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
data <- read.csv("Patrimoine_Arbore.csv", header = TRUE, encoding = "latin1", sep = ",")



print(head(data)) #a affiché les 6 premières lignes de data

#Conversion des types de données
