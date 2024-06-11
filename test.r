# Projet big data
library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
install.packages("RColorBrewer")

"
Sujet : Concevoir et développer une application d’étude du patrimoine arboré


Dans cette partie la on analyse et on traite les données

Les étapes de traitement des données sont les suivantes :
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
# X = Longitude
# Y = Latitude
# OBJECTID = Identifiant de l'objet
# created_date = Date de création
# created_user = Utilisateur de création
# data$src_geo = Source géographique
# clc_quartier = Quartier
# clc_secteur = Secteur
# id_arbre = Identifiant de l'arbre
# haut_tot = Hauteur totale
# haut_tronc = Hauteur du tronc
# tronc_diam = Diamètre du tronc
# fk signifie clé étrangère : Une clé étrangère est un groupe de colonnes d’une table qui fait référence à la clé primaire d’une autre table
# fk_arb_etat = Etat de l'arbre
# fk_stadedev = Stade de développement
# fk_port = Port (forme de l'arbre : reduit, semi-libre, ...)
# fk_pied = Pied (forme du sol : gazon, revetement, ...)
# fk_situation = Situation (situation de l'arbre : groupe, aligné, ...)
# fk_revetement = Revetement (revetement du sol : gazon, revetement, ...) SERT A RIEN
# commentaire_environnement = Commentaire sur l'environnement
# dte_plantation = Date de plantation
# age_estim = Age estimé
# fk_prec_estim = Précision de l'age estimé
# clc_nbr_diag = Nombre de diagnostics
# dte_abattage = Date d'abattage
# fk_nomtech = Nom technique
# last_edited_user = Dernier utilisateur édité
# last_edited_date = Date de la dernière édition
# villeca = Ville
# nomfrancais = Nom français
# nomlatin = Nom latin
# nomlatin, fk_nomtech et nomfrancais sont les noms de l'arbre (presque les memes, sert limite a rien de mettre les deux)


# comparé fk_nomtech, nomlatin et nomfrancais

# for (i in 1:nrow(data)) {
#     if (data$fk_nomtech[i] != data$nomlatin[i] || data$fk_nomtech[i] != data$nomfrancais[i] || data$nomlatin[i] != data$nomfrancais[i]) {
#         print(data[i, c("fk_nomtech", "nomlatin", "nomfrancais")])
#         print("---------------------------------------------------")
#     }
# }

#les colonnes nomlatin, nomfrancais et fk_nom sont les memes à 4 valeurs près donc on se permet de supprimer la colonne nomlatin
#suppression de la colonne nomlatin
data$nomlatin <- NULL


#print(table(data$nomlatin))
#print(table(data$fk_nomtech))
#print(table(data$nomfrancais))


# GlobalID = Identifiant global
# CreationDate = Date de création de l'objet (GlobalID)

# for (i in 1:nrow(data)) {
#     if (data$CreationDate[i] != data$created_date[i] ) {
#         print(data[i, c("created_date", "CreationDate")])
#         print("---------------------------------------------------")
#     }
# }
#creationDate et created_date sont différents
# Creator = Créateur SERT A RIEN ON REMPLACE PAR created_user
# print(table(data$created_user))
# print(table(data$Creator))
# EditDate = Date d'édition
# Editor = Editeur
# print(table(data$Editor))
# print(table(data$last_edited_user))
# feuillage = Feuillage
# remarquable = Remarquable


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
#data$nomlatin <- as.character(data$nomlatin) #a converti la colonne nomlatin en caractère
data$GlobalID <- as.character(data$GlobalID) #a converti la colonne GlobalID en caractère  
data$CreationDate <- as.Date(data$CreationDate, format = "%Y/%m/%d") #a converti la colonne CreationDate en date
data$Creator <- as.character(data$Creator) #a converti la colonne Creator en caractère
data$EditDate <- as.Date(data$EditDate, format = "%Y/%m/%d") #a converti la colonne EditDate en date
data$Editor <- as.character(data$Editor) #a converti la colonne Editor en caractère
data$feuillage <- as.character(data$feuillage) #a converti la colonne feuillage en caractère
data$remarquable <- as.character(data$remarquable) #a converti la colonne remarquable en caractère

#affiche type de data$X
#permet de verifier si la conversion a bien été faite
# print("data$X")
# print(class(data$X))
# print("data$Y")
# print(class(data$Y))
# print("data$OBJECTID")
# print(class(data$OBJECTID))
# print("data$created_date")
# print(class(data$created_date))
# print("data$created_user")
# print(class(data$created_user))
# print("data$src_geo")
# print(class(data$src_geo))
# print("data$clc_quartier")
# print(class(data$clc_quartier))
# print("data$clc_secteur")
# print(class(data$clc_secteur))
# print("data$id_arbre")
# print(class(data$id_arbre))
# print("data$haut_tot")
# print(class(data$haut_tot))
# print("data$haut_tronc")
# print(class(data$haut_tronc))
# print("data$tronc_diam")
# print(class(data$tronc_diam))
# print("data$fk_arb_etat")
# print(class(data$fk_arb_etat))
# print("data$fk_stadedev")
# print(class(data$fk_stadedev))
# print("data$fk_port")
# print(class(data$fk_port))
# print("data$fk_pied")
# print(class(data$fk_pied))
# print("data$fk_situation")
# print(class(data$fk_situation))
# print("data$fk_revetement")
# print(class(data$fk_revetement))
# print("data$commentaire_environnement")
# print(class(data$commentaire_environnement))
# print("data$dte_plantation")
# print(class(data$dte_plantation))
# print("data$age_estim")
# print(class(data$age_estim))
# print("data$fk_prec_estim")
# print(class(data$fk_prec_estim))
# print("data$clc_nbr_diag")
# print(class(data$clc_nbr_diag))
# print("data$dte_abattage")
# print(class(data$dte_abattage))
# print("data$fk_nomtech")
# print(class(data$fk_nomtech))
# print("data$last_edited_user")
# print(class(data$last_edited_user))
# print("data$last_edited_date")
# print(class(data$last_edited_date))
# print("data$villeca")
# print(class(data$villeca))
# print("data$nomfrancais")
# print(class(data$nomfrancais))
# print("data$nomlatin")
# print(class(data$nomlatin))
# print("data$GlobalID")
# print(class(data$GlobalID))
# print("data$CreationDate")
# print(class(data$CreationDate))
# print("data$Creator")
# print(class(data$Creator))
# print("data$EditDate")
# print(class(data$EditDate))
# print("data$Editor")
# print(class(data$Editor))
# print("data$feuillage")
# print(class(data$feuillage))
# print("data$remarquable")
# print(class(data$remarquable))

# print(head(data))
# print(summary(data))
#View(data)
# print(head(data))
# print(summary(data))
#View(data)



"
Nettoyer les données
    - Valeurs manquantes
    - Valeurs aberrantes
    - Doublons
"
"
Mise en minuscule de toute les colonnes de type caractère
"
for (colonne in names(data)) {
  if (is.character(data[[colonne]])) {
    data[[colonne]] <- tolower(data[[colonne]])
  }
}

#tous les characteres "" deviennent des NA
data[data == ""] <- NA

# na_indices <- which(is.na(data), arr.ind = TRUE)
# na_df <- data.frame(Ligne = na_indices[, 1], Colonne = colnames(data)[na_indices[, 2]])
# #print(na_df)

"
Nettoyage colonne X et Y
Si il n'y a pas de X ou de Y, on supprime la ligne
"
# on observe deja les valeurs manquantes dans les colonnes X et Y
# print("--------------------")
# print(table(is.na(data$X)))
# print(table(is.na(data$Y)))
# print("--------------------")
# affiche les lignes avec des valeurs manquantes
# print(data[is.na(data$X) | is.na(data$Y), c("X", "Y")])

# on supprime les lignes avec des valeurs manquantes
data <- data[!is.na(data$X) & !is.na(data$Y), ]
# print(table(is.na(data$X)))
# print(table(is.na(data$Y)))

print("lignes trop vides")
print(nrow(data))
#supprime les lignes où il y a + de 12 valeurs NA 
# data <- data[rowSums(is.na(data)) < 13, ]
print(nrow(data))

# data <- data[!duplicated(data[, c("X", "Y", "fk_arb_etat")]), ]

"
Verification de si on a des NA dans OBJECTID
"

print(table(is.na(data$OBJECTID)))
#on a pas de NA dans OBJECTID


"
Nettoyage colonne created_date
Si il n'y a pas de created_date, on supprime la ligne
"
# on observe deja les valeurs manquantes dans les colonnes created_date
# print("-----------les dates---------")
# print(table(is.na(data$created_date)))
# print(table(data$created_date))
#remplacer les valeurs manquantes par la moyenne de la created_date située avant et après
for (x in 2:length(data$created_date)) {
    if (is.na(data$created_date[x])) {
      if (!is.na(data$created_date[x-1])){
        data$created_date[x] <- data$created_date[x-1]
      } else if (!is.na(data$created_date[x+1])){
        data$created_date[x] <- data$created_date[x+1]
      } else {
        for (y in x:length(data$created_date)) {
          if (!is.na(data$created_date[y])) {
            data$created_date[x] <- data$created_date[y]
            break
          }
        }
      }
    }
}
# print("\n")
# print (table(data$created_date))
# print("\n voila maitnenant les dates na possible")
# print(table(is.na(data$created_date)))
#okok
#revoir le format des dates car bon il n'est pas fou




"
Nettoyage colonne src_geo
"

data$src_geo <- "orthophoto"


"
nettoyage colonne created_user

"

#il n'y a pas de valeurs manquantes il faut donc juste modifier les espace dans la phrase par des .
data$created_user <- gsub(" ", ".", data$created_user)
print(table(data$created_user))
#quand on ne sait pas par qui ça a été créé on met rien

"
netttoyage colonne clc_quartier
"
print("quartiers")
print(table(data$clc_quartier))
# affiche les NA
print(table(is.na(data$clc_quartier)))
#pour ce qui est des valeurs manquantes je vais chercher via les valeurs x et y du quartier manquant le quartier ayant les valeurs x et y les plus proches

# Calculer la distance géographique entre deux points
distance_geo <- function(x1, y1, x2, y2) {
    return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

# Imputation des valeurs manquantes dans clc_quartier en utilisant la distance géographique
impute_clc_quartier <- function(data) {
    for (i in 1:nrow(data)) {
        if (is.na(data$clc_quartier[i])) {
            min_dist <- Inf
            closest_quartier <- NA
            for (j in 1:nrow(data)) {
                if (!is.na(data$clc_quartier[j]) && i != j) {
                    dist <- distance_geo(data$X[i], data$Y[i], data$X[j], data$Y[j])
                    if (dist < min_dist) {
                        min_dist <- dist
                        closest_quartier <- data$clc_quartier[j]
                    }
                }
            }
            # Si la distance minimale est supérieure à 500, on met "quartier inconnu"
            if (min_dist > 200) {
                closest_quartier <- "quartier inconnu"
            }
            data$clc_quartier[i] <- closest_quartier
        }
    }
    return(data)
}

# Appliquer la fonction d'imputation
data <- impute_clc_quartier(data)

print("Vérification des valeurs manquantes dans clc_quartier après imputation")
# Afficher le tableau de fréquence de la colonne clc_quartier après le nettoyage
print(table(data$clc_quartier))
print("\n")
print(table(is.na(data$clc_quartier)))

"
netttoyage colonne clc_secteur
"
print("------nos secteur--------")
#print(table(data$clc_secteur))
print(table(is.na(data$clc_secteur)))
#sert a rien de le nettoyer car il n'y a pas de valeurs manquantes

"
netttoyage colonne id_arbre
"
# print("nombre de valeurs manquantes dans id_arbre avant nettoyage")
# print(table(is.na(data$id_arbre)))
# remplacer les valeurs manquantes par l'id de l'arbre précédent + 1
for (x in 2:length(data$id_arbre)) {
    if (is.na(data$id_arbre[x])) {
      if (!is.na(data$id_arbre[x-1])){
        data$id_arbre[x] <- data$id_arbre[x-1] + 1
      } else if (!is.na(data$id_arbre[x+1])){
        data$id_arbre[x] <- data$id_arbre[x+1] - 1
      } else {
        for (y in x:length(data$id_arbre)) {
          if (!is.na(data$id_arbre[y])) {
            data$id_arbre[x] <- data$id_arbre[y] + 1
            break
          }
        }
      }
    }
}
# print("nombre de valeurs manquantes dans id_arbre après nettoyage")
# print(table(is.na(data$id_arbre)))
"
Nettoyage de la colonne 'feuillage'
    - Remplacer proportionnellement les valeurs manquantes par 'Conifère' et 'Feuillu'
"
# print(table(data$feuillage))
# feuillage <- function(data){
#     total_coniferes <- sum(data$feuillage == "conifère")
#     total_feuillus <- sum(data$feuillage == "feuillu")
#     total <- total_coniferes + total_feuillus

#     prop_coniferes <- total_coniferes / total
#     prop_feuillus <- total_feuillus / total

#     # print(prop_coniferes)
#     # print(prop_feuillus) 

#     set.seed(123) 
#     data$feuillage <- ifelse(data$feuillage == "",
#                             sample(c("conifère", "feuillu"), size = sum(data$feuillage == ""), replace = TRUE, prob = c(prop_coniferes, prop_feuillus)),
#                             data$feuillage)
#     return(data)
# }

# data=feuillage(data)
# print(table(data$feuillage))

data$feuillage[is.na(data$feuillage)] <- "Inconnu"
print("aaaaaaaa")
print(table(data$feuillage))

"
Nettoyage de la colonne 'remarquable'
    - Remplacer les valeurs manquantes par la valeur la plus fréquente
    - Remplacer les valeurs 'Oui' par TRUE
    - Remplacer les valeurs 'Non' par FALSE
    - Remplacer les valeurs vides par FALSE
    - Afficher le tableau de fréquence de la colonne 'remarquable
"

#print(table(data$remarquable))

remarquable <- function(data){
    data$remarquable[data$remarquable == "oui"] <- TRUE
    data$remarquable[data$remarquable == "non"] <- FALSE
    data$remarquable[is.na(data$remarquable)] <- FALSE
    # met en type booleen
    data$remarquable <- as.logical(data$remarquable)
    return(data)
}
data = remarquable(data)



#plot(data$X, data$Y)
# print(head(data))
# View(data)
# print(table(data$remarquable))


#remplace les valeurs NA de haut_tot par la valeur medianne des valeurs de haut_tot
data$haut_tot[is.na(data$haut_tot)] <- median(data$haut_tot, na.rm = TRUE)

#print(data[data$tronc_diam == 0, c("fk_arb_etat", "tronc_diam")])

#remplace les valeurs NA de tronc_diam par la valeur medianne des valeurs de tronc_diam
data$tronc_diam[is.na(data$tronc_diam)] <- median(data$tronc_diam, na.rm = TRUE)
#print(data[data$tronc_diam == 0, c("fk_arb_etat", "tronc_diam")])

#remplace les valeurs NA de haut_tronc par la valeur medianne des valeurs de haut_tronc
data$haut_tronc[is.na(data$haut_tronc)] <- median(data$haut_tronc, na.rm = TRUE)
#print(data[data$haut_tronc == 0, c("fk_arb_etat", "haut_tronc")])

"
Nettoyage de la colonne 'fk_arb_etat'
"
print("------nos etats--------")
print(table(is.na(data$fk_arb_etat)))
#pas de valeurs manquantes

"
Nettoyage de la colonne 'age_estim'
"



View(data)


'
---------------------------------------------------------------------------------
Affichages de toutes les cellules vides
'
# na_i <- which(is.na(data), arr.ind = TRUE)
# na_data <- data.frame(Ligne = na_i[, 1], Colonne = colnames(data)[na_i[, 2]])
# # print(na_data)
# unique_na <- unique(na_data$Colonne)
# print(unique_na)



print(summary(data$OBJECTID))

print(table(data$created_user))


# print(head(data))

# View(data)

# write_csv(df, "votre_fichier_modifie.csv")

# # Statistique descriptive univariée
# print(summary(data))

# # Histogramme de la hauteur totale
# hist(data$haut_tot)

# # Boxplot du diamètre du tronc
# boxplot(data$tronc_diam)

# # Boxplot de la hauteur totale par quartier
# boxplot(haut_tot ~ clc_quartier, data = data)

# # Distribution des arbres par quartier
# barplot(table(data$clc_quartier))

# # Répartition des types de feuillage
# pie(table(data$feuillage))

# # Fréquence des variables catégorielles
# categorical_columns <- c("created_user", "src_geo", "clc_quartier", "clc_secteur", 
#                          "fk_arb_etat", "fk_stadedev", "fk_port", "fk_pied", 
#                          "fk_situation", "fk_revetement", "commentaire_environnement", 
#                          "fk_prec_estim", "fk_nomtech", "last_edited_user", 
#                          "villeca", "nomfrancais", "nomlatin", "Creator", 
#                          "Editor", "feuillage", "remarquable")
# for (col in categorical_columns) {
#   cat("\nFréquence de la variable:", col)
#   print(table(data[[col]]))
# }

'
---------------------------------------------------------------------------------
Visualisation des données sur des graphiques
'

# Installation des bibliothèques
# install.packages("sf")
# install.packages("leaflet")
# install.packages("ggplot2")
# install.packages("dplyr")
# Chargement des bibliothèques



#Graphique de répartition des arbres par stade de développement
ggplot(data = data, aes(x = fk_stadedev)) +
    geom_bar(fill = "red") +
    labs(title = "Répartition des arbres suivant leur stade de développement", 
        x = "Stade de développement", 
        y = "Nombre d'arbres") +
    theme_minimal()     # Fond blanc quadrillé

# Histogramme de la quantité d'arbres par quartier
ggplot(data = data, aes(x = clc_quartier)) +
    geom_bar(fill = "orange") +
    labs(title = "Quantité d'arbres par quartier",
        x = "Quartier",
        y = "Nombre d'arbres") +
    theme_minimal() +   # Fond blanc quadrillé
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogramme de la quantité d'arbres par secteur
ggplot(data = data, aes(x = clc_secteur)) +
    geom_bar(fill = "yellow") +
    labs(title = "Quantité d'arbres par secteur",
          x = "Secteur",
          y = "Nombre d'arbres") +
    theme_minimal() +     # Fond blanc quadrillé
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogramme de la quantité d'arbres par situation
ggplot(data = data, aes(x = fk_situation)) +
    geom_bar(fill = "green") +
    labs(title = "Quantité d'arbres par situation",
          x = "Situation",
          y = "Nombre d'arbres") +
    theme_minimal() +     # Fond blanc quadrillé
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


'
---------------------------------------------------------------------------------
Visualisation des données sur une carte
'

# Conversion des données en objet spatial sf
# Transformation des coordonnées de EPSG:3949 à EPSG:4326
tree_sf <- st_as_sf(data, coords = c("X", "Y"), crs = 3949)
tree_sf_transfo <- st_transform(tree_sf, crs = 4326)

# Extraire les coordonnées transformées
tree_transfo <- cbind(data, st_coordinates(tree_sf_transfo))
names(tree_transfo)[(ncol(tree_transfo)-1):ncol(tree_transfo)] <- c("Longitude", "Latitude")

# Création d'une palette de couleurs
n <- length(unique(data$clc_quartier))
palette <- colorFactor(palette = brewer.pal(n, "Set3"), domain = data$clc_quartier)

# Visualisation sur une carte de Saint Quentin
carte <- leaflet(tree_transfo) %>% addTiles() %>% 
    addCircleMarkers(~Longitude, ~Latitude, color = ~palette(clc_quartier), popup = ~paste("ID:", OBJECTID), radius = 1) %>%
    addLegend(pal = palette, values = ~clc_quartier, title = "Quartiers", position = "bottomright")
print(carte)

# Graphique de la quantité d'arbres par quartier
graph1 <- ggplot(data = tree_transfo, aes(x = clc_quartier)) +
    geom_bar(fill = "blue") +
    labs(title = "Quantité d'arbres par quartier",
        x = "Quartier",
        y = "Nombre d'arbres") +
    theme_minimal()       # Fond blanc quadrillé
print(graph1)

# Graphique de la quantité d'arbres par secteur
graph2 <- ggplot(data = tree_transfo, aes(x = clc_secteur)) +
    geom_bar(fill = "blue") +
    labs(title = "Quantité d'arbres par secteur",
        x = "Secteur",
        y = "Nombre d'arbres") +
    theme_minimal()       # Fond blanc quadrillé
print(graph2)