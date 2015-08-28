#L. Beauguitte, septembre 2015
#Initiation à R
#Principes généraux

#tous les commentaires commmençant par Ex. : sont des exercices à réaliser vous-même

#initialiser (nettoyer espace de travail)
rm(list=ls())

#R comme calculatrice...
#les scripts et les commentaires

#R base et les packages
#installer un package 
#install.packages("nom_du_package", dependencies = TRUE)

#Ex. : installez reshape2

#charger un package
library("reshape2")

#citer un package dans une publication
citation("reshape2")

#connaître les jeux de données contenus dans un package
data(package = "MASS")

#packages chargés
(.packages()) 

#emplacement du répertoire de travail
getwd()

#liste des fichiers du répertoire
list.files()

#les rubriques d'aides
#aide pour une fonction
?plot

#aide pour un caractère spécial
help("^")

#pour consulter toutes les fonctions d'un package
library(help = "reshape2")

#operateurs
# + - / * 
# ^ ou ** = exposant
2^3
2**3

#R comme calculatrice
2 + 5

#création d'objet
a <- 2.3 + 5
a

#classe
class(a)

#description détaillée
str(a)

#numeric, character et logical
a <- 7.3
b <- "bob"
c <- TRUE

class(a)
class(b)
class(c)

#transformation possible
a <- as.character(a)
class(a)

#transformation non possible et réalisée
b <- as.numeric(b)
b #NA = Non Available (données manquantes)

#espaces ne comptent pas, la casse si...
a     <-         2
a
A

#gérer les arrondis
exem <- c(3,1,1)
mean(exem)
round(mean(exem), 2)

#vecteur : ensemble de données de même nature créé avec opérateur c
v1 <- c(1,3,4,2)

#possibilité de combiner des vecteurs
v2 <- c(0,2,3,1)

v1*v2

#s'ils sont de même longueur
v3 <- c(0,2,1)
v1*v3

#mais attention si la longueur de l'un est un multiple de l'autre - pas de message d'erreur
v4 <- c(0,1)
v1*v4

#position dans un vecteur (indexation) nomduvecteur[position] - 
#commence à 1
v1[1]
v1[4]
v1[5] 

#si [a:b] : renvoie les éléments de a à b inclus
v1[1:3]

#fonction : prend des arguments et retourne un résultat
length(v1)

#le résultat d'une fonction peut être stocké
lv1 <- length(v1)

s#charger un tableau de données, examiner l'objet
#options de base : header, sep, dec 
#préférer les formats .txt ou .csv aux formats tableur (.xls, .xlsx, .ods etc.)
?read.table

#read.table
rm(list=ls())

#Ex. : changez les chemins d'accès... puis importer le fichier insee76.txt
d <- 
class(d)
str(d)

#nombre de lignes, de colonnes et dimension
nrow(d)
ncol(d)
dim(d)

#nom de colonnes
names(d)

#premières et dernières lignes
head(d,5)
tail(d,5)

#modalités variables (factor)
levels(d$EMPAID)

levels(as.factor(d$EMPAID))

#trier un tableau
d <- d[order(d[,2],decreasing=TRUE),] #,2 indique que le tri se fait sur la colonne 2
head(d,4)

#description statistique d'une variable numérique
summary(d$POP)

#du tableau entier
summary(d)

#moyenne - ne sait pas géner les données manquantes par défaut
mean(d$LOCHLM)
mean(d$LOCHLM, na.rm = TRUE)

#idem avec min, max, sd (standard deviation) et var

#tri à plat
table(d$GRATUIT)

#tri ordonné
sort(table(d$GRATUIT), decreasing = TRUE)

#sélection de lignes, de colonnes
#1:10 signifie de 1 à 10
d2 <- d[1:3, 1:2] #lignes 1 à 3 et colonnes 1 à 2
d3 <- d[, 1:3] #toutes les lignes et colonnes 1 à 3
d4 <- d[-3,] #toutes les lignes sauf la troisième et toutes les colonnes
d5 <- d[1:3, -c(2, 6, 8)] #lignes 1 à 3, toutes les colonnes sauf la 2, 6 et 8

#opérateurs
# > < >= <= == !=
# & (et) | (ou)

#sélection de données
dPOP500 <- d[d$POP >= 500,]

#équivalent
dPOP5002 <- subset(d, POP >= 500)

#création d'une variable commune : extraire 5 premiers caractères de BVCOM
d$COM <- substr(d$BVCOM, 1,5) #chaine à traiter, premier élément, dernier élément
str(d$COM)                    #crée une variable de type character

#Ex. Sélectionnez les données correspondants à Rouen (76540)

drouen <- 

#changer le nom d'une colonne
names(drouen)[2] <- "POPTOTALE"
names(drouen)

#Ex. créer une variable donnant le pourcentage de plus de 65 ans par bureau de vote
drouen$TX65 <- 

summary(drouen$TX65)

#créer un typologie sur taux 65 ans et +

drouen$TYPO <- cut(drouen$TX65, c(8,14.3,23.1)) # ] valeur incluse
levels(drouen$TYPO)

#avec des labels propres...
drouen$TYPO <- cut(drouen$TX65, c(8,14.3,23.1), 
              labels = c("de 8 à 14.3", "de 14.3 à 23.1")) 

#autre option
drouen$TYPO2[drouen$TX65 <= 14.3] <- "de 8 à 14.3"
drouen$TYPO2[drouen$TX65 > 14.3] <- "de 14.3 à 23.1"

#autre option
drouen$TYPO3 <- ifelse(drouen$TX65 <= 14.3 ,"de 8 à 14.3", "de 14.3 à 23.1")
head(drouen, 5)
