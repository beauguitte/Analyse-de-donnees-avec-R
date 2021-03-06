#L. Beauguitte, septembre 2015
#Initiation à R

#tous les commentaires commmençant par Ex. : sont des exercices à réaliser vous-même
#Attention : des erreurs de code simples (parenthèse ou guillemet ouvert
#et non refermé, virgule manquante etc.) sont présentes dans le script...

#manipulation de tableaux
rm(list = ls())
insee <- read.table("insee76.txt")
vote <- read.table("data76.txt")

#Ex. : sélectionner les 10 premières colonnes et les lignes 1 à 100 du tableau insee
#Ex. : sélectionner les 10 premières colonnes et les lignes 50 à 150 du tableau data

insee2 <- 
vote2 <- 

#jointure en conservant tous les éléments
join <- merge(insee2, vote2, by.x = "BVCOM", by.y = "bvcom", all.x = TRUE, ally = TRUE)
head(join, 4)

#ne conserver que les éléments communs
join2 <- merge(insee2 vote2, by.x = "BVCOM", by.y = "bvcom", all.x = FALSE, all.y = FALSE)
head(join2)

#possibilité d'utiliser du SQL  package sqldf 
#sqldf("instructions en sql")

#exporter le tableau modifié en format .txt

#les boucles - nombre d'itérations connu
for (i in 1:5){
  print (i)
}

#si nombre d'itérations non connu, utiliser while
#générer un entier  entre 10 et 30
n <- round(runif(1, min=10, max=30), 0) #génére de façon aléatoire un nombre entre 10 et 30

i <- 0
while (i< n){
  print(i)
  i <- i + 1
}

#créer une fonction nom <- function(argument){liste de commandes}
#renvoie l'écart-type (racine carrée de la variance)

ecarttype <- function(x){
  ecarttyp <- sqrt(sd(x))
  return (ecarttyp)
}

data <- c(1, 2, 2.1, 4, 7, 3, 2)
ecarttype(data)

########################################
#visualisations ########################
########################################
rm(list=ls())

#importer le fichier insee76.txt
d <- 
  
#pourcentage de mineur-e-s par unité spatiale  
d$MIN <- 100 * d$POP0017 / d$POP

#nuages de points
#plus il y a de jeunes, moins il y a de vieux...
plot(d$MIN, d$TX65)

#histogramme
hist(d$TX65)

#Ex. : ajouter un titre, un label pour les abscisses et les ordonnées
#changer les couleurs et le nombre des barres

#variable centrée- réduite
d$TX65c <- scale(d$TX65)

#Ex. faire l'histogramme de d$X65c avec comme bornes min, moyenne - 1 écart-type,
#moyenne - 0.5 et, moyenne, moy + 0.5, moy + 1, max

#boîte à moustacle
boxplot(d$TX65)

#Ex. : ajouter un titre
#changer les couleurs

#exporter une figure en format vectoriel
#postscript("boxplot.ps")

#créer une figure directement dans un pdf
#pdf("boxlot.pdf")
#boxplot(d$ONENIGHT)
#dev.off()

#faire varier l'affichage
#par(mfrow = c(nombre_de_lignes, nombre_de_colonnes))

par(mfrow=c(1,2))

boxplot(d$TX65c)
hist(d$MIN)

#retour aux paramètres graphiques par défaut
dev.off()

#visualisations interactives
#chargement des packages
library(manipulate)

manipulate(hist(x = d[[myVar]], breaks = nbBreaks),
           nbBreaks = slider (min = 2, max = 20),
           myVar = picker("TX65", "MIN", "POPc", "TX65c"))

#pour visualisation interactive plus riche : 
#H. Commenges et R. Cura, 2015, http://elementr.hypotheses.org/232

