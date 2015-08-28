#Statistiques bivariées et multivariées avec R
#L. Beauguitte, septembre 2015

#package ade4
#acp (variables quantitatives), afc tableau de contingence
#acm tableau disjonctif complet
rm(list = ls())

library(ade4)
d <- read.table("eurosex.txt")
rownames(d) <- as.character(d$COUNTRYID)

#se débarasser des valeurs manquantes
d <- na.omit(d)

#acp sur variables centrées réduites
?dudi.pca  #par défaut, center = TRUE, scale = TRUE
acp <- dudi.pca(d[,3:8]) #très forte hiérarchie, 2 premiers axes suffisent

str(acp) 
#c : colonnes (variables) -> c1 : position des individus dans le premier plan factoriel
#l : individus (lignes)

#valeurs propres (variances de chaque composante = inertie totale prise en charge) :
acp$eig

#valeurs propres cumulées
cumsum(acp$eig)  #pourquoi la somme est-elle égale à 6 ?...

#en pourcentage
100 * acp$eig / sum(acp$eig)

#sous forme graphique
inertie  <-acp$eig/sum(acp$eig)*100
barplot(inertie, ylab = "% d'inertie", names.arg=round(inertie,2))
title("Eboulis des valeurs propres en %")

#coordonnées
cl1<-acp$li[,1] #pour les individus
cc1<-acp$co[,1] #pour les variables
cl2<-acp$li[,2] #pour les individus
cc2<-acp$co[,2] #pour les variables

#visualisation des variables
plot(cc1,cc2,type="n", main="Pratiques sexuelles en Europe")
abline(h=0,v=0)
text(cc1,cc2,row.names(acp$co), cex = 0.7)

#visualisation des individus
plot(cl1,cl2,type="n", main="Pratiques sexuelles en Europe")
abline(h=0,v=0)
text(cl1,cl2,row.names(acp$li), cex = 0.7)

#autre option : variables
s.corcircle(acp$co)

#autre option : individus
s.class(acp$li, fac = as.factor(d$COUNTRYID))

#contributions des variables et des individus
contri <- inertia.dudi(acp, row.inertia = TRUE, col.inertia = TRUE)
contriVar <- contri$col.abs #exprimée en 1/10 000
contriInd <- contri$row.abs #exprimée en 1/10 000

contriVar #axe 1 combinaison de facteurs, axe 2 fréquence
contriInd #rôle déterminant des pays nordiques axe2, Allemagne et GB pour le 2

#pour faciliter l'interprétation
qualVar <- contri$col.rel
barplot(abs(qualVar[,1]/10000),
        col = "black",
        border = "white",
        horiz = TRUE,
        las = 1,
        xlim = c(0,1))

qualInd <- contri$row.rel
barplot(abs(qualInd[,1]/10000),
        col = "black",
        border = "white",
        horiz = TRUE,
        las = 1,
        xlim = c(0,1))

#afc
#library(ade4)
#fonction dudi.coa -  mêmes principes

#acm 
#library(ade4) fonction dudi.acm  - mêmes principes

#classification ascendante hiérarchique (Agglomerative nesting)
library(cluster)

#choisir une distance et une méthode d'agrégation
cah <- agnes(d[,3:8],
             metric = "euclidean",
             method = "ward")

str(cah)

#diagramme de niveaux
tri <- sort(cah$height, decreasing = TRUE)
plot(tri, type = "h", xlab = "Noeuds",
     ylab = "Niveau d'agrégation")  #repérer les sauts pour choisir nombre de classes

#choix plus solide du nombre de classes
library(NbClust)
NbClust(d[,3:8], distance ="euclidean", method = "ward.D")

#dendogramme
pltree(cah, main ="Dendogramme", labels = d$COUNTRYNAME)

#couper l'arbre et profils des classes
clu3 <- cutree(cah, k = 3)

#ajout au tableau de départ
d$clu3 <- factor(clu3, levels = 1:3, labels = paste("CLUS", 1:3))

#calcul de la moyenne des variables pour chacune des classes
profil <- aggregate(d[,3:8], by = list(d$clu3), mean)
profil

#visualisation des profils de classes (données)
library(reshape2) #manipulation de données
library(ggplot2)  #visualisation - syntaxe spécifique

colnames(profil)[1] <- "CLUSTER"
proflong <- melt(profil, id.vars = "CLUSTER")
ggplot(proflong) +
  geom_bar(aes(x = variable, y = value, fill = CLUSTER),
           stat = "identity") +
  scale_fill_grey() +
  facet_wrap(~ CLUSTER) +
  coord_flip() + theme_bw()

#Exercice
rm(list=ls())

#charger le fichier diplome2011.csv

#réaliser une acp (sur les pourcentages seulement) et projeter les individus sur premier plan factoriel
#quel est le problème ? contrôler avec la contribution des individus

#supprimer l'individu aberrant puis réaliser acp et CAH
