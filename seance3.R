#Statistiques bivariées et multivariées avec R
#L. Beauguitte, septembre 2015

#Attention : des erreurs de code simples (parenthèse ou guillemet ouvert
#et non refermé, virgule manquante etc.) sont présentes dans le script...

#import du fichier 
d <- read.table("eurosex.txt")

#examen du fichier
str(d)

#relation entre 2 variables quantitatives : corrélation et régression linéaire

#relation entre toutes les variables
plot(d)

#à modifier pour ne garder que les variables numériques
plot(d[, -c(1:2)])

#poser une hypothèse : les expériences de triolisme favorisent les relations d'un soir
#visualiser la relation supposée (indispensable !)
plot(d$THREEBED, d$ONENIGHT, pch = 3, col = "blue")

#avec titre, libellés des axes
plot(d$THREEBED, d$ONENIGHT, pch = 3, 
     col = "blue", 
     main = "Titre", 
     xlab = "Trio" 
     ylab = "Un soir")

#corrélation
COR <- cor.test(d$THREEBED, d$ONENIGHT) #création d'une liste
COR   #corrélation élevée (0.61), significativé forte (p-value)
str(COR)                                #éléments de la liste
COR$p.value                             #accès à un élément précis

#pour obtenir toutes les corrélations entre variables
cor(d[, -c(1:2)])

#régression linéaire : lm(variable à expliquer ~ variable explicative)
REG <- lm(THREEBED ~ ONENIGHT, data = d)
summary(REG)  #variable significative au seuil de 1 pour 100 (**)
              #r2 ajusté médiocre (0.34)

#éléments de l'objet REG
str(REG)

#contrôle des résidus
par(mfrow = c(2,2)) #division fenêtre graphique en 2 lignes et 2 colonnes
plot(REG)
dev.off()

#Residuals vs Fitted : permet contrôle homosédasticité des résidus
#Normal Q-Q : contrôle normalité distribution des résidus
#Scale location : homosédastiité des résidus standardisés
#Residuals vs leverage : existence valeurs extrêmes

#y = ax + b
coefficients <- coef(REG)
str(coefficients)

#représentation de la relation avec droite régression, r2 et équation
plot(d$ONENIGHT, d$THREEBED) 
abline(REG, col = "red")
legend("topright", legend =paste("R²=", round(summary(REG)$r.squared, 2)))
legend("bottomright", paste("y = ", round(coefficients[2], 2), "x +", round(coefficients[1], 2)))

#ajouter les résidus au tableau de départ
d$residus <- REG$residuals

#relation entre deux variables qualitatives
#test du chi2
rm(list=ls())
? chisq.test()

d <- as.data.frame(rbind(c(32, 12), c(20,24)))

chisq.test(d) #p-value = 0.01 - HO peut être rejetée avec une marge d'erreur de 1%

#pour examiner les éléments du test, créer un objet
ch2 <- chisq.test(d)
str(ch2)

#distribution théorique si le contenu des cases dépendait uniquement des sommes marginales
ch2$expected

#résidus
ch2$residuals

#résidus standardisés
ch2$stdres

#en cas d'effectifs trop faibles, message d'alerte mais test réalisé
d <- as.data.frame(rbind(c(2, 3), c(20,24)))
chisq.test(d)

#Ex. : y-a-t'il plus d'abstention dans le 20 que dans le 16° (election 2015)
#création d'un tableau de données
d <- as.data.frame(rbind(c(16, 42925, 823, 47741, 91489),
                         c(20, 50712, 1703, 54966, 107381)))
names(d) <- c("Arr", "Abstentions","Blancs et nuls", "Exprimés","Total")

#relation entre une variable qualitative et une variable quantitative
#analyse de la variance aov ou lm puis anova : résultats identiques
rm(list = ls())

#3 enseignants, 3 séries de 8 notes
d1 <- as.data.frame(cbind(c(5, 8, 12, 12, 8, 14, 7, 13)))
d1$E <- "E1"                        
d2 <-  as.data.frame(cbind(c(4, 9, 13, 11, 7, 15, 6, 12)))
d2$E <- "E2"
d3 <-  as.data.frame(cbind(c(3, 7, 15, 8, 4, 16, 4, 15))
d3$E <- "E3"

d <- rbind(d1,d2,d3)

#Option 1 : aov
aov1 <- aov(d$V1 ~ d$E)
summary(aov1)

#Df : degrés de liberté, Sum Sq : somme des carrés des écarts
#Mean Sq: moyenne de carrés des écarts, Pr(>F) p-value associée au test de Fisher
#il y a 96% de chances que les notes différent selon les enseignants

#Option 2 : lm puis anova
aov2 <- lm(d$V1 ~ d$E)
anova(aov2)

#mêmes types de graphiques que pour régression linéaire
par(mfrow= c(2,2))
plot(aov1) 
dev.off()

#lecture plus simple sur boîtes à moustache
boxplot(residuals(aov2) ~ d$E,
        col = "grey",
        names = c("E1", "E2", "E3"),
        ylab = "Résidus du modèle",
        xlab = "Enseignant")

#statistiques multivariées
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

#importer le fichier PTS_2011.csv
d <-
names(d)

#quelles sont les différentes modalités de la variable Income ?
#calculer le score moyen AI et US selon le niveau de revenu moyen (fonction aggregate)

#le score AI et le score du département d'État US sont-ils corrélés ?

#hypothèse : le département d'Etat s'inspire des rapports d'AI : 
#faire la régression linéaire et étudier la distribution des résidus
