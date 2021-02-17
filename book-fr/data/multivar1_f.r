# Auteurs: Amanda Winegardner, Xavier Giroux-Bougard, Bérenger Bourgeois, Emmanuelle Chrétien et Monica Granados
# Date: Février 2016
# Description: Ordination et Analyses canoniques
# Jeux de données: fichiers "DoubsSpe.csv", "DoubsEnv.csv"
# Notes: Le matériel utilisé est issu de 
#        Borcard D., Gillet F. et Legendre P., 2011. Numerical Ecology with R. Springer.
#***************************************************************************************#

# Chargement des données ####
rm(list=ls())
install.packages("vegan")
install.packages("gclus")
install.packages("ape")
library(vegan)
library(gclus)
library(ape)
source(file.choose()) # fichier coldiss.R

# Fichier d'abondance des espèces de poissons: "DoubsSpe.csv"
spe<-read.csv(file.choose(), row.names=1)
spe=spe[-8,] # le site 8 ne contient aucune espèce et doit être enlevé

# Fichier de variables environnementales: "DoubsEnv.csv"
env<-read.csv(file.choose(), row.names=1)
env=env[-8,]

#***************************************************************************************#

# 1.Exploration des données ####
## 1.1.Esp?ces
names(spe)
dim(spe)
str(spe)
head(spe)
summary(spe) 

### Distribution des espèces, toutes espèces confondues
(ab<-table(unlist(spe)))
barplot(ab, las=1, xlab="Abundance class", ylab="Frequency", col=grey(5:0/5))

### Nombre d'absences
sum(spe==0)

### Propotion de zéro dans le fichier espèces
sum(spe==0)/(nrow(spe)*ncol(spe))

### Fréquence des espèces
spe.pres<-colSums(spe>0) # calcule le nombre de sites où chaque espèces est présente
hist(spe.pres, main="Species occurence", las=1, xlab="Frequency of occurences", ylab="Number of species", breaks=seq(0,30, by=5), col="grey")

### Diversité spécifique
site.pres<-rowSums(spe>0) # calcule le nombre d'espèces dans chaque site
hist(site.pres, main="Species richness", las=1, xlab="Frequency of sites", ylab="Number of species", breaks=seq(0,30, by=5), col="grey")

### Indices de diversité
?diversity
(H<-diversity(spe, index="shannon")) # Indice de Shannon
(N<-diversity(spe, index="simpson")) # Indice de Simpson

## 1.2.Variables environnementales
names(env)
dim(env)
str(env)
head(env)
summary(env)
pairs(env, main="Bivariate Plots of the Environmental Data" )

# Standardisation des variables environnementales
env.z<-decostand(env, method="standardize")
apply(env.z, 2, mean) # centrage des donn?es (moyennes=0)
apply(env.z, 2, sd)   # r?duction des donn?es (?cart-types=1)

par(mfrow=c(1,2))
hist(env$oxy, main="Histogram of Oxygen Concentration", ylab="Frequency", ylim=c(0,10),  col="grey")
hist(env.z$oxy, main="Histogram of Standardized Oxygen Concentration", ylab="Frequency", ylim=c(0,10), col="grey")

# Note: les variables exprimées en différentes unités
#       doivent obligatoirement être standardisées avant de calculer des mesures de distance et d'effectuer des ordinations.

#***************************************************************************************#

# 2.Mesures d'association ####
## 2.1. Mesures de distance ####
### Données d'abondance d'espèces (quantitatives)
?vegdist # cette fonction calcule des indices de distances courramment utlisés pour l'analyse des communautés d'espèces 
(spe.db<-vegdist(spe, method="bray")) # indice de Bray-Curtis 
(spe.dj<-vegdist(spe, method="jac")) # indice de Jaccard 
(spe.dg<-vegdist(spe, method="gower")) # indice de Gower

### Données de présence-absence des espèces (binaire)
(spe.db.pa<-vegdist(spe, method="bray", binary=TRUE)) # indice de Bray-Curtis 
(spe.dj.pa<-vegdist(spe, method="jac", binary=TRUE)) # indice de Jaccard 
(spe.dg.pa<-vegdist(spe, method="gower", binary=TRUE)) # indice de Gower 

### Représentation graphique des matrices d'association: heat maps
windows()
coldiss(spe.db, byrank=FALSE, diag=TRUE) # Heat map de l'indice de Bray-Curtis 
windows()
coldiss(spe.dj, byrank=FALSE, diag=TRUE) # Heat map de l'indice de Jaccard 
windows() 
coldiss(spe.dg, byrank=FALSE, diag=TRUE) # Heat map de l'indice de Gower 

### Variables environnementales quantitatives
#### Association entre variables environnementales (Q mode)
?dist # cette fonction calcule aussi des matrices de distances
env.de<-dist(env.z, method = "euclidean") # matrice de distance euclidienne des variables environnemetales standardisées 
windows() 
coldiss(env.de, diag=TRUE) 

### Corrélations entre variables environnementales (R mode)
(env.pearson<-cor(env)) # coefficient r de corrélation linéaire de Pearson 
round(env.pearson, 2)
(env.ken<-cor(env, method="kendall")) # coefficient tau de corrélation de rang de Kendall 
round(env.ken, 2)

### Mélange de variables environnementales (quantitatives et qualitatives)
#### Association entre variables environnementales (Q mode)
##### Création d'un jeu de données fictif
var.g1<-rnorm(30, 0, 1)
var.g2<-runif(30, 0, 5)
var.g3<-gl(3, 10)
var.g4<-gl(2, 5, 30)
(dat2<-data.frame(var.g1, var.g2, var.g3, var.g4))
str(dat2)
summary(dat2)
##### Calcule de la matrice de distances de Gower
?daisy
(dat2.dg<-daisy(dat2, metric="gower"))
coldiss(dat2.dg)

# Défi 1 ####

# Calculer à la mitaine les distances de Bray-Curtis et de Gower pour les epèces CHA, TRU et VAI et les sites 1, 2 et 3
# (réponse ci-dessous)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# Distance de Bray-Curtis : d[jk] = (sum abs(x[ij]-x[ik]))/(sum (x[ij]+x[ik]))
(spe.challenge<-spe[1:3,1:3])
# Calcul de l'abondance totale en espèces pour chaque site
(Abund.s1<-sum(spe.challenge[1,]))
(Abund.s2<-sum(spe.challenge[2,]))
(Abund.s3<-sum(spe.challenge[3,]))
# Calcul des différences d'abondance entre paires de sites pour chaque espèce
Spec.s1s2<-0
Spec.s1s3<-0
Spec.s2s3<-0
for (i in 1:3) {
  Spec.s1s2<-Spec.s1s2+abs(sum(spe.challenge[1,i]-spe.challenge[2,i]))
  Spec.s1s3<-Spec.s1s3+abs(sum(spe.challenge[1,i]-spe.challenge[3,i]))
  Spec.s2s3<-Spec.s2s3+abs(sum(spe.challenge[2,i]-spe.challenge[3,i])) }
# Calcul de la distance de Bray-Curtis 
(db.s1s2<-Spec.s1s2/(Abund.s1+Abund.s2))
(db.s1s3<-Spec.s1s3/(Abund.s1+Abund.s3)) 
(db.s2s3<-Spec.s2s3/(Abund.s2+Abund.s3)) 
# Validation des résultats
(spe.db.challenge<-vegdist(spe.challenge, method="bray"))

# disance de Gower :    d[jk] = (1/M) sum(abs(x[ij]-x[ik])/(max(x[i])-min(x[i])))
# Calcul du nombre de colonnes dans le jeu de données
M<-ncol(spe.challenge)
# Calcul des différences d'abondance entre paires de sites pour chaque espèce
Spe1.s1s2<-abs(spe.challenge[1,1]-spe.challenge[2,1])
Spe2.s1s2<-abs(spe.challenge[1,2]-spe.challenge[2,2])
Spe3.s1s2<-abs(spe.challenge[1,3]-spe.challenge[2,3])
Spe1.s1s3<-abs(spe.challenge[1,1]-spe.challenge[3,1])
Spe2.s1s3<-abs(spe.challenge[1,2]-spe.challenge[3,2])
Spe3.s1s3<-abs(spe.challenge[1,3]-spe.challenge[3,3])
Spe1.s2s3<-abs(spe.challenge[2,1]-spe.challenge[3,1])
Spe2.s2s3<-abs(spe.challenge[2,2]-spe.challenge[3,2])
Spe3.s2s3<-abs(spe.challenge[2,3]-spe.challenge[3,3])
# Calcul de l'étendue d'abondance pour chaque espèce  
Range.spe1<-max(spe.challenge[,1]) - min (spe.challenge[,1])
Range.spe2<-max(spe.challenge[,2]) - min (spe.challenge[,2])
Range.spe3<-max(spe.challenge[,3]) - min (spe.challenge[,3])
# Calcul de la distance de Gower 
(dg.s1s2<-(1/M)*((Spe2.s1s2/Range.spe2)+(Spe3.s1s2/Range.spe3)))
(dg.s1s3<-(1/M)*((Spe2.s1s3/Range.spe2)+(Spe3.s1s3/Range.spe3)))
(dg.s2s3<-(1/M)*((Spe2.s2s3/Range.spe2)+(Spe3.s2s3/Range.spe3)))
# Validation des résultats
(spe.db.challenge<-vegdist(spe.challenge, method="gower"))


## 2.2. Transformations  des données de composition des communautés ####
?decostand # cette focntion fournit des méthodes usuelles de standardisation des données de composition des communautés 
# Transformation de données d'abondance en présence/absence (1/0)
(spe.pa<-decostand(spe, method="pa"))

# Transformation d'Hellinger 
(spe.hel<-decostand(spe, method="hellinger"))

# Transformation du Chi2
(spe.chi<-decostand(spe, method="chi.square"))


# Défi 2 ####

# Calculer à la mitaine les données d'abondance d'espèces avec transformation d'Hellinger et du Chi2
# (réponse ci-dessous)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#
# Transformation d'Hellinger 
# Calcul de l'abondance totale en espèces de chaque site
(site.totals<-apply(spe, 1, sum))
# Centrage des abondances d'espèces par division par l'abondance totale de chaque site
(scale.spe<-spe/site.totals)
# Calcul de la racine carrée de abondances centrées d'espèces
(sqrt.scale.spe<-sqrt(scale.spe))
# Validation des résultats
sqrt.scale.spe
spe.hel
sqrt.scale.spe-spe.hel # ou: sqrt.scale.spe/spe.hel

# Transformation du Chi2
# Calcul de l'abondance totale en espèces de chaque site
(site.totals<-apply(spe, 1, sum))
# Calcul de la racine carrée de la diversité de chaque site
(sqrt.spe.totals<-sqrt(apply(spe, 2, sum)))
# Centrage des abondances d'espèces par division par l'abondance totale de chaque site et par la racine carrée de la diversité de chaque site
scale.spe2<-spe
for (i in 1:nrow(spe)) {
  for (j in 1:ncol(spe)) {
    (scale.spe2[i,j]<-scale.spe2[i,j]/(site.totals[i]*sqrt.spe.totals[j]))   }}
# Ajustement des abondances centrées par multiplication par la racine carrée du total de la matrice d'abondances                                         
(adjust.scale.spe2=scale.spe2*sqrt(sum(rowSums(spe))))
# Validation des résultats
adjust.scale.spe2
spe.chi
adjust.scale.spe2-spe.chi # ou: adjust.scale.spe2/spe.chi



## 2.3 Groupement ####

### Calculer la distance de Hellinger
spe.dhel<-vegdist(spe.hel,method="euclidean") #crée une matrice de distances Hellinger à partir des données d’abondance transformées

# Pour voir la différence entre les deux types d’objets
head(spe.hel)# données d’abondances transformées Hellingerhead(spe.dhel)# matrice de distances de Hellinger entre les sites

### Comparaison du groupement à liens simples et à liens complets

# Faire le groupement à liens simples
spe.dhel.single<-hclust(spe.dhel, method=”single”)
plot(spe.dhel.single)

# Faire le groupement à liens complet
spe.dhel.complete<-hclust(spe.dhel, method=”complete”)
plot(spe.dhel.complete)

### Méthode de Ward

# Faire le groupement de Ward 
spe.dhel.ward<-hclust(spe.dhel, method=”ward.D2”)
plot(spe.dhel.ward)

# Refaire le dendrogramme en utilisant la racine carrée des distances
spe.dhel.ward$height<-sqrt(spe.dhel.ward$height)
plot(spe.dhel.ward)
plot(spe.dhel.ward, hang=-1) # hang=-1 permet d’afficher les objets sur la même ligne




#***************************************************************************************#

# 3.Ordination ####
## 3.1. Analyse en Composantes Principales (PCA) ####
### PCA sur les abondances d'espèces transform?es Hellinger
?rda
#### Calcul de la PCA
spe.h.pca<-rda(spe.hel)
# Identification des axes significatifs par le critère de Kaiser-Guttman
ev<-spe.h.pca$CA$eig
ev[ev>mean(ev)]
n<-length(ev)
bsm<-data.frame(j=seq(1:n), p=0)
bsm$p[1]=1/n
for (i in 2:n) {
  bsm$p[i]=bsm$p[i-1]+(1/(n=1-i))}
bsm$p=100*bsm$p/n
bsm
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red") 
legend("topright", "Average eigenvalues", lwd=1, col=2, bty="n")

#### Extraction des résultats
summary(spe.h.pca) # tous les résultats
summary(spe.h.pca)
summary(spe.h.pca, display=NULL) # seulement les valeurs propres et leur contribution à la variance
eigen(cov(spe.hel)) # méthode alternative de calcul des valeurs propres
(spe.scores<-scores(spe.h.pca, display="species", choices=c(1,2))) # scores des espèces sur les deux premiers axes de PCA
(sit.scores<-scores(spe.h.pca, display="sites", choices=c(1,2))) # scores des sites sur les deux premiers axes de PCA




### PCA sur les variables environnementales (standardisées)
#### Calcul de la PCA
env.pca<-rda(env.z) # ou rda(env, scale=TRUE)
# Identification des axes significatifs par le critère de Kaiser-Guttman
ev<-env.pca$CA$eig
ev[ev>mean(ev)]
n=length(ev)
bsm=data.frame(j=seq(1:n), p=0)
bsm$p[1]=1/n
for (i in 2:n) {
  bsm$p[i]=bsm$p[i-1]+(1/(n=1-i))}
bsm$p=100*bsm$p/n
bsm
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red") 
legend("topright", "Average eigenvalues", lwd=1, col=2, bty="n")
#### Extraction des r?sultats
summary(env.pca)
summary(env.pca, scaling=2)

### Création des biplots
#### Biplot de la PCA sur les abondances d'espèces transformées Hellinger (cadrage 1)
windows()
plot(spe.h.pca)
windows()
?biplot 
biplot(spe.h.pca)
windows()
plot(spe.h.pca, scaling=1, type="none", # scaling 1 = biplot de distance : 
     # les distances entre objets sur le biplot sont des approximations de leurs distances euclidiennes
     # mais les angle entre descripteurs NE reflètent PAS leur corrélation
     xlab=c("PC1 (%)", round((spe.h.pca$CA$eig[1]/sum(spe.h.pca$CA$eig))*100,2)),
     ylab=c("PC2 (%)", round((spe.h.pca$CA$eig[2]/sum(spe.h.pca$CA$eig))*100,2)))
points(scores(spe.h.pca, display="sites", choices=c(1,2), scaling=1),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(spe.h.pca, display="species", choices=c(1), scaling=1),
     scores(spe.h.pca, display="species", choices=c(2), scaling=1),
     labels=rownames(scores(spe.h.pca, display="species", scaling=1)),
     col="red", cex=0.8)       

# Ajouter des flèches pour les espèces
spe.sc<-scores(spe.h.pca,choices=1:2,scaling=,display="sp")
arrows(0,0,spe.sc[,1],spe.sc[,2],length=0)

#### Biplot de la PCA sur les variables environnementales (cadrage 2)
windows()
plot(env.pca)
windows()
plot(env.pca, scaling=2, type="none", # scaling 2 = biplot de correlation : 
     # les distances entre objets sur le biplot NE sont PAS des approximations de leurs distances euclidiennes
     # mais les angle entre descripteurs reflètent leur corrélation
     xlab=c("PC1 (%)", round((env.pca$CA$eig[1]/sum(env.pca$CA$eig))*100,2)),
     ylab=c("PC2 (%)", round((env.pca$CA$eig[2]/sum(env.pca$CA$eig))*100,2)),
     xlim=c(-1,1), ylim=c(-1,1))
points(scores(env.pca, display="sites", choices=c(1,2), scaling=2), 
       pch=21, col="black", bg="darkgreen", cex=1.2) 
text(scores(env.pca, display="species", choices=c(1), scaling=2),
     scores(env.pca, display="species", choices=c(2), scaling=2),
     labels=rownames(scores(env.pca, display="species", scaling=2)),
     col="red", cex=0.8)


#### Abondance de l'espèce TRU le long du gradient oligotrophe-eutrophe
Sites_scores_Env_Axis1<- scores(env.pca, display="sites", choices=c(1), scaling=2)
spe$ANG
plot( Sites_scores_Env_Axis1, spe$TRU)
summary(lm(spe$TRU~Sites_scores_Env_Axis1))
abline(lm(spe$TRU~Sites_scores_Env_Axis1))

# Défi 3 ####
# Effectuer la PCA sur les abondances d'espèces du fichier mites
# Quels sont les axes significatifs ?
# Quels groupes de sites pouvez-vous identifier ?
# Quelles espèces sont liées ? chaque groupe de sites ?
mite.spe<-mite # données provenant du package vegan
# (réponse ci-dessous)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#
# 
mite.spe.hel<-decostand(mite.spe, method="hellinger")
mite.spe.h.pca<-rda(mite.spe.hel)        
ev<-mite.spe.h.pca$CA$eig
ev[ev>mean(ev)]
n=length(ev)
bsm=data.frame(j=seq(1:n), p=0)
bsm$p[1]=1/n
for (i in 2:n) {
  bsm$p[i]=bsm$p[i-1]+(1/(n=1-i))}
bsm$p=100*bsm$p/n
bsm
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red") 
legend("topright", "Average eigenvalues", lwd=1, col=2, bty="n")       
summary(mite.spe.h.pca, display=NULL)
windows()
plot(mite.spe.h.pca, scaling=1, type="none", 
     xlab=c("PC1 (%)", round((mite.spe.h.pca$CA$eig[1]/sum(mite.spe.h.pca$CA$eig))*100,2)),
     ylab=c("PC2 (%)", round((mite.spe.h.pca$CA$eig[2]/sum(mite.spe.h.pca$CA$eig))*100,2)))
points(scores(mite.spe.h.pca, display="sites", choices=c(1,2), scaling=1),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(mite.spe.h.pca, display="species", choices=c(1), scaling=1),
     scores(mite.spe.h.pca, display="species", choices=c(2), scaling=1),
     labels=rownames(scores(mite.spe.h.pca, display="species", scaling=1)),
     col="red", cex=0.8)  



## 3.2. Analyse des Correpondances (CA) ####
?cca
### Executer la CA sur les abondances d'esp?ces
spe.ca<-cca(spe)

### Identifier les axes significatifs à l'aide du critère de Kaiser-Guttman 
ev<-spe.ca$CA$eig
ev[ev>mean(ev)]
n=length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red")
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

### Extraire les résultats
spe.ca
summary(spe.ca, display=NULL)

### Construire le biplot
par(mfrow=c(1,2))
#### cadrage 1
plot(spe.ca, scaling=1, type="none", main='CA - biplot cadrage 1',
     xlab=c("CA1 (%)", round((spe.ca$CA$eig[1]/sum(spe.ca$CA$eig))*100,2)),
     ylab=c("CA2 (%)", round((spe.ca$CA$eig[2]/sum(spe.ca$CA$eig))*100,2)))
points(scores(spe.ca, display="sites", choices=c(1,2), scaling=1),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(spe.ca, display="species", choices=c(1), scaling=1),
     scores(spe.ca, display="species", choices=c(2), scaling=1),
     labels=rownames(scores(spe.ca, display="species", scaling=1)),
     col="red", cex=0.8)

#### cadrage 2
plot(spe.ca, scaling=1, type="none", main='CA - biplot cadrage 2',
     xlab=c("CA1 (%)", round((spe.ca$CA$eig[1]/sum(spe.ca$CA$eig))*100,2)),
     ylab=c("CA2 (%)", round((spe.ca$CA$eig[2]/sum(spe.ca$CA$eig))*100,2)), ylim=c(-2,3))
points(scores(spe.ca, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(spe.ca, display="species", choices=c(1), scaling=2),
     scores(spe.ca, display="species", choices=c(2), scaling=2),
     labels=rownames(scores(spe.ca, display="species", scaling=2)),
     col="red", cex=0.8)



# Défi 4 ####
# Effectuer la CA sur les abondances d'espèces du fichier mites
# Quels sont les axes significatifs ?
# Quels groupes de sites pouvez-vous identifier ?
# Quelles espèces sont liées ? chaque groupe de sites ?
# (réponse ci-dessous)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
mite.spe.ca<-cca(mite.spe)
ev<-mite.spe.ca$CA$eig
ev[ev>mean(ev)]
n=length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red")
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
summary(mite.spe.ca, display=NULL)
windows()
plot(mite.spe.ca, scaling=1, type="none",
     xlab=c("PC1 (%)", round((mite.spe.ca$CA$eig[1]/sum(mite.spe.ca$CA$eig))*100,2)),
     ylab=c("PC2 (%)", round((mite.spe.ca$CA$eig[2]/sum(mite.spe.ca$CA$eig))*100,2)))
points(scores(mite.spe.ca, display="sites", choices=c(1,2), scaling=1),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(mite.spe.ca, display="species", choices=c(1), scaling=1),
     scores(mite.spe.ca, display="species", choices=c(2), scaling=1),
     labels=rownames(scores(mite.spe.ca, display="species", scaling=1)),
     col="red", cex=0.8)



## 3.3. Analyse en coordonnées principales (PCoA) ####
### PCoA sur les abondances d'espèces transformées Hellinger
?cmdscale
cmdscale(dist(spe.hel), k=(nrow(spe)-1), eig=TRUE)
?pcoa
spe.h.pcoa<-pcoa(dist(spe.hel))
### Extraction des résultats
spe.h.pcoa
### Création du biplot
windows()
biplot.pcoa(spe.h.pcoa, spe.hel, dir.axis2=-1)

### PCoA sur la matrice de distance de Bray-Curtis des abondances d'espèces transformées
spe.db
spe.bray.pcoa<-pcoa(spe.db)
spe.bray.pcoa
windows()
biplot.pcoa(spe.bray.pcoa, spe.hel, dir.axis2=-1)
# La mesure de distance choisie influence fortement les résultats de la PCoA

# Défi 4 ####
# Effectuer la PCoA sur les abondances d'espèces du fichier mites
# Quels sont les axes significatifs ?
# Quels groupes de sites pouvez-vous identifier ?
# Quelles espèces sont liées ? chaque groupe de sites ?
# Comment les résultats de la PCoA se comparent-ils avec ceux de la PCA ?
# (réponse ci-dessous)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#
# 
mite.spe.h.pcoa<-pcoa(dist(mite.spe.hel))
mite.spe.h.pcoa
windows()
biplot.pcoa(mite.spe.h.pcoa, mite.spe.hel, dir.axis2=-1)



## 3.4. Postitionnement multidimensionnel non-métrique (NMDS) ####
?metaMDS
### Executer le NMDS en deux dimensions sur les abondances d'espèces à l'aide d'une distance de Bray-Curtis 
spe.nmds<-metaMDS(spe, distance='bray', k=2) # l'argument k précise le nombre de dimensions choisies (i.e. le nombre d'axe d'ordination)

### Extraction des résultats
spe.nmds
spe.nmds$stress

### Evaluer la qualité de l'ajustement
spe.nmds$stress
stressplot(spe.nmds, main='Shepard plot')

### Construire le biplot
windows()
plot(spe.nmds, type="none", main=paste('NMDS/Bray - Stress=', round(spe.nmds$stress, 3)),
     xlab=c("NMDS1"),
     ylab=c("NMDS2"))
points(scores(spe.nmds, display="sites", choices=c(1,2)),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(spe.nmds, display="species", choices=c(1)),
     scores(spe.nmds, display="species", choices=c(2)),
     labels=rownames(scores(spe.nmds, display="species")),
     col="red", cex=0.8)

# Challenge 6 ####
# Exécuter un NMDS sur les données d'abondance des espèces d'acariens (données mite) 
# en deux dimensions à partir de distances de Bray-Curtis. 
# Évaluer la qualité de l'ajustement et interpréter le biplot.
# (réponse ci-dessous)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
mite.spe.nmds<-metaMDS(mite.spe, distance='bray', k=2)
### Extraction des résultats
mite.spe.nmds

### Evaluation de la qualité de l'ajustement
mite.spe.nmds$stress
stressplot(mite.spe.nmds, main='Shepard plot')

### Construction du biplot
windows()
plot(mite.spe.nmds, type="none", main=paste('NMDS/Bray - Stress=', round(mite.spe.nmds$stress, 3)),
     xlab=c("NMDS1"),
     ylab=c("NMDS2"))
points(scores(mite.spe.nmds, display="sites", choices=c(1,2)),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(mite.spe.nmds, display="species", choices=c(1)),
     scores(mite.spe.nmds, display="species", choices=c(2)),
     labels=rownames(scores(mite.spe.nmds, display="species")),
     col="red", cex=0.8)

