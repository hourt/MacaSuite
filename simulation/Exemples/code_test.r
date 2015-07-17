library(SDDE)
source("../R/MacaFunctionsV3.R")
library(Rcpp)
sourceCpp("../R/kcombination.cpp")
 
# Deux graphes de même topologie, mais de couleur inverse (même proportion des 2 couleurs)
ex1<-load_network("ex1.txt","ex1_tax.txt")
ex1<-add.vertices(ex1,nv=1,tax=2,name=9) #pour ajouter les noeuds sans liens
V(ex1)$color=V(ex1)$tax
k_ex1<-k4combination(V(ex1))

ex2<-load_network("ex1.txt","ex1_tax_rev.txt")
ex2<-add.vertices(ex1,nv=1,tax=2,name=9)
V(ex2)$color=V(ex2)$tax

dist_motifs_colores(ex1,ex2,k_ex1,4)
# Il n'y a pas de différences.

# Deux graphes de même topologie, mais de couleur inverse (diff proportion des 2 couleurs)
ex3<-load_network("ex1.txt","ex1_tax_2.txt")
ex3<-add.vertices(ex3,nv=1,tax=2,name=9) #pour ajouter les noeuds sans liens
V(ex3)$color=V(ex3)$tax
k_ex3<-k4combination(V(ex3))

ex4<-load_network("ex1.txt","ex1_tax3.txt")
ex4<-add.vertices(ex4,nv=1,tax=2,name=9)
V(ex4)$color=V(ex4)$tax
dist_motifs_colores(ex3,ex4,k_ex1,4)
# Il y a des différences.

# Deux graphs carrés de même couleur et topologie: ex4 -> 2 carrés reliés avec 6 liens, ex5 ->2 carrés reliés avec 4liens
ex4<-load_network("ex4.txt","ex4_tax.txt")
V(ex4)$color=V(ex4)$tax
k_ex4<-kcombination(V(ex4),4)

ex5<-load_network("ex5.txt","ex3_tax.txt")
V(ex5)$color=V(ex5)$tax
dist_motifs_colores(ex4,ex5,k_ex4,4)

# Un graph de 2 carrés reliés avec 4 liens et un graph de 2 carrés non reliés avec 4 liens
ex6<-load_network("ex6.txt","ex3_tax.txt")
V(ex6)$color=V(ex6)$tax
dist_motifs_colores(ex6,ex5,k_ex4,4)

# créer pdf avec la topologie des différents motifs de type 4
pdf(file="List_motifs_4.pdf")
par(mfrow=c(3,3))
for (i in 8:18){
	graph.i<-graph.atlas(i)
	plot(graph.i,main=paste0("no.graph.atlas: ",i,"\n",sep=""))
	title("Les différentes topologies pour les motifs de taille 4", outer=TRUE)
}

a=rep("blue",4)
b=rep("green",4)
d=c(rep("blue",3),"green")
e=c("green",rep("blue",3))
f=c("blue","blue","green","blue")
g=c("blue","green","blue","blue")
h=c(rep("green",3),"blue")
r=c("blue",rep("green",3))
j=c("green","green","blue","green")
k=c("blue","green","green","blue")
l=c("blue","blue","green","green")
m=c("green","green","blue","blue")
n=c("green","blue","green","blue")
o=c("blue","green","blue","green")
p=c("green","blue","blue","green")
z=c("green","blue","green","green")

color.list<-list(un=a,deux=b,trois=g,quatre=f,cinq=z,six=j,sept=o,huit=l,neuf=m)
par(mfrow=c(3,3))
for (i in 1:9){
	graph.i<-graph.atlas(9)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 9 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 9", outer=TRUE)
}

color.list<-list(un=a,deux=b,trois=e,quatre=d,cinq=f,six=r,sept=h,huit=j,neuf=m,dix=k,onze=o,douze=p)
par(mfrow=c(3,3))
for (i in 1:12){
	graph.i<-graph.atlas(10)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 10 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 10", outer=TRUE)
}

color.list<-list(un=a,deux=b,trois=e,quatre=h,cinq=o,six=k)
par(mfrow=c(3,3))
for (i in 1:6){
	graph.i<-graph.atlas(11)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 11 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 11", outer=TRUE)
}

color.list<-list(un=a,deux=b,trois=f,quatre=e,cinq=j,six=r,sept=k,huit=h)
par(mfrow=c(3,3))
for (i in 1:8){
	graph.i<-graph.atlas(12)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 12 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 12", outer=TRUE)
}

color.list<-list(un=a,deux=b,trois=f,quatre=e,cinq=j,six=r,sept=k,huit=h)
par(mfrow=c(3,3))
for (i in 1:8){
	graph.i<-graph.atlas(13)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 13 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 13", outer=TRUE)
}

color.list<-list(un=a,deux=b,trois=f,quatre=e,cinq=j,six=r,sept=k,huit=h,neuf=o,dix=p)
par(mfrow=c(3,3))
for (i in 1:10){
	graph.i<-graph.atlas(14)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 14 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 14", outer=TRUE)
}

color.list<-list(un=a,deux=b,trois=e,quatre=d,cinq=f,six=r,sept=h,huit=j,neuf=m,dix=k,onze=o,douze=p)
par(mfrow=c(3,3))
for (i in 1:12){
	graph.i<-graph.atlas(15)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 15 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 15", outer=TRUE)
}

color.list<-list(un=a,deux=b,trois=e,quatre=h,cinq=o,six=k)
par(mfrow=c(3,3))
for (i in 1:6){
	graph.i<-graph.atlas(16)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 16 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 16", outer=TRUE)
}

color.list<-list(un=a,deux=b,trois=g,quatre=f,cinq=z,six=j,sept=o,huit=l,neuf=m)
par(mfrow=c(3,3))
for (i in 1:9){
	graph.i<-graph.atlas(17)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 17 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 17", outer=TRUE)
}

color.list<-list(un=a,deux=b,trois=e,quatre=h,cinq=o)
par(mfrow=c(3,3))
for (i in 1:5){
	graph.i<-graph.atlas(18)
	V(graph.i)$color=color.list[[i]]
	plot(graph.i,main=paste0("Vecteur 18 position: ",i,"\n",sep=""))
	title("Les différentes motifs de taille 4 à 2 couleurs de topologie 18", outer=TRUE)
}

dev.off()
