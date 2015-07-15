require (igraph)

# Version 2: créer graphes, sélectionner les noeuds, et calculer les indices.

# Motifs (sous-réseaux) composés de 4 noeuds. 

#########################################################################################

# 
# Réseaux avec noeuds de couleurs.



# Fonction qui permet de déterminer si les couleurs d’une liste de noeuds sont les memes

coloresVertices <- function(v)
{
	colors<-as.factor(v$color)
	if (length(levels(colors)) ==1){
		return(TRUE)
	}
	else{
		return(FALSE)
	}
}


# Fonction qui génère k sous-réseaux de noeuds avec au moins 2 couleurs différentes. 
generarConjuntoDeNodosColor <- function(lnodos, k, motif.size)
{
  listaConjuntos<-list()
  i <- 1
  while (i<=k) 
  {
    v<- sample(lnodos, motif.size)
    if(coloresVertices(v) == FALSE)
    {
      listaConjuntos[[length(listaConjuntos)+1]]<-v
      i<-i+1
    }
    print(v)
  }
  return (listaConjuntos)
}


#########################################################################################

# Fonction qui permet de tester si les noeuds ont été déjà selectionnés. 

compararconseleccionados <- function (v, vseleccionados)
{
  
  estaAlguno <- FALSE
  for (i in 1:length(v))
  {
    if (v[[i]] %in% vseleccionados)
    {
      estaAlguno <- TRUE
    }
  }
  
  return (estaAlguno)
}

agregar <- function(v1,v2)
{
  long <- length(v2)
  
  v1 <- append(v1,v2,after=length(v1))
  
  return(v1)
}



# Fonction qui permet de créer k sous réseaux de noeuds à partir d’une liste de 

# noeuds donnés (lnodos) avec 1, 2 ou 3 noeuds déjà présélectionnés (vseleccionados)
generarConjuntoDeNodosConPreseleccionados <- function(lnodos, k, vseleccionados, motif.size)
  
{
  listaConjuntos<-list()
  n <- motif.size - length(vseleccionados)
  i <- 1
  while (i<=k) 
  {
    v <- sample(lnodos, n)
    if(compararconseleccionados(v, vseleccionados) == FALSE)
    {
      unidos <- agregar(v, vseleccionados)
      print(unidos)
      listaConjuntos [[length(listaConjuntos)+1]] <- unidos
      i<-i+1
    }
  }
  return (listaConjuntos)
}

# Fonction qui crée k sous-réseaux de noeuds 
generarConjuntoDeNodos <- function(lnodos, k, motif.size)
{
  listaConjuntos<-list()
  for (i in 1:k) 
  {
    v<- sample(lnodos, motif.size)
    listaConjuntos[[length(listaConjuntos)+1]]<-v
  }
  return (listaConjuntos)
}

# Fonction qui plot les sous-réseaux
pintarSubgrafos <- function(lista, g1, g2)
{
  #num <- length(lista[[1]])
  #png('prueba.png')
  #par(mfrow=c(num+1, 2))
  #plot(g1, main="Graph 1")
  #plot(g2, main="Graph 2")
  #for (i in 1:num)
  #{
  #  plot((lista[[1]])[[i]])
  #  plot((lista[[2]])[[i]])
  #  
  #}
 # dev.off()
}



#########################################################################################
# Fonctions qui permettent de calculer les indices pour les trois niveaux d'information:
# Niveaux d'information: aucune, couleures, étiquettes. 

GraphIsomorphism <- function (listasg1, listasg2)
{
  llistasg <- length (listasg1)
  isomorphism <- c()
  for (i in 1:llistasg)
  {
    isomorphism[[length(isomorphism)+1]]<- graph.isomorphic(listasg1[[i]], listasg2[[i]])
  }
  return(isomorphism)
}


#########################################################################################
# Fonctions qui permettent de calculer les indices pour les noeuds sans information:

avgDegree <- function(sg)
{
  grados1 <- degree(sg)
  
  return(mean(grados1))
}

NumeroEdges <- function (sg)
{
  edges <- ecount(sg)
  
  return(edges)
}

DegreeMin <- function (sg)
{
  mindegree <- min(degree(sg))
  
  return (mindegree)
}

DegreeMax <- function (sg)
{
  maxdegree <- max(degree(sg))
  
  return (maxdegree)
}

DifferenceMinMax <- function (sg)
{
  difference <- (max(degree(sg))-min(degree(sg)))
  
  return (difference)
}

Diameter <- function (sg)
{
  diam <- diameter(sg, directed=FALSE, unconnected=TRUE)
  
  return (diam)
}

VectorDegrees <- function (sg1, sg2, size.motif)
{
  llistasg <- length (sg1)
  alldegrees <- c()
  for (i in 1:llistasg)
  {
    
    alldegrees1 <- (degree.distribution(sg1[[i]]))*size.motif
    alldegrees2 <- (degree.distribution(sg2[[i]]))*size.motif
    
    if (length(alldegrees1) != length(alldegrees2))
    {
        if (length(alldegrees1) < length(alldegrees2))
        {
          alldegrees1 <- c(alldegrees1, rep(0, length(alldegrees2)-length(alldegrees1))) 
        }else
        {
          alldegrees2 <- c(alldegrees2, rep(0, length(alldegrees1)-length(alldegrees2)))
        } 
    }
  
    alldegrees[[length(alldegrees)+1]]<- sum(abs(alldegrees1 - alldegrees2))
  }

  return (alldegrees)
}


#########################################################################################
# Fonctions qui permettent de calculer les indices pour les noeuds avec couleurs: (2 couleurs)

AvgDegreesColors <- function (sg1, sg2)
{
  llistasg <- length (sg1)
  degreeColors<- c()
  degreeColors1 <- c()
  degreeColors2 <- c()
  
  for (i in 1:llistasg)
  {
    coloresnodossg1 <- unique(V(sg1[[i]])$color)
    coloresnodossg2 <- unique(V(sg2[[i]])$color)
    
    degreeColors1[1]<- mean(degree(sg1[[i]], v=V(sg1[[i]]) [color==coloresnodossg1[1]]))
    degreeColors1[2] <- mean(degree(sg1[[i]], v=V(sg1[[i]]) [color==coloresnodossg1[2]]))
    
    degreeColors2[1]<- mean(degree(sg2[[i]], v=V(sg2[[i]]) [color==coloresnodossg2[1]]))
    degreeColors2[2] <- mean(degree(sg2[[i]], v=V(sg2[[i]]) [color==coloresnodossg2[2]]))
    
    degreeColors[[length(degreeColors)+1]] <- sum(abs(degreeColors1 - degreeColors2))
  }
  
  return(degreeColors)
}

MaxDegreesColors <- function (sg1, sg2) 
{
  llistasg <- length (sg1)
  maxdegreeColors<- c()
  maxdegreeColors1 <- c()
  maxdegreeColors2 <- c()

  for (i in 1:llistasg)
  {
    coloresnodossg1 <- unique(V(sg1[[i]])$color)
    coloresnodossg2 <- unique(V(sg2[[i]])$color)
    
    maxdegreeColors1[1]<- max(degree(sg1[[i]], v=V(sg1[[i]]) [color==coloresnodossg1[1]]))
    maxdegreeColors1[2] <- max(degree(sg1[[i]], v=V(sg1[[i]]) [color==coloresnodossg1[2]]))
    
    maxdegreeColors2[1]<- max(degree(sg2[[i]], v=V(sg2[[i]]) [color==coloresnodossg2[1]]))
    maxdegreeColors2[2] <- max(degree(sg2[[i]], v=V(sg2[[i]]) [color==coloresnodossg2[2]]))
    
    maxdegreeColors[[length(maxdegreeColors)+1]] <- sum(abs(maxdegreeColors1 - maxdegreeColors2))
  }
                          
  return (maxdegreeColors)
}

MinDegreesColors <- function (sg1, sg2)
{
  llistasg <- length (sg1)
  mindegreeColors<- c()
  mindegreeColors1 <- c()
  mindegreeColors2 <- c()
  
  for (i in 1:llistasg)
  {
    coloresnodossg1 <- unique(V(sg1[[i]])$color)
    coloresnodossg2 <- unique(V(sg2[[i]])$color)
    
    mindegreeColors1[1]<- min(degree(sg1[[i]], v=V(sg1[[i]]) [color==coloresnodossg1[1]]))
    mindegreeColors1[2] <- min(degree(sg1[[i]], v=V(sg1[[i]]) [color==coloresnodossg1[2]]))
    
    mindegreeColors2[1]<- min(degree(sg2[[i]], v=V(sg2[[i]]) [color==coloresnodossg2[1]]))
    mindegreeColors2[2] <- min(degree(sg2[[i]], v=V(sg2[[i]]) [color==coloresnodossg2[2]]))
    
    mindegreeColors[[length(mindegreeColors)+1]] <- sum(abs(mindegreeColors1 - mindegreeColors2))
  }
  
  return (mindegreeColors)
}

EdgesNodosC1C1 <- function (sg)
{
  coloresnodossg <- unique(V(sg)$color)
  EdgesC1C1 <- length ((E)(sg)[V(sg) [color==coloresnodossg[1]] %--% 
                                 V(sg) [color==coloresnodossg[1]]])
  return (EdgesC1C1)
}

EdgesNodosC2C2 <- function (sg)
{
  coloresnodossg <- unique(V(sg)$color)
  EdgesC2C2 <- length ((E)(sg)[V(sg) [color==coloresnodossg[2]] %--% 
                                 V(sg) [color==coloresnodossg[2]]])
  
  return (EdgesC2C2)
}

EdgesNodosC1C2 <- function (sg)
{
  coloresnodossg <- unique(V(sg)$color)
  EdgesC1C2 <- length ((E)(sg)[V(sg) [color==coloresnodossg[1]] %--% 
                                 V(sg) [color==coloresnodossg[2]]])
  return (EdgesC1C2)
}


#########################################################################################
# Fonctions qui permettent de calculer les indices pour les noeuds avec étiquettes:

DegreesEtiquettes <- function (sg1, sg2)
{
  llistasg <- length (sg1)
  degrees<- c()
  for (i in 1:llistasg)
  {
    degrees1 <- degree (sg1[[i]])
    degrees2 <- degree (sg2[[i]])
    
    degrees [[length(degrees)+1]]<- sum (abs(degrees1 - degrees2))
  }
  return (degrees)
}

AdjacencyMatrix <- function (sg1, sg2)
{
  llistasg <- length (sg1)
  adjMatrix <- c()
  for (i in 1:llistasg)
  {
    adjMatrix1 <- get.adjacency(sg1[[i]], type=c("lower"),sparse=FALSE)
    
    adjMatrix2 <- get.adjacency(sg2[[i]], type=c("lower"),sparse=FALSE)
    
    adjMatrix [[length(adjMatrix)+1]] <- sum(abs(adjMatrix1 - adjMatrix2))
    
  }
  return (adjMatrix)
}

PathsMatrix <- function (sg1, sg2)
{
  llistasg <- length (sg1)
  pathsMatrixsg <- c()
  for (i in 1:llistasg)
  {
    pathsMatrixsg1 <- shortest.paths(sg1[[i]])
    pathsMatrixsg1[!is.finite(pathsMatrixsg1)] <- 0
    
    pathsMatrixsg2 <- shortest.paths(sg2[[i]])
    pathsMatrixsg2[!is.finite(pathsMatrixsg2)] <- 0
    
    pathsMatrixsg[[length(pathsMatrixsg)+1]] <- sum(abs(pathsMatrixsg1-pathsMatrixsg2),na.rm=TRUE)/2
  }
   
  return (pathsMatrixsg)
}



#########################################################################################
# Fonction qui calcule la différence pour un indice entre une paire de motifs
# listaSG: liste qui contient en première position la liste de sous réseaux du graphe1 
# et en deuxième position la lite de sous réseaux du graphe 2. 
# FUN: fonction qui calcule l'indice pour chaque motif. 

calcularIndice<- function(listaSG,FUN)
  
{
  calculo1 <- sapply(listaSG[[1]], FUN)
  calculo2 <- sapply(listaSG[[2]], FUN)
  lavgDegree <- length(calculo1)
  diferencia <- c()
  for(i in 1:lavgDegree)
  {
    diferencia[[length(diferencia)+1]]<-abs(calculo1[[i]]-calculo2[[i]])
  }
  return (diferencia)
  
}


# Code qui permet de créer k sous-réseaux avec des noeuds de couleurs
# puis de calculer les indices. Résultats sous forme de data frame. 
# g1 : réseau 1 g2: réseau 2
# Fonction principale 

generer_indices <- function(g1,g2,k,m)
{

  #On crée k sous réseaux de 4 noeuds à partir 
  # de noeuds du graphe 1 
  
  conjuntos <- generarConjuntoDeNodosColor(V(g1),k,motif.size)
  
  #On sélectionne les sous-graphes à partir de g1 y g2
  listaSubgrafos1<-list()
  listaSubgrafos2<-list()
  for (i in 1:k)
  {
    sg1<-induced.subgraph(g1, conjuntos[[i]])
    sg2<-induced.subgraph(g2, conjuntos[[i]])
    
    listaSubgrafos1[[length(listaSubgrafos1)+1]]<-sg1
    listaSubgrafos2[[length(listaSubgrafos2)+1]]<-sg2
  }
  
  listaSubgrafos<-list(listaSubgrafos1,listaSubgrafos2)
  
  pintarSubgrafos(listaSubgrafos, g1, g2)

  # On calcule la différence entre les indices des 2 motifs (sous-réseaux).
  
  indiceIso <- GraphIsomorphism(listaSubgrafos1,listaSubgrafos2)
  
  indiceAvgDegreeColor <- AvgDegreesColors (listaSubgrafos1,listaSubgrafos2)
  
  indiceMaxColors <- MaxDegreesColors (listaSubgrafos1,listaSubgrafos2)
  
  indiceMinColors <- MinDegreesColors (listaSubgrafos1,listaSubgrafos2)
  
  indiceEdgesC1C1 <- calcularIndice(listaSubgrafos, EdgesNodosC1C1)
  
  indiceEdgesC2C2 <- calcularIndice(listaSubgrafos, EdgesNodosC2C2)
  
  indiceEdgesC1C2 <- calcularIndice(listaSubgrafos, EdgesNodosC1C2)
  
  datosResultado <- data.frame(Isomorphism=indiceIso, AvgDegreeColors=indiceAvgDegreeColor, 
                               MaxDifferenceColors= indiceMaxColors, 
                               MinDifferenceColors=indiceMinColors, EdgesC1C1=indiceEdgesC1C1, 
                               EdgesC2C2=indiceEdgesC2C2, EdgesC1C2=indiceEdgesC1C2)
  
  datosResultado
}

#Fonction qui compte le nb de motif selon la couleur (2)

#conjuntos<-generarConjuntoDeNodos(V(test$g2),k,motif.size)

dist_motifs_colores<-function(g1,g2,conjuntos,motif.size)
{
	if(length(levels(as.factor(V(g1)$color)))==2 && length(levels(as.factor(V(g2)$color)))==2){ # pour 2 couleurs pour l'instant
		#if(motif.size ==3){
			#motifs=c(rep(0,4))
		#}
		if(motif.size ==4){
			motifs_list_g1<-get_motifs4_2colors(g1,conjuntos)
			motifs_list_g2<-get_motifs4_2colors(g2,conjuntos)
			
		}
		#if(motif.size ==5){
			#motifs=c(rep(0,34))
		#}
		#if(motif.size ==6){
			#motifs=c(rep(0,156))
		#}
		#if(motif.size ==7){
			#motifs=c(rep(0,1016))
		#}
		
		diff_topologie=abs((motifs_list_g1$motif_by_topology)-(motifs_list_g2$motif_by_topology))
		diff_topologie_et_couleur=list()
		for (i in 1:length(motifs_list_g1$motif_by_colors)){
			diff_topologie_et_couleur[[i]]=abs((motifs_list_g1$motif_by_colors[[i]])-(motifs_list_g2$motif_by_colors[[i]]))
		}
		diff_total_topologie=sum(diff_topologie)
		diff_total_topo_and_col=list()
		for (i in 1:length(motifs_list_g1$motif_by_colors)){
			diff_total_topo_and_col[[i]]=sum(diff_topologie_et_couleur[[i]])
		}
		
	}
	else{
		print("more than two colors")
	}
	return(list(motifs_topology_difference=diff_topologie, motifs_topology_and_color_difference=diff_topologie_et_couleur,total_difference_in_topology=diff_total_topologie,total_difference_in_topology_and_color=diff_total_topo_and_col))
}


#fonction pour comparer 2 vecteurs, voir s'ils contiennent les mêmes éléments, mais dans un ordre différent
mm_vecteur<-function(v1,v2){
	if(length(v1)==length(v2)){
		nb_element_trouve=0
		for(i in 1:length(v1)){
			j=1
			trouve=FALSE
			while((trouve==FALSE) && j<=length(v2)){
				if(v1[i]==v2[j]){
					nb_element_trouve=nb_element_trouve+1
					v2=v2[-j]
					trouve=TRUE
					
				}
				else{
					j=j+1
				}
			}
		}
		if(nb_element_trouve == length(v1)){
			res<-TRUE
		}
		else{
			res<-FALSE
		}
	}
	else{
		res<-FALSE
	}
	return(res)
}

get_motifs4_2colors<-function(g1, kcombination_res){
		motifs_topologie=rep(0,11)
		motifs=list(n8=0,n9=c(rep(0,9)),n10=c(rep(0,12)),n11=c(rep(0,6)),n12=c(rep(0,8)),n13=c(rep(0,8)),n14=c(rep(0,10)),n15=c(rep(0,12)),n16=c(rep(0,6)),n17=c(rep(0,9)),n18=c(rep(0,5)))
		for (i in 1:length(kcombination_res)){
			#vposition<-(as.vector(conjuntos[[i]])) #chercher la position des noeuds dans la liste de vertices
			names(kcombination_res[[i]])=kcombination_res[[i]]
			cmotif<-V(g1)[names(kcombination_res[[i]])]$color #chercher la couleur des noeuds
			names(cmotif)=names(kcombination_res[[i]])
			
			position=as.vector(V(g1)[names(kcombination_res[[i]])]);

			gmotif=induced.subgraph(g1, position)
			lmotif=degree(gmotif) #chercher le nb de liens des noeuds faisant partie du motif
			no_color=FALSE
			
			if(is.null(cmotif)){
				no_color=TRUE
			}
			else{
				col1=sort(levels(as.factor(V(g1)$color)))[1]
				col2=sort(levels(as.factor(V(g1)$color)))[2]
			}
			
			for (j in 1:11){
				if(graph.isomorphic(gmotif,graph.atlas(7+j)) == TRUE){
					motifs_topologie[j]= motifs_topologie[j]+1
					if(no_color==FALSE){
						if((7+j) == 8){
							motifs[[j]]=motifs[[j]]+1
						}
						if((7+j) == 9){
							noeuds_liens<-get.edges(gmotif, E(gmotif)) 
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 3 noeuds de couleur 1 et 1 noeud de couleur 2
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){
									if(V(gmotif)[noeuds_liens[1,1]]$color == V(gmotif)[noeuds_liens[1,2]]$color){
										motifs[[j]][3]=motifs[[j]][3]+1
									}
									else{
										motifs[[j]][4]=motifs[[j]][4]+1
									}
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
									if(V(gmotif)[noeuds_liens[1,1]]$color == V(gmotif)[noeuds_liens[1,2]]$color){
										motifs[[j]][5]=motifs[[j]][5]+1
									}
									else{
										motifs[[j]][6]=motifs[[j]][6]+1
									}
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
									if(V(gmotif)[noeuds_liens[1,1]]$color != V(gmotif)[noeuds_liens[1,2]]$color){
										motifs[[j]][7]=motifs[[j]][7]+1
									}
									else{
										if(V(gmotif)[noeuds_liens[1,1]]$color == col1){
											motifs[[j]][8]=motifs[[j]][8]+1
										}
										else{
											motifs[[j]][9]=motifs[[j]][9]+1
										}
									}
								}
							}
							
						}
						if((7+j) == 10){
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 3 noeuds de couleur 1 et 1 noeud de couleur 2
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){ #changer cmotif par g1
									couleur_noeud_seul<-cmotif[names(which(lmotif==0))]
									noeud_2liens<-cmotif[names(which(lmotif==2))]
									if(couleur_noeud_seul==col2){
										motifs[[j]][3]=motifs[[j]][3]+1
									}
									else{
										if(noeud_2liens==col2){
											motifs[[j]][4]=motifs[[j]][4]+1
										}
										else{
											motifs[[j]][5]=motifs[[j]][5]+1
										}
									}
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
									couleur_noeud_seul<-cmotif[names(which(lmotif==0))]
									noeud_2liens<-cmotif[names(which(lmotif==2))]
									if(couleur_noeud_seul==col1){
										motifs[[j]][6]=motifs[[j]][6]+1
									}
									else{
										if(noeud_2liens==col1){
											motifs[[j]][7]=motifs[[j]][7]+1
										}
										else{
											motifs[[j]][8]=motifs[[j]][8]+1
										}
									}
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
									couleur_noeud_seul<-cmotif[names(which(lmotif==0))]
									noeud_2liens<-cmotif[names(which(lmotif==2))]
									if(noeud_2liens==col1){
										if(couleur_noeud_seul==col2){
											motifs[[j]][9]=motifs[[j]][9]+1

										}
										else{
											motifs[[j]][10]=motifs[[j]][10]+1

										}
									}
									else{
										if(couleur_noeud_seul==col1){
											motifs[[j]][11]=motifs[[j]][11]+1

										}
										else{
											motifs[[j]][12]=motifs[[j]][12]+1

										}
										
									}
								}
							}

						}
						if((7+j) == 11){
							noeuds_liens<-get.edges(gmotif, E(gmotif)) 
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 1 noeud de couleur 2 et 3 noeuds de couleur 1
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){
										motifs[[j]][3]=motifs[[j]][3]+1
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
										motifs[[j]][4]=motifs[[j]][4]+1
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
									if(V(gmotif)[noeuds_liens[1,1]]$color != V(gmotif)[noeuds_liens[1,2]]$color){ #besoin de verifier le 2e edge?
										motifs[[j]][5]=motifs[[j]][5]+1
									}
									else{
											motifs[[j]][6]=motifs[[j]][6]+1
									}
								}
							}
							
						}
						if((7+j) == 12){
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 3 noeuds de couleur 1 et 1 noeud de couleur 2
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){ #changer cmotif par g1
									couleur_noeud_seul<-cmotif[names(which(lmotif==0))]
									if(couleur_noeud_seul==col1){
										motifs[[j]][3]=motifs[[j]][3]+1
									}
									else{
										motifs[[j]][4]=motifs[[j]][4]+1
									}
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
									couleur_noeud_seul<-cmotif[names(which(lmotif==0))]
									if(couleur_noeud_seul==col2){
										motifs[[j]][5]=motifs[[j]][5]+1
									}
									else{
										motifs[[j]][6]=motifs[[j]][6]+1
									}
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
									couleur_noeud_seul<-cmotif[names(which(lmotif==0))]
									if(couleur_noeud_seul==col1){
										motifs[[j]][7]=motifs[[j]][7]+1
									}
									else{
										motifs[[j]][8]=motifs[[j]][8]+1
									}	
								}
							}
						}
						if((7+j) == 13){
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 3 noeuds de couleur 1 et 1 noeud de couleur 2
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){ #changer cmotif par g1
									couleur_noeud_central<-cmotif[names(which(lmotif==3))]
									if(couleur_noeud_central==col1){
										motifs[[j]][3]=motifs[[j]][3]+1
									}
									else{
										motifs[[j]][4]=motifs[[j]][4]+1
									}
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
									couleur_noeud_central<-cmotif[names(which(lmotif==3))]
									if(couleur_noeud_central==col2){
										motifs[[j]][5]=motifs[[j]][5]+1
									}
									else{
										motifs[[j]][6]=motifs[[j]][6]+1
									}
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
									couleur_noeud_central<-cmotif[names(which(lmotif==3))]
									if(couleur_noeud_central==col1){
										motifs[[j]][7]=motifs[[j]][7]+1
									}
									else{
										motifs[[j]][8]=motifs[[j]][8]+1
									}	
								}
							}
							
						}
						if((7+j) == 14){
							noeuds_liens<-get.edges(gmotif, E(gmotif)) 
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 1 noeud de couleur 2 et 3 noeuds de couleur 1
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){
									# les couleurs des 2 noeuds ayant 2 liens
									noeud_2liens<-cmotif[names(which(lmotif == 2))]
									if(noeud_2liens[1]==noeud_2liens[2]){
										motifs[[j]][3]=motifs[[j]][3]+1
									}
									else{
										motifs[[j]][4]=motifs[[j]][4]+1
									}
										
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
									# les couleurs des 2 noeuds ayant 3 liens
									noeud_2liens<-cmotif[names(which(lmotif == 2))]
									if(noeud_2liens[1]==noeud_2liens[2]){
										motifs[[j]][5]=motifs[[j]][5]+1
									}
									else{
										motifs[[j]][6]=motifs[[j]][6]+1
									}
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
									noeud_2liens<-cmotif[names(which(lmotif == 2))]
									if(noeud_2liens[1]==noeud_2liens[2]){
										if(noeud_2liens[1]== col1){
											motifs[[j]][9]=motifs[[j]][8]+1
										}
										else{
											motifs[[j]][10]=motifs[[j]][9]+1
										}
									}
									else{
										v1<-noeuds_liens[,1]
										v2<-noeuds_liens[,2]
										lien_mm_couleur=FALSE
										
										for(i in 1:3){
											if(V(gmotif)[v1[i]]$color==V(gmotif)[v2[i]]$color){
												lien_mm_couleur=TRUE
											}
										}
										
										if(lien_mm_couleur == TRUE){
											motifs[[j]][7]=motifs[[j]][7]+1
										}
										else{
											motifs[[j]][8]=motifs[[j]][8]+1
										}
									}
								}
							}
							
						}
						if((7+j) == 15){
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 3 noeuds de couleur 1 et 1 noeud de couleur 2
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){ #changer cmotif par g1
									central_noeud_color<-cmotif[names(which(lmotif==3))]
									noeud_1lien<-cmotif[names(which(lmotif==1))]
									if(central_noeud_color==col2){
										motifs[[j]][3]=motifs[[j]][3]+1
									}
									else{
										if(noeud_1lien==col2){
											motifs[[j]][4]=motifs[[j]][4]+1
										}
										else{
											motifs[[j]][5]=motifs[[j]][5]+1
										}
									}
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
									central_noeud_color<-cmotif[names(which(lmotif==3))]
									noeud_1lien<-cmotif[names(which(lmotif==1))]
									if(central_noeud_color==col1){
										motifs[[j]][6]=motifs[[j]][6]+1
									}
									else{
										if(noeud_1lien==col1){
											motifs[[j]][7]=motifs[[j]][7]+1
										}
										else{
											motifs[[j]][8]=motifs[[j]][8]+1
										}
									}
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
									central_noeud_color<-cmotif[names(which(lmotif==3))]
									noeud_1lien<-cmotif[names(which(lmotif==1))]
									if(noeud_1lien==col1){
										if(central_noeud_color==col2){
											motifs[[j]][9]=motifs[[j]][9]+1
										}
										else{
											motifs[[j]][10]=motifs[[j]][10]+1
										}
									}
									else{
										if(central_noeud_color==col1){
											motifs[[j]][11]=motifs[[j]][11]+1
										}
										else{
											motifs[[j]][12]=motifs[[j]][12]+1
										}
										
									}
								}
							}
						
						}
						if((7+j) == 16){
							noeuds_liens<-get.edges(gmotif, E(gmotif)) 
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 3 noeuds de couleur 1 et 1 noeud de couleur 2
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){
									motifs[[j]][3]=motifs[[j]][3]+1
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
									motifs[[j]][4]=motifs[[j]][4]+1
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
										v1<-noeuds_liens[,1]
										v2<-noeuds_liens[,2]
										lien_mm_couleur=FALSE
										
										for(i in 1:3){
											if(V(gmotif)[v1[i]]$color==V(gmotif)[v2[i]]$color){
												lien_mm_couleur=TRUE
											}
										}
										
										if(lien_mm_couleur == TRUE){
											motifs[[j]][5]=motifs[[j]][5]+1
										}
										else{
											motifs[[j]][6]=motifs[[j]][6]+1
										}
								}
							}
						}
						if((7+j) == 17){
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 1 noeud de couleur 2 et 3 noeuds de couleur 1
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){
									# les couleurs des 2 noeuds ayant 3 liens
									noeud_3liens<-cmotif[names(which(lmotif == 3))]
									if(noeud_3liens[1]==noeud_3liens[2]){
										motifs[[j]][3]=motifs[[j]][3]+1
									}
									else{
										motifs[[j]][4]=motifs[[j]][4]+1
									}
										
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
									# les couleurs des 2 noeuds ayant 3 liens
									noeud_3liens<-cmotif[names(which(lmotif == 3))]
									if(noeud_3liens[1]==noeud_3liens[2]){
										motifs[[j]][5]=motifs[[j]][5]+1
									}
									else{
										motifs[[j]][6]=motifs[[j]][6]+1
									}
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
									noeud_3liens<-cmotif[names(which(lmotif == 3))]
									if(noeud_3liens[1]!=noeud_3liens[2]){
										motifs[[j]][7]=motifs[[j]][7]+1
									}
									else{
										if(noeud_3liens[1]== col1){
											motifs[[j]][8]=motifs[[j]][8]+1
										}
										else{
											motifs[[j]][9]=motifs[[j]][9]+1
										}
									}
								}
							}
						}
						if((7+j) == 18){
							noeuds_liens<-get.edges(gmotif, E(gmotif)) 
							# tous les noeuds sont de 1 couleur
							if(length(levels(as.factor(cmotif)))==1){ 
								if(levels(as.factor(cmotif)) == col1){
									motifs[[j]][1]=motifs[[j]][1]+1
								}
								else{
									motifs[[j]][2]=motifs[[j]][2]+1
								}
							}
							else{ 
								# 3 noeuds de couleur 1 et 1 noeud de couleur 2
								if(mm_vecteur(cmotif,c(rep(col1,3),col2))==TRUE){
									motifs[[j]][3]=motifs[[j]][3]+1
								}
								# 1 noeud de couleur 1 et 3 noeuds de couleur 2
								if(mm_vecteur(cmotif,c(rep(col2,3),col1))==TRUE){
									motifs[[j]][4]=motifs[[j]][4]+1
								}
								# 2 noeuds de chaque couleur
								if(mm_vecteur(cmotif,c(rep(col2,2),rep(col1,2)))==TRUE){
									motifs[[j]][5]=motifs[[j]][5]+1

								}
							}
						}
					}	
				}
			
			}
		}	
		return(list(motif_by_topology=motifs_topologie, motif_by_colors=motifs))
			
}

#names(lmotif8[which(as.vector(lmotif8)==0)])

