require (igraph)

# Version 2: créer graphes, sélectionner les noeuds, et calculer les indices.

# Motifs (sous-réseaux) composés de 4 noeuds. 

#########################################################################################

# 
# Réseaux avec noeuds de couleurs.



# Fonction qui permet de déterminer si les couleurs d’une liste de noeuds sont les memes

coloresVertices <- function(v)
{
  if ((v$color[1] == v$color[2] )&&(v$color[2]==v$color[3])&&(v$color[3]==v$color[4]))
  {
    return (TRUE)
  }else
  {
    return (FALSE)
  }
}


# Fonction qui génère k sous-réseaux de noeuds avec au moins 2 couleurs différentes. 
generarConjuntoDeNodosColor <- function(lnodos, k)
{
  listaConjuntos<-list()
  i <- 1
  while (i<=k) 
  {
    v<- sample(lnodos, 4)
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
generarConjuntoDeNodosConPreseleccionados <- function(lnodos, k, vseleccionados)
  
{
  listaConjuntos<-list()
  n <- 4 - length(vseleccionados)
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
generarConjuntoDeNodos <- function(lnodos, k)
{
  listaConjuntos<-list()
  for (i in 1:k) 
  {
    v<- sample(lnodos, 4)
    listaConjuntos[[length(listaConjuntos)+1]]<-v
  }
  return (listaConjuntos)
}

# Fonction qui plot les sous-réseaux
pintarSubgrafos <- function(lista, g1, g2)
{
  num <- length(lista[[1]])
  pdf('prueba.pdf')
  par(mfrow=c(num+1, 2))
  plot(g1, main="Graph 1")
  plot(g2, main="Graph 2")
  for (i in 1:num)
  {
    plot((lista[[1]])[[i]])
    plot((lista[[2]])[[i]])
    
  }
  dev.off()
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

VectorDegrees <- function (sg1, sg2)
{
  llistasg <- length (sg1)
  alldegrees <- c()
  for (i in 1:llistasg)
  {
    
    alldegrees1 <- (degree.distribution(sg1[[i]]))*4
    alldegrees2 <- (degree.distribution(sg2[[i]]))*4
    
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
# Fonctions qui permettent de calculer les indices pour les noeuds avec couleurs:

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






