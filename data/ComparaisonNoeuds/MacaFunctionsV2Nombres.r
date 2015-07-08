#setwd("C:/Users/Maca/Documents/R/StageUdem/Package Maca/")
source("MacaFunctionsV2.r", verbose=TRUE )

# Code qui permet de créer k sous-réseaux avec des noeuds avec étiquettes
# puis de calculer les indices. Résultats sous forme de data frame. 
# g1 : réseau 1 g2: réseau 2

generarDatosIndices <- function(g1,g2,k)
{
  #On crée k sous réseaux de 4 noeuds à partir 
  # de noeuds du graphe 1 
  
  conjuntos <- generarConjuntoDeNodos(V(g1),k)
  
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

  indiceIso <- GraphIsomorphism(listaSubgrafos1, listaSubgrafos2)
  
  indiceAdjMatrix <- AdjacencyMatrix (listaSubgrafos1, listaSubgrafos2)
  
  indicePathsMatrix <- PathsMatrix (listaSubgrafos1, listaSubgrafos2)
  
  indiceDegreesEtiquettes <- DegreesEtiquettes (listaSubgrafos1, listaSubgrafos2)
    
  datosResultado <- data.frame(Isomorphism=indiceIso, AdjacencyMatrix=indiceAdjMatrix,
                               MatrixPaths=indicePathsMatrix, 
                               DegreesEtiquettes=indiceDegreesEtiquettes)
  
  datosResultado
}




# Création de réseaux aléatoires et calcul des indices. 

grafo1<-barabasi.game(16, directed=FALSE)
V(grafo1)$name<-c(1:16)

grafo2<-barabasi.game(16, directed=FALSE)
V(grafo2)$name<-c(1:16)


generarDatosIndices(grafo1,grafo2,1)