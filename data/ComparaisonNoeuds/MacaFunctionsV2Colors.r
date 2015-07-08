#setwd("C:/Users/Maca/Documents/R/StageUdem/Package Maca/")
source("MacaFunctionsV2.r", verbose=TRUE )

# Code qui permet de créer k sous-réseaux avec des noeuds de couleurs
# puis de calculer les indices. Résultats sous forme de data frame. 
# g1 : réseau 1 g2: réseau 2

generarDatosIndices <- function(g1,g2,k)
{

  #On crée k sous réseaux de 4 noeuds à partir 
  # de noeuds du graphe 1 
  
  conjuntos <- generarConjuntoDeNodosColor(V(g1),k)
  
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




# Création de réseaux aléatoires et calcul des indices. 

grafo1<-barabasi.game(16, directed=FALSE)
V(grafo1)$name<-c(1:16)
V(grafo1)$color<-c("white", "grey")

grafo2<-barabasi.game(16, directed=FALSE)
V(grafo2)$name<-c(1:16)
V(grafo2)$color <- V(grafo1)$color


generarDatosIndices(grafo1,grafo2,1)