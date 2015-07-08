# function random_network
#  Generate undirected random network X and another network Y with the same number of node 
#  and the same colors.

# Input:
# nb_node:       number of node in the original network X and Y (g1 and g2) (default=25)
# nb_color:      number of color in the graph
# type :         either 'erdos'     for Erdos-Renyi model (default)
#                       'barabasi'  for Barabasi-Albert model 
#                       'watts'     for Watts-Strogatz model
#
# For each network type, additional parameter are required:
# Erdos-Renyi:
#     prob  :  probability of link between two nodes (default 0.2)
#
# Barabasi:
#     none
#
#  Watts-Strogatz: 
#     nei    :  the neighborhood within which the vertices of the lattice will be connected. (default = 1)
#     prob   :  the rewiring probability (default 0.2) 


# Output: 

# g1:          		network X
# g2:          		network Y
library(SDDE)

random_network<-function(nb_node=25, nb_color=2,  type='erdos', prob=0.2, nei=1) {
	ccolor=c(1:nb_color);	
	xcolor=array(0, nb_node);	
	set.seed(sample(1:10000000,1));
	for (i in 1:nb_node) {		
		xcolor[i]=sample(ccolor,1);
	}	
	if (type=='erdos') {		
		g1 <- erdos.renyi.game(nb_node,prob,type="gnp", directed=FALSE);
		g2 <- erdos.renyi.game(nb_node,prob,type="gnp", directed=FALSE);				
	} else if (type=='barabasi') {
		g1<-barabasi.game(nb_node, power=1, directed=FALSE);	
		g2<-barabasi.game(nb_node, power=1, directed=FALSE);		
	} else {
		g1=watts.strogatz.game(1, nb_node, nei, prob)
		g2=watts.strogatz.game(1, nb_node, nei, prob)
	}
	V(g1)$name<-paste("x",c(1:nb_node),sep="");
	V(g2)$name<-paste("x",c(1:nb_node),sep="");
	V(g1)$color <- xcolor;	
	V(g2)$color <- V(g1)$color; 
	return (list("g1"=g1,"g2"=g2));
}

## Test
##
## r=random_network(25,type="erdos");
## r=random_network(25,type="barabasi");
## r=random_network(25,type="watts");
## plot(r$g1)
## plot(r$g2)


