library(SDDE)
source("../R/random_network.R")
source("../R/MacaFunctionsV3.R")
r=random_network(50, prob=0.2);
generer_indices(r$g1, r$g2, 500);
pdf('test.pdf')
  par(mfrow=c(1, 2))
  plot(r$g1, main="Graph 1")
  plot(r$g2, main="Graph 2")
  dev.off();