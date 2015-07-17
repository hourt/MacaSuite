/**
 * k-combination 
 * This is a collection of function alloying the generation of k-combination of n object in R.
 * "More formally, a k-combination of a set S is a subset of k distinct elements of S. 
 * If the set has n elements, the number of k-combinations is equal to the binomial coefficient"
 * -http://en.wikipedia.org/wiki/Combination
 * e.g. k2combination(c(1,2,3)) will return:
 * [[1]] 
 * [1] 1 2
 * [[2]]
 * [1] 1 3
 * [[3]]
 * [1] 2 3
 *
 * Usage (in R):
 *
 * library(Rcpp)
 * sourceCpp("kcombination.cpp")
 * k2combination(c(1,2,3))
 * #Selected only the combinations containing 4 (84 elements vs 210):
 * k4combination(c(1:10),4)
 * Author: Etienne Lord
 * Since : Mai 2015
 */
#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include <algorithm>
#include <map>
#include <stdlib.h>     /* srand, rand */			  
 
//Return a list of combination for k=6
//In: data    : vector of the element e.g. c(1,2,3,4,5,6,7) or c(1:20)
//    selector: include only the combination including the <selector> element
//Out: a list of combination     
 
 // [[Rcpp::export]]
 Rcpp::List k6combination(Rcpp::NumericVector data, int selector=0) {
	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
 	combination.clear();
 	combinations.clear();
 	Rcpp::Timer timer;
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			for (int k=j+1;k<data.size();k++) {
 				for (int l=k+1;l<data.size();l++) {
 					for (int m=l+1;m<data.size();m++) {
 						for (int n=m+1;n<data.size();n++) {
							if (data[i]==selector||data[j]==selector||data[k]==selector||data[l]==selector||data[m]==selector||data[n]==selector||selector==0) {
								combination.clear();
								combination.push_back(data[i]);
								combination.push_back(data[j]);
								combination.push_back(data[k]);
								combination.push_back(data[l]);
								combination.push_back(data[m]);
								combination.push_back(data[n]);
								combinations.push_back(combination);
							}
						}
					}			
				}
 			}  
 		}
 	}
 	timer.step("k6combination");
    Rcpp::NumericVector res(timer);
    //printf("Total time: %f \n",res[0]);
 	//printf("Total combinations: %lu \n",combinations.size());
 	return Rcpp::wrap( combinations );
 }
 
  // [[Rcpp::export]]
 Rcpp::List k5combination(Rcpp::NumericVector data, int selector=0) {
 	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
 	combination.clear();
 	combinations.clear();
 	Rcpp::Timer timer;
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			for (int k=j+1;k<data.size();k++) {
 				for (int l=k+1;l<data.size();l++) {
 					for (int m=l+1;m<data.size();m++) {
						if (data[i]==selector||data[j]==selector||data[k]==selector||data[l]==selector||data[m]==selector||selector==0) {
							combination.clear();
							combination.push_back(data[i]);
							combination.push_back(data[j]);
							combination.push_back(data[k]);
							combination.push_back(data[l]);
							combination.push_back(data[m]);
							combinations.push_back(combination);
						}
					}			
				}
 			}  
 		}
 	}
 	timer.step("k5combination");
    Rcpp::NumericVector res(timer);
    //printf("Total time: %f \n",res[0]);
 	//printf("Total combinations: %lu \n",combinations.size());
 	return Rcpp::wrap( combinations );
 }
 
  // [[Rcpp::export]]
 Rcpp::List k4combination(Rcpp::NumericVector data, int selector=0) {
	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
 	combination.clear();
 	combinations.clear();
 	Rcpp::Timer timer;
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			for (int k=j+1;k<data.size();k++) {
 				for (int l=k+1;l<data.size();l++) {
 					if (data[i]==selector||data[j]==selector||data[k]==selector||data[l]==selector||selector==0) {
						combination.clear();
						combination.push_back(data[i]);
						combination.push_back(data[j]);
						combination.push_back(data[k]);
						combination.push_back(data[l]);
						combinations.push_back(combination);
					}
				}
 			}  
 		}
 	}
 	timer.step("k4combination");
    Rcpp::NumericVector res(timer);
    //printf("Total time: %f \n",res[0]);
 	//printf("Total combinations: %lu \n",combinations.size());
 	return Rcpp::wrap( combinations );
 }
 
 
  // [[Rcpp::export]]
 Rcpp::List k3combination(Rcpp::NumericVector data, int selector=0) {
	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
 	combination.clear();
 	combinations.clear();
 	Rcpp::Timer timer;
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			for (int k=j+1;k<data.size();k++) {
 				if (data[i]==selector||data[j]==selector||data[k]==selector||selector==0) {
 					combination.clear();
 					combination.push_back(data[i]);
 					combination.push_back(data[j]);
 					combination.push_back(data[k]);
 					combinations.push_back(combination);
 				}
 			}  
 		}
 	}
 	timer.step("k3combination");
    Rcpp::NumericVector res(timer);
    //printf("Total time: %f \n",res[0]);
 	//printf("Total combinations: %lu \n",combinations.size());
 	return Rcpp::wrap( combinations );
 }
 
 
   // Faster for k=2
  // [[Rcpp::export]]
 Rcpp::List k2combination(Rcpp::NumericVector data, int selector=0) {
	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
 	combination.clear();
 	combinations.clear();
 	Rcpp::Timer timer;
 	for (int i=0; i< data.size();i++) {
 		for (int j=i+1;j<data.size();j++) {
 			if (data[i]==selector||data[j]==selector||selector==0) {
 				combination.clear();
 				combination.push_back(data[i]);
 				combination.push_back(data[j]);
 			 	combinations.push_back(combination); 
 			}
 		}
 	}
 	timer.step("k2combination");
    Rcpp::NumericVector res(timer);
    //printf("Total time: %f \n",res[0]);
 	//printf("Total combinations: %lu \n",combinations.size());
 	return Rcpp::wrap( combinations );
 }
 
 // [[Rcpp::export]]
 Rcpp::List kcombination(Rcpp::NumericVector data, int k, int selector=0) {
	std::vector<std::vector<int> > combinations;
	std::vector<int> combination;
 	combinations.clear();
 	combination.clear();
 	Rcpp::Timer timer;
 	if (k==2) return k2combination(data, selector);
 	if (k==3) return k3combination(data, selector);
 	if (k==4) return k4combination(data, selector);
 	if (k==5) return k5combination(data, selector);
 	if (k==6) return k6combination(data, selector);
 	//go(data,0,k,selector);
 	timer.step("kcombination");
 	 Rcpp::NumericVector res(timer);
 	 //printf("Total time: %f \n",res[0]);
 	//printf("Total combinations: %lu \n",combinations.size());
 	return Rcpp::wrap( combinations );
 	//return combinations.size();
 	//printf("%f:",combinations.size()); 
 	//return  1;
 }