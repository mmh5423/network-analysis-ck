#' Bimodality Test
#'
#' Given the Cohen's Kappa scores for the vertices in a network, bimodality in the distribution implies there is community structure.
#' @param net An adjacency matrix with zeros on the diagonal
#' @keywords communities
#' @export
#' @examples
#' bimod()
 
"bimod" <- function(net){
  
n<-ncol(net)
prox<-proximity(net)
ck<-prox$ck

ck[lower.tri(ck)] <- NaN
ck<-as.vector(ck)
ck<-as.matrix(ck)
ck<-ck[complete.cases(ck), ]
#hist(ck, breaks=2*n)
  
library(diptest)
dip<-dip.test(ck)
dip

library(mixtools)
mix<-normalmixEM(ck, k=2, fast=TRUE)
plot(mix, density = TRUE, w = 1.1, breaks=n/2)
mu=as.matrix(mix$mu)
sig=as.matrix(mix$sigma)
p<-as.matrix(mix$lambda)
d=(mu[2,]-mu[1,])/(2*sqrt(sig[2,]*sig[1,]))
q1=abs(log(p[2,])-log(p[1,]))
a=sqrt(d*d-1)
q2=2*log(d-a)+2*d*a

em.Res=cbind(mu, sig, p)
colnames(em.Res)<-c("mu", "sigma", "lambda")

bimodality.Res<-cbind(d, q1, q2)
colnames(bimodality.Res)<-c("d", "q1", "q2")

if(d>1 && q1<q2){print("Bimodal - Communities Present")} else {print("Unimodal - No Community Structure")}

res<-list(em=em.Res, bimodality=bimodality.Res)


}