#' Vertx Contingency Table Values - Bipartite
#'
#' This function calculates the components of a contingency table for each pair of column vertices.
#' @param net is an unweighted bipartite graph where the columns are the observations of interest
#' @keywords contingency
#' @export
#' @examples
#' contingency2()

"contingency2" <- function(net){

n<-ncol(net)
m<-nrow(net)

#a= common neighbors
#a(i,j) = count of vertices in network that are linked to both i and j
a<-t(net) %*% net
p<-diag(a)
a<-a-diag(p)
p<-as.matrix(p)
colnames(p) <- "degree"

#d= common non-neighbors
#d(i,j) = count of vertices in network that neither i nor j are linked to
net2=1-net
d<-t(net2) %*% net2
q<-diag(d)
d<-d-diag(q)
q<-as.matrix(q)
colnames(q) <- "nonneighbors"

#b&c= unique neighbors
#pi(i,j)= i's neighbor count (row vertex)
#pj(i,j)= j's neighbor count (column vertex)
pi<-matrix(rep(p, n), nrow=n)
pj<-t(pi)

#b(i,j) = i's unique neighbors (row verex)
b<-(pi-a)
b<-b-diag(diag(b))

#c(i,j) = j's unique neighbors (column verex)
#c=(pj-a)
#c<-c-diag(diag(c))
c<-t(b)

res<-list(a=a, b=b, c=c, d=d, p=p, q=q)
}