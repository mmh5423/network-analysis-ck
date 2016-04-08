#' Vertx Contingency Table Values
#'
#' This function calculates the components of a contingency table for each pair of vertices in an adjacency matrix. Primarily used as input for the proximity function.
#' @param net is a binary, symmetric adjacency matrix with zeros on the diagonal
#' @keywords contingency
#' @export
#' @examples
#' contingency()

"contingency" <- function(net){

if( isSymmetric(net)==FALSE ) warning('Adjacency matrix not symmetric')
if( any(net > 1) ) warning('Adjacency matrix not binary')
  
n<-ncol(net)

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
net2<-net2-diag(diag(net2))
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
b=(pi-a)-net
b[lower.tri(b)] <- 0
b<-b-diag(diag(b))
b=(b+t(b))

#c(i,j) = j's unique neighbors (column verex)
c=(pj-a)-net
c[lower.tri(c)] <- 0
c<-c-diag(diag(c))
c=(c+t(c))


res<-list(a=a, b=b, c=c, d=d, p=p, q=q)

}