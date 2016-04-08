#' Link Prediction ARI
#'
#' This function outputs the ARI (Cohen's Kappa) scores for linked and unlinked pairs of vertices in the network.
#' @param net A symmetric binary adjacency matrix with zeros on the diagonal
#' @keywords link prediction
#' @export
#' @examples
#' linkpred()

"linkpred" <- function(net){
  
  prox<-proximity(net, kind="undirected")
  
  n<-ncol(net)
  dist<-prox$ck
  dist<-replace(dist,is.nan(dist),NA)
  
  links<-as.vector(net)
  dist<-as.vector(dist)
  
  v1<-matrix(rep(rownames(net), n), nrow=n)
  v2<-t(v1)
  v1[lower.tri(v1)] <- NA
  v2[lower.tri(v2)] <- NA
  
  v1<-as.vector(v1)
  v2<-as.vector(v2)
  
  full<-cbind(v1, v2, links, dist)
  full<-full[complete.cases(full),]
  
  Linked<-subset(full, full[,3] > 0)
  Linked<-Linked[sort.list(Linked[,4], decreasing=TRUE), ]
  Unlinked<-subset(full, full[,3] < 1)    
  Unlinked<-Unlinked[sort.list(Unlinked[,4], decreasing=TRUE), ]

  results.date<-c(Sys.Date(), Sys.time())
  res<-list(linked=Linked, unlinked=Unlinked, results.date=results.date)

}
