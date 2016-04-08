#' Community Detection using Cohen's Kappa - Known K
#'
#' 
#' @param net An adjacency matrix with zeros on the diagonal
#' @keywords communities
#' @export
#' @examples
#' kmeansCK()

"kmeansCK" <- function(net, k, kind){
  
  n<-ncol(net)
  prox<-proximity(net, kind)
  ck<-prox$ck
  ck<-replace(ck,is.nan(ck),1)
  
  #K Means
  km<-kmeans(ck, k, nstart=1000)
  part<-as.matrix(km$cluster)
  
}