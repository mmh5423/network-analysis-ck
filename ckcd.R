#' Community Detection using Cohen's Kappa
#'
#' 
#' @param ck The matrix of Cohen's Kappa scores for the network
#' @keywords communities
#' @export
#' @examples
#' ckcd()

"ckcd" <- function(net, kmax, mode){
  
  n<-ncol(net)
  
  if (mode=='bipartite'){ 
    res=contingency2(net) 
  } else { 
    if (mode=='directed'){
    res=contingencyD(net) 
    } else { 
    res=contingency(net)}
  }
  
  a<-res$a
  b<-res$b
  c<-res$c
  d<-res$d
  
  ck=(2*((a*d)-(b*c)))/(((a+b)*(b+d))+((a+c)*(c+d)))
  
  library(igraph)
  if (mode=='bipartite'){ 
    ck2<-ck
    ck2[ck2>0]<-1
    ck2[ck2<0]<-0
    ck2<-replace(ck2,is.nan(ck2),0)
    ck2<-as.matrix(ck2)
    inet<-graph.adjacency(ck2, mode="undirected")
    } else { 
    if (mode=='directed'){
    inet<-graph.adjacency(net, mode="directed")
    } else { 
    inet<-graph.adjacency(net, mode="undirected")
    }
  }

  #K Means
  ck<-replace(ck,is.nan(ck),1)
  partmat = matrix(nrow=n, ncol=kmax)
  Q = matrix(nrow=kmax, ncol=1)
  SSW = matrix(nrow=kmax, ncol=1)
  for (i in 2:kmax)
  {
    km<-kmeans(ck, i, nstart=1000)
    part<-as.matrix(km$cluster)
    partmat[,i]=part
    Q[i,]=modularity(inet, part)
    SSW[i,]=km$tot.withinss
  }
  
  k<-which.max(Q)
  
  best<-partmat[,k]
  best<-as.matrix(best)
  rownames(best)<-colnames(net)
  colnames(best)<-"ckcd"
  
  colnames(partmat)<-1:kmax
  partmat<-partmat[,-1]
  
  out<-cbind(1:kmax, Q, SSW)
  out<-out[-1,]
  colnames(out)<-c("k", "Q", "SSW")
  print(out[k-1,])
  
  res=list(partmat=partmat, out=out, ck=ck, best=best, k=k)
}