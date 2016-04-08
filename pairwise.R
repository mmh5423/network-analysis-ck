#' Link Analysis
#'
#' This function considers two vertices at a time and outputs their common neighbor and unique neighbors.
#' @param A is the network, name1 is the first vertex and name2 is the second (column labels)
#' @keywords neighbors
#' @export
#' @examples
#' pairwise()

#border.wars<-pairwise(A, "Missouri", "Kansas")

"pairwise" <- function(net, name1, name2){
  
V1<-net[,name1]
V2<-net[,name2]
edge<-net[name1, name2]
temp<-cbind(V1, V2, V1+V2, V1-V2)
common.neighbors<-subset(temp, temp[,3] == 2)
common.neighbors<-row.names(common.neighbors)
V1.neighbors<-subset(temp, temp[,4] == 1)
V1.neighbors<-row.names(V1.neighbors)
V2.neighbors<-subset(temp, temp[,4] == -1)
V2.neighbors<-row.names(V2.neighbors)
#D<-subset(temp, temp[,3] ==0)

res = list(V1=name1, V2=name2, edge=edge, common.neighbors=common.neighbors, 
           V1.neighbors=V1.neighbors, V2.neighbors=V2.neighbors) 

}