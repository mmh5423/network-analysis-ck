#' Vertex Proximity Measures
#'
#' This function calculates a variety of similarity measures for each pair of vertices in a network
#' @param net An adjacency matrix, bipartite if net has 2 modes
#' @keywords proximity
#' @export
#' @examples
#' proximity()

"proximity" <- function(net, kind){
  
  #Contingency2 -> bipartite/two mode graphs
  #ContingencyD -> directed graphs
  #Contingency --> undirected graphs
  
  if (kind=='bipartite') {
    res=contingency2(net)
  } else {
    if(kind=='directed'){
      res=contingencyD(net)
    } else {
      res=contingency(net)
      # kind='undirected'
    }
  }
  
  a<-res$a
  b<-res$b
  c<-res$c
  d<-res$d
  
  #Comon Neighbors
  cn=a
  
  #Preferential attachment
  pa<-(a+b)*(a+c)
  
  #Jaccard
  jc=a/(a+b+c)
  #jc<-replace(jc,is.nan(jc),1)
  
  #Cohen's Kappa (ARI)
  ck=(2*((a*d)-(b*c)))/(((a+b)*(b+d))+((a+c)*(c+d)))
  #ck<-replace(ck,is.nan(ck),1)
  
  #Maxwell Pillner 1968
  mp=(2*((a*d)-(b*c)))/(((a+b)*(c+d))+((a+c)*(b+d)))
  #mp<-replace(mp,is.nan(mp),1)
  
  #phi coefficient
  phi=((a*d)-(b*c))/sqrt((a+b)*(a+c)*(b+d)*(c+d))
  #phi<-replace(phi,is.nan(phi),1)
  
  #rand
  r=(a+d)/(a+b+c+d)
  #r<-replace(r,is.nan(r),1)
  
  #fowlkes and mallows
  fm=a/sqrt((a+b)*(a+c))
  #fm<-replace(fm,is.nan(fm),1)
  
  #kulczynski
  k=(1/2)*((a/(a+b))+(a/(a+c)))
  #k<-replace(k,is.nan(k),1)
  
  #euclidean
  eu=sqrt(b+c)
  
  #asymmetric binary (from R "dist")
  abn=(b+c)/(a+b+c)
  
  res<-list(a=a, b=b, c=c, d=d, eu=eu, abn=abn, cn=cn,  pa=pa, jc=jc, ck=ck, mp=mp, phi=phi, r=r, fm=fm, k=k, kind=kind)
}

