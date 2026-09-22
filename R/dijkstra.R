#' dijkstra's shortest path algorithm
#' it calculates the shotest path fron initial node to every node in a graph
#' @param graph a dataframe containing v1,v2,w
#' @param init_node the starting node
#' @return vector conraining the shortest distance from initial node to every node
#' @references
#' https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)
#' @export
dijkstra<-function(graph,init_node){
  stopifnot(is.numeric(init_node))
  stopifnot(is.data.frame(graph))
  nodes<-sort(unique(c(graph[["v1"]],graph[["v2"]])))
  d<-rep(Inf,length(nodes))
  names(d)<-nodes
  d[as.character(init_node)]<-0
  unv<-nodes
  while(length(unv)>0){
    unv_d<-d[as.character(unv)]
    current<-unv[which.min(unv_d)]
    edge<-graph[graph[["v1"]]==current,]
    for (i in seq_len(nrow(edge))) {
      neighbour<-edge$v2[i]
      w<-edge$w[i]
      new_d<-d[as.character(current)]+w

      if (new_d<d[as.character(neighbour)]) {
        d[as.character(neighbour)]<-new_d
      }
    }
    unv<- unv[unv!=current]
  }
  return(unname(d))
}


