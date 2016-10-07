
#' @export
PruneTree_Fn = function( EdgeSet, Nodes2Keep, EdgeDist ){
  # IDEA: if network is "built" from single node, then there's one single path from outer ("starting") nodes to original ("terminal") node
  # Starting nodes
  Nodes2Match = Nodes2Keep
  # Aggregator for rows in tree that are necessary
  Rows2Keep = NULL
  # Total distance for each starting node to terminal node
  TotalDist = rep(0,length(Nodes2Keep))
  # Number of times each element in distance vector is used
  n_times_row_used = rep(0,nrow(EdgeSet))
  # path for each starting node
  Path2TopNode = vector("list",length=length(Nodes2Keep))
  # Loop through network from "starting" to "terminal" node
  while( length(na.omit(Nodes2Match))>0 ){
    NewRows = match(Nodes2Match, EdgeSet[,2])
    Rows2Keep = union(Rows2Keep, na.omit(NewRows))
    Nodes2Match = EdgeSet[NewRows,1]
    NewDist = EdgeDist[NewRows]
    TotalDist = TotalDist + ifelse(is.na(NewDist), 0, NewDist)
    n_times_row_used[na.omit(NewRows)] = n_times_row_used[na.omit(NewRows)] + 1
    for(i in 1:length(Nodes2Keep)) if( !is.na(NewRows[i]) ) Path2TopNode[[i]] = c( Path2TopNode[[i]], NewRows[i])
  }
  # Return stuff
  Return = list( "Rows2Keep"=Rows2Keep, "Path2TopNode"=Path2TopNode )
  return( Return )
}
