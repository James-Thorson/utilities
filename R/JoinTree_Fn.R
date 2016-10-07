
#' @export
JoinTree_Fn = function( EdgeSet, Nodes2Keep, EdgeDist){
  EdgeSetOrig = EdgeSet
  DistOrig = EdgeDist
  # Figure out which are important (i.e. nodes with two outputs, or terminal nodes)
  n_out = table(EdgeSetOrig[,1])
  ImportantNodes = union( EdgeSetOrig[Nodes2Keep,2], as.numeric(names(n_out)[which(n_out>=2)]) )
  # Identify branches to start with when pruning
  Rows_to_start = which( EdgeSetOrig[,1]  %in% ImportantNodes)
  # New edge and distance set
  EdgeSetNew = DistNew = NULL
  # populate new edge set and dist set
  for(i in 1:length(Rows_to_start)){
    EdgeSetNew = rbind( EdgeSetNew, EdgeSetOrig[Rows_to_start[i],] )
    DistNew = c( DistNew, DistOrig[Rows_to_start[i]] )
    while( TRUE ){
      # Exit if at important node
      if( EdgeSetNew[i,2] %in% ImportantNodes ) break()
      # IDentify row and check for error
      RowI = which( EdgeSetOrig[,1] == EdgeSetNew[i,2] )
      if( length(RowI)>=2 ) stop("Some problem")
      # Join new branch to starting branch
      EdgeSetNew[i,2] = EdgeSetOrig[RowI,2]
      DistNew[i] = DistNew[i] + DistOrig[RowI]
    }
  }
  # return
  Return = list("ImportantNodes"=ImportantNodes, "EdgeSetNew"=EdgeSetNew, "DistNew"=DistNew )
  return(Return)
}
