vector_to_design_matrix = function( vec ){
  levels = sort(unique(vec))
  mat = matrix(0,ncol=length(levels),nrow=length(vec),dimnames=list(NULL,levels))
  for(i in 1:nrow(mat)){
    mat[i,which(vec[i]==levels)] = 1
  }
  return( mat )
}
