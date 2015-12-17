rename_columns = function( DF, origname=colnames(DF), newname ){
  DF_new = DF
  for(i in 1:length(origname)){
    Match = match( origname[i], colnames(DF_new) )
    if(length(Match)==1) colnames(DF_new)[Match] = newname[i]
  }
  return(DF_new)
}
