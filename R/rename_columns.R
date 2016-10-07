
#' Rename columns
#'
#' \code{rename_columns} renames columns of a matrix or data frame
#'
#' @param DF original data frame
#' @param newname new names for columns

#' @export
rename_columns = function( DF, origname=colnames(DF), newname ){
  DF_new = DF
  for(i in 1:length(origname)){
    Match = match( origname[i], colnames(DF_new) )
    if(length(Match)==1) colnames(DF_new)[Match] = newname[i]
  }
  return(DF_new)
}
