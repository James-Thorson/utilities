
load_and_attach = function( file, remove_conflicts=TRUE, ... ){
  # Define function
  ls.ext <- function(file) {
    local({
      base::load(file)
      base::ls()
    }) }

  # Load
  Names = ls.ext(file)
  message( "File contains... ", Names )
  if( length(Names)>1 ) stop("File contains multiple objects")

  # detach and delete if necessary
  if( Names[1] %in% search()) detach( pos=which(Names[1]==search()) )
  if( remove_conflicts==TRUE ){
    Which = which( Names %in% ls() )
    if( length(Which)>0 ){
      remove( list=Names[Which] )
      message( "Deleting conflicts... ", Names[Which] )
    }
  }

  # attach
  attach( what=file )
  message( "Attaching... ", Names )

  # Return
  return( invisible(Names) )
}
