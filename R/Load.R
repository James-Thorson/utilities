
Load = function( file, dir=getwd(), remove_conflicts=TRUE, ... ){
  # Define function
  ls.ext <- function(file){
    local({
      base::load(file)
      base::ls()
    })
  }

  # Load names
  Object_names = ls.ext( file.path(dir,file) )
  message( "File contains... ", Object_names )
  if( length(Object_names)>1 ) stop("File contains multiple objects")

  # Load object
  load( file.path(dir,file) )
  Object = get( Object_names )

  # detach
  if( Object_names[1] %in% search()){
    detach( pos=which(Object_names[1]==search()) )
  }

  # delete if necessary
  if( remove_conflicts==TRUE ){
    Which = which( names(Object) %in% ls() )
    if( length(Which)>0 ){
      remove( list=names(Object)[Which] )
      message( "Deleting conflicts... ", paste(names(Object)[Which], collapse=",") )
    }
  }

  # Return
  return( Object )
}
