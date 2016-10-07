
#' Save a figure
#'
#' \code{save_fig} saves a figure to a local directory
#'
#' @param filename name (including absolute path) for figure
#' @param width width for figure
#' @param height height for figure
#' @param units units for measuring height/width
#' @param type character-vector specifying type of figure (Options: "png", "tif", "jpg"). If \code{length(type)}>1, and \code{FUN} is specified, then makes muliple plots
#' @param suffix character-vector specifying suffix for different plots, e.g., if \code{type}>2
#' @param FUN optional function containing expression (all lines of script) for plotting.  Useful if \code{length(type)}>1.

#' @export
save_fig = function( filename, width, height, units="in", res=200, type="png", suffix="", FUN=NULL, ...){
  # how many figures
  if( is.null(FUN) ){
    n_fig = 1
  }
  if( !is.null(FUN)){
    n_fig = max( length(res), length(type) )
    if( length(res)==1) res = rep(res,n_fig)
    if( length(type)==1) type = rep(type,n_fig)
    if( length(suffix)==1) suffix = rep(suffix,n_fig)
  }
  # Remove file type from filename if necessary
  Test = sapply( c(".tif",".tiff",".png",".pdf",".jpg",".jpeg"), FUN=grep, filename)
  if(length(unlist(Test))>0){
    filename = strsplit(filename,"\\.")[[1]]
    filename = filename[-length(filename)]
    filename = paste(filename, collapse=".")
  }
  # loop through figures
  for( figI in 1:n_fig ){
    if( type[figI]=="png" ) png( filename=paste0(filename,suffix[figI],".png"), width=width, height=height, units=units, res=res[figI], ... )
    if( type[figI]=="pdf" ) pdf( file=paste0(filename,suffix[figI],".pdf"), width=width, height=height, ... )
    if( type[figI]=="tif" | type[figI]=="tiff" ) tiff( filename=paste0(filename,suffix[figI],".tif"), width=width, height=height, units=units, res=res[figI], ... )
    if( type[figI]=="jpg" | type[figI]=="jpeg" ) jpeg( filename=paste0(filename,suffix[figI],".jpg"), width=width, height=height, units=units, res=res[figI], ... )

    # if FUN is present, then do full plots
    if( !is.null(FUN) ){
      FUN()
      dev.off()
    }
  }
}
