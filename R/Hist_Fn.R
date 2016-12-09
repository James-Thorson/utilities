
#' Plot histogram
#'
#' \code{Hist_Fn} plots one or more histograms on a single panel
#'
#' @param x, a list of entries to transform to a histogram and plot
#' @inheritParams hist
#' @param y_buffer, a buffer at the top of the highest histogram
#' @param xlim, bounds for x-axis (default is detected from \code{x})
#' @param ylim, bounds for y-axis (default is detected from \code{x})
#' @inheritParams plot
#' @param ... arguments passed to \code{hist}


#' @export
Hist_Fn = function( x, freq=TRUE, breaks="Sturges", y_buffer=0.05, ylim=NULL, xlim=NULL, main="", col="lightgrey", bty="o", add=FALSE, ...){
  # Modify default inputs
  if( is.list(x)==FALSE ) x = list( x )
  if( length(col)==1 & length(x)>1 ) col = rep(col,length(x))

  # Figure out ylim
  Hist = NULL
  if(is.null(ylim)) ylim = c(NA, 0)
  if(is.null(xlim)){
    xlim_to_use = c(NA, NA)
  }else{
    xlim_to_use = xlim
  }
  for(i in 1:length(x)){
    Hist[[i]] = hist( x[[i]], breaks=breaks, plot=FALSE )
    if(is.na(ylim[1]) & freq==TRUE) ylim[2] = max(ylim[2], max(Hist[[i]]$counts)*(1+y_buffer) )
    if(is.na(ylim[1]) & freq==FALSE) ylim[2] = max(ylim[2], max(Hist[[i]]$density)*(1+y_buffer) )
    if(is.null(xlim)) xlim_to_use = range( c(xlim_to_use,Hist[[i]]$breaks), na.rm=TRUE)
  }
  if(is.na(ylim[1])) ylim[1] = 0

  # Plot
  for(i in 1:length(x)){
    hist( x[[i]], breaks=breaks, freq=freq, ylim=ylim, xlim=xlim_to_use, col=col[i], main=main, add=ifelse(i==1,add,TRUE), ...)
  }
  if( bty=="o" ) box()

  # Return stuff
  Return = list("Hist"=Hist, "ylim"=ylim)
  return( invisible(Return) )
}
