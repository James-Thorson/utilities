Hist_Fn = function( x, freq=TRUE, breaks="Sturges", y_buffer=0.05, ylim=NULL, main="", col="lightgrey", bty="o", ...){
  Hist = hist( x, breaks=breaks, plot=FALSE )
  if(is.null(ylim) & freq==TRUE) ylim = c(0, max(Hist$counts)*(1+y_buffer))
  if(is.null(ylim) & freq==FALSE) ylim = c(0, max(Hist$density)*(1+y_buffer))
  hist( x, breaks=breaks, freq=freq, ylim=ylim, col=col, main=main, ...)
  if( bty=="o" ) box()
  Return = list("Hist"=Hist, "ylim"=ylim)
  return( invisible(Return) )
}
