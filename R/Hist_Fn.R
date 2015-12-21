Hist_Fn = function( x, freq=TRUE, breaks="Sturges", y_buffer=0.05, ylim=NULL, ...){
  Hist = hist( x, breaks=breaks, plot=FALSE )
  if(is.null(ylim) & freq==TRUE) ylim = c(0, max(Hist$counts)*(1+y_buffer))
  if(is.null(ylim) & freq==FALSE) ylim = c(0, max(Hist$density)*(1+y_buffer))
  hist( x, breaks=breaks, freq=freq, ylim=ylim, ...)
  Return = list("Hist"=Hist, "ylim"=ylim)
  return( invisible(Return) )
}
