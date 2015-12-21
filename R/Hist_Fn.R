Hist_Fn = function( x, freq=TRUE, breaks="Sturges", y_buffer=0.05, ...){
  Hist = hist( x, breaks=breaks, plot=FALSE )
  Ylim = c(0, max(Hist$counts)*(1+y_buffer))
  hist( x, breaks=breaks, freq=freq, ylim=Ylim, ...)
  Return = list("Hist"=Hist, "Ylim"=Ylim)
  return( invisible(Return) )
}
