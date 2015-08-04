bundlelist <-
function( charvector ){
  Return = list()
  for(i in 1:length(charvector)){
    Tmp = list( get(charvector[[i]]) )
    names(Tmp) = charvector[[i]]
    Return = c( Return, Tmp )
  }
  return(Return)
}
