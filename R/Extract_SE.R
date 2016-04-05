
Extract_SE = function( SD, Dim=NULL, Map=NULL, parname){
  if( is.null(dim)) Return = rep(NA, sum(parname==rownames(summary(SD))))
  if( !is.null(dim)) Return = array(NA, dim=Dim)
  if( !is.null(Map) ){
    if( parname %in% names(Map) ) Return[which(!is.na(Map[[parname]]))] = summary(SD)[which(parname==rownames(summary(SD))),'Std. Error']
    if( !(parname %in% names(Map)) ) Return[] = summary(SD)[which(parname==rownames(summary(SD))),'Std. Error']
  }
  if( is.null(Map) ) Return[] = summary(SD)[which(parname==rownames(summary(SD))),'Std. Error']
  return(Return)
}

