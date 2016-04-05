
Extract_SE = function( SD, Report=NULL, Map=NULL, parname){
  if( is.null(Report)) Return = rep(NA, sum(parname==rownames(summary(SD))))
  if( !is.null(Report)) Return = array(NA, dim=dim(Report[[parname]]))
  if( !is.null(Map) ){
    if( parname %in% names(Map) ) Return[which(!is.na(Map[[parname]]))] = summary(SD)[which(parname==rownames(summary(SD))),'Std. Error']
    if( !(parname %in% names(Map)) ) Return[] = summary(SD)[which(parname==rownames(summary(SD))),'Std. Error']
  }
  if( is.null(Map) ) Return[] = summary(SD)[which(parname==rownames(summary(SD))),'Std. Error']
  return(Return)
}

