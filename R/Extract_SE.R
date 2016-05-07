
Extract_SE = function( SD, Dim=NULL, Map=NULL, parname, columns=2){
  # Size of return
  if( is.null(Dim)) Return = array(NA, dim=c(sum(parname==rownames(summary(SD))),length(columns)) )
  if( !is.null(Dim)) Return = array(NA, dim=c(Dim,length(columns)) )
  # Fixed or not
  if( !is.null(Map) ){
    if( parname %in% names(Map) ) Return[which(!is.na(Map[[parname]]))] = summary(SD)[which(parname==rownames(summary(SD))),columns]
    if( !(parname %in% names(Map)) ) Return[] = summary(SD)[which(parname==rownames(summary(SD))),columns]
  }
  if( is.null(Map) ) Return[] = summary(SD)[which(parname==rownames(summary(SD))),columns]
  # Return things
  return(Return)
}

