
#' @export
scan_admb_par = function( filename ){
  Scan = scan( filename, what="character", quiet=TRUE, skip=1)  # , sep="\n"

  optionswarnorig = options()$warn
  options('warn'=-1)

  Return = NULL
  for(i in 1:length(Scan)){
    if( is.na(as.numeric(Scan[i])) ){
      if( i==1 || !is.na(as.numeric(Scan[i-1])) ){
        Name = Scan[i]
      }else{
        Name = paste0(Name," ",Scan[i])
      }
    }else{
      New = as.numeric(Scan[i])
      names(New) = Name
      Return = c(Return,New)
    }
  }
  options('warn'=optionswarnorig)
  
  return(Return)
}
