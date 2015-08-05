
scan_admb_par = function( filename ){
  Scan = scan( filename, what="character")  # , sep="\n"

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
  
  return(Return)
}
