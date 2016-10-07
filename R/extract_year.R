
#' @export
extract_year = function( vec, format="%m/%d/%Y", units="year" ){
  new_vec = as.POSIXlt( vec, format=format )
  output = new_vec[[units]]
  return( output )
}

