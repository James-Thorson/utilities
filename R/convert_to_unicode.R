
#' Convert character to unicode
#'
#' \code{convert_to_unicode} converts a single character to a unicode (e.g., dash to en-hyphen)
#'
#' @param char character-string in which to find a character to replace
#' @param pattern character to replace
#' @param replacement replacement (presumably using unicode formatting)

#' @export
convert_to_unicode = function( char, pattern="-", replacement="\u2013" ){
  paste0( ifelse(substr(char,start=1,stop=1)==pattern,replacement,""),  gsub(pattern=pattern,replacement="",x=char) )
}
