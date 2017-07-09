

#' List fixed and random effects
#'
#' \code{list_parameters} lists all fixed and random effects
#'
#' @param Obj Compiled TMB object
#' @return Return Tagged-list of fixed and random effects (returned invisibly)

#' @export
list_parameters = function( Obj ){
  Return = list()
  if( length(Obj$env$random)>0 ){
    Return[["Fixed_effects"]] = names(Obj$env$last.par[-Obj$env$random])
    message("Number of fixed effects:")
    print( table(Return[["Fixed_effects"]]) )


    Return[["Random_effects"]] = names(Obj$env$last.par[Obj$env$random])
    message("Number of random effects:")
    print( table(Return[["Random_effects"]]) )
  }else{
    Return[["Fixed_effects"]] = names(Obj$env$last.par)
    message("Number of parameters:")
    print( table(Return[["Fixed_effects"]]) )
  }
  return( invisible(Return) )
}
