
#' Make initial values list for tmbstan
#'
#' \code{make_tmbstan_inits} makes a list of initial values for use by \code{tmbsta::tmbstan}
#'
#' @param obj Compiled TMB object, either before or after optimization
#' @param cores the number of cores to initialize
#' @param amount difference plus or minus from \code{obj$env$last.par} used for a uniform jitter
#' @param lower optional vector of lower bounds for fixed effects
#' @param upper optional vector of upper bounds for fixed effects

#' @export
make_tmbstan_inits = function( obj, cores, amount=1,
  upper=rep(Inf,length(obj$par)), lower=rep(-Inf,length(obj$par)) ){

  init_list = list()
  for(core in 1:cores){
    init_list[[core]] = obj$env$last.par + runif(length(obj$env$last.par), min=-amount, max=amount)
    for( parnum in seq_along(obj$par) ){
      num = match(names(obj$par)[parnum], names(obj$env$last.par) )
      rand = runif(n=1, min=max(obj$env$last.par[num]-amount,lower[parnum]), max=min(obj$env$last.par[num]+amount,upper[parnum]))
      init_list[[core]][num] = rand
    }
  }
  return(init_list)
}
