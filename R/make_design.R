

#' Make design matrix from lme4 formula
#'
#' \code{make_design} generates a design-matrix and grouping vector from a formula representing fixed and random effects
#'
#' @param formula formula using \code{lme4} style, e.g., \code{~ 1 + (0+SST|group)} for a random-slope model
#' @param data data-frame used to parse the formula
#' @param random_spline Boolean indicating whether to treat basis-spline coefficients as a grouping factor
#' @param ... additional arguments passed to \code{model.frame}
#'
#' @return Tagged-list of containing a design-matrix \code{X_ij} and the grouping variable
#'         \code{Group_j} where \code{Group_j[X]=NA} indicates that column \code{X} of \code{X_ij}
#'         corresponds to a fixed effect, whereas \code{Group_j[X]} having a positive integer
#'         indicates that column \code{X} is associated with that group of random effects.

#' @export
make_design <-
function( formula,
          data,
          random_spline = FALSE,
          coerce_to_dense = TRUE,
          ... ){

  library(splines)

  # Change defaults to allow NAs in data without messing up number of rows
  na.action.orig = options()$na.action
  on.exit( options(na.action=na.action.orig) )
  options(na.action='na.pass')

  # Build fixed effects
  f2 = lme4::nobars(formula)
  X_ij = model.matrix(f2, data)
  Group_j = rep(NA,ncol(X_ij))

  # Dealw with splines
  if( random_spline==TRUE ){
    which_bs = grep( pattern="bs(", x=colnames(X_ij), fixed=TRUE )
    if( length(which_bs) >= 2 ) Group_j[which_bs] = 1
  }

  # Build random effects
  f1 = lme4::findbars(formula)
  if( length(f1) > 0 ){
    fr = model.frame(formula=lme4::subbars(formula), data=data) #, ...)
    m2 = lme4::mkReTrms(f1, fr=fr)
    for( k in seq_len(length(m2$Ztlist)) ){
      Z_ij = Matrix::t(m2$Ztlist[[k]])
      X_ij = cbind( X_ij, Z_ij )
      Group_j = c( Group_j, rep(max(c(0,Group_j),na.rm=TRUE)+1,ncol(Z_ij)) )
    }
  }

  # Return
  if( coerce_to_dense==TRUE ){
    X_ij = as.matrix(X_ij)
  }
  return(list("X_ij"=X_ij,"Group_j"=Group_j))
}
