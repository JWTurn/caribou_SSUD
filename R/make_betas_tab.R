#' @title make table of beta values
#' @export
#' @author Julie W. Turner
#' 
make_betas_tab <- function(mod){
  sum <- broom.mixed::tidy(mod, effect = 'fixed')
  as.data.table(sum)[,.(term, estimate)]
}