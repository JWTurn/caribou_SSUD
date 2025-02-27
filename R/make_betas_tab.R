#' @title make table of beta values
#' @export
#' @author Julie W. Turner
#' 
make_betas_tab <- function(mod){
  mod2 <- up2date(mod)
  sum <- broom.mixed::tidy(mod2, effect = 'fixed')
  as.data.table(sum)[,.(term, estimate)]
}