#' @title Functions for PDEs
#' @export
#' @author Julie W. Turner


# mod2UD <- function(mod, envlayers, studyArea, pde.saveName = NULL, map.saveName = NULL){
#   if(is.character(mod))
#     {mod <- readRDS(mod)}
#
#   pde <- make_pde(mod, lsRasters = envlayers, studyArea = studyArea, saveName = pde.saveName)
#   map.pde <- as.numeric(make_pde_map(pde, saveName = map.saveName))
#   return(map.pde)
# }
mod2UD <- function(mod, envlayers, studyArea, prefix = NULL, normalize = TRUE) {

  if (is.character(mod)) {
    mod <- readRDS(mod)
  }

  U <- make_pde(
    mod = mod,
    lsRasters = envlayers,
    studyArea = studyArea,
    normalize = normalize
  )

  if (normalize) {

    map.pde <- make_pde_map(U)

    return(list(pde = U,map = map.pde))

  } else {

    return(list(utility = U))
  }
}

#' fixed-effect coefficients of an iSSA model, as a `data.table` with columns `term` and `estimate`
#' `mod` is a glmmTMB model, or a table with those two columns (others, e.g. the `se`, `z` and `p`
#' of hsaCEtool's `<model>_fixed.csv`, are dropped), or a list holding one of these
coef_tab <- function(mod) {
  if (is.list(mod) && !is.object(mod)) {
    if (length(mod) != 1L) {
      stop('coef_tab() needs one model or coefficients table; the list holds ', length(mod), call. = FALSE)
    }
    mod <- mod[[1]]
  }
  if (is.data.frame(mod)) {
    miss <- setdiff(c('term', 'estimate'), names(mod))
    if (length(miss)) {
      stop('the coefficients table lacks column(s): ', paste(miss, collapse = ', '), call. = FALSE)
    }
    return(data.table(term = as.character(mod$term), estimate = as.numeric(as.character(mod$estimate))))
  }
  make_betas_tab(mod)
}

#' prepares pde and feeds into `make_pde_map()`
# make_pde <- function(mod, lsRasters, studyArea, saveName = NULL) {
#   fixEff <- data.table(var = names(fixef(mod)[['cond']]), estimate = fixef(mod)[['cond']])
#   selectionCoefs <- fixEff[var %like% '_end']
#   selectionCoefs[,expr:=gsub('_end|I', '', var)]
#   selectionCoefs[,expr:=gsub(':', '*', expr)]
#
#
#   numerator <- lapply(1:nrow(selectionCoefs),
#                       FUN = function(x, dat = selectionCoefs, rasterEnviron = lsRasters){
#                         2*as.double(selectionCoefs$estimate[[x]])*eval(parse(text = selectionCoefs$expr[[x]]), envir = rasterEnviron)}
#   )
#
#   numeratorRast <- mask(rast(numerator), studyArea)
#   names(numeratorRast) <- selectionCoefs$expr
#   numeratorSum <- exp(app(numeratorRast, fun = 'sum', na.rm = T))
#
#   C <- terra::global(numeratorSum, sum, na.rm = T)
#   pde <- numeratorSum/C[[1]]
#
#   if(!is.null(saveName)){
#     writeRaster(pde,
#                 file.path(saveName), overwrite = T)
#   }
#
#   return(pde)
# }
#' `mod` is a glmmTMB iSSA model, or a table of its fixed-effect coefficients (see `coef_tab()`)
make_pde <- function(mod, lsRasters, studyArea, normalize = TRUE) {

  fixEff <- coef_tab(mod)
  setnames(fixEff, 'term', 'var')

  selectionCoefs <- fixEff[var %like% '_end']
  # drop '_end', and the I of an I() wrapper, not a capital I inside a covariate's name
  selectionCoefs[, expr := gsub('_end', '', gsub('\\bI\\(', '(', var, perl = TRUE))]
  selectionCoefs[, expr := gsub(':', '*', expr)]

  numerator <- lapply(
    1:nrow(selectionCoefs),
    function(x) {
      2 * as.double(selectionCoefs$estimate[[x]]) *
        eval(parse(text = selectionCoefs$expr[[x]]),
             envir = lsRasters)
    }
  )

  numeratorRast <- mask(rast(numerator), studyArea)
  names(numeratorRast) <- selectionCoefs$expr

  U <- exp(app(numeratorRast, fun = 'sum', na.rm = TRUE))

  if (normalize) {

    C <- terra::global(U, sum, na.rm = TRUE)[[1]]
    pde <- U / C

    return(pde)

  } else {

    return(U)

  }
}

#' crops `make_pde()` to study area and descretizes to 10 bins
make_pde_map <- function(pde, saveName = NULL){
 # pde.sa <- crop(pde, sArea, mask = T)


  breaks <- terra::global(pde, quantile, na.rm = T, probs = seq(0,1,.1))
  v.breaks <- unname(breaks)
  t.breaks <- as.vector(t(v.breaks))
  pde.discrete <- terra::classify(pde, t.breaks, include.lowest=TRUE, brackets=TRUE)

  if(!is.null(saveName)){
    writeRaster(pde.discrete,
                file.path(derived, saveName), overwrite = T)
  }

  return(pde.discrete)
}

