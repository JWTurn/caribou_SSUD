#' @title Functions for PDEs
#' @export
#' @author Julie W. Turner

#' 
mod2UD <- function(modpath, envlayers, studyArea, pde.saveName = NULL, map.saveName = NULL){
  mod <- readRDS(modpath)
  mod.tab <- make_betas_tab(mod)
  pde <- make_pde(mod.tab, envlayers, studyArea = studyArea, saveName = pde.saveName)
  map.pde <- as.numeric(make_pde_map(pde, studyArea, saveName = map.saveName))
  return(map.pde)
}

#' prepares pde and feeds into `make_pde_map()`
make_pde <- function(mod, lsRasters, studyArea, saveName = NULL) {
  fixEff <- data.table(var = names(fixef(mod)[['cond']]), estimate = fixef(mod)[['cond']])
  selectionCoefs <- fixEff[var %like% '_end']
  selectionCoefs[,expr:=gsub('_end|I', '', var)]
  selectionCoefs[,expr:=gsub(':', '*', expr)]
  
  
  numerator <- lapply(1:nrow(selectionCoefs), 
                      FUN = function(x, dat = selectionCoefs, rasterEnviron = lsRasters){
                        2*as.double(selectionCoefs$estimate[[x]])*eval(parse(text = selectionCoefs$expr[[x]]), envir = rasterEnviron)}
  )
  
  numeratorRast <- mask(rast(numerator), studyArea)
  names(numeratorRast) <- selectionCoefs$expr
  numeratorSum <- exp(app(numeratorRast, fun = 'sum', na.rm = T))
  
  C <- terra::global(numeratorSum, sum, na.rm = T)
  pde <- numeratorSum/C[[1]]
  
  if(!is.null(saveName)){
    writeRaster(pde, 
                file.path(saveName), overwrite = T)
  }
  
  return(pde)
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

