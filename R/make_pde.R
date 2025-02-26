#' @param mod.tab glmmTMB output procesed by make_betas_tab
#' @param land raster stack
#' @param saveName name to save raster as; defaults to NULL and doesn't save
#' @title Make stable-state utilization distribution based on partial differential equations (PDE)
#' @name make_pde
#' @author Julie W. Turner & Alex Jack
#' @export
# cite Potts and Schlaegel
make_pde <- function(mod.tab, land, saveName = NULL){
  ls_names <- names(land)
  ls_name_assoc <- lapply(ls_names, function(x) data.frame(land = x, substr = split_pde_column_name(x)))
  # now run lapply to this list
  covs <- lapply(ls_name_assoc, function(x) 2*as.double(mod.tab[term %like% x[2],
                                                                .(estimate)])*land[[as.character(x[1])]])
  numerator <- 0
  for(i in 1:length(covs)){
    numerator <- numerator + covs[[i]]  
  }
  numerator <- terra::rast(exp(numerator))
  
  # the normalizing constant.
  C <- terra::global(numerator, sum, na.rm = T)
  pde <- numerator/C[[1]]
  if(!is.null(saveName)){
    writeRaster(pde,
                file.path(derived, saveName), overwrite = T)
  }
  return(pde)
}



make_pde_map <- function(pde, sArea, saveName = NULL){
  pde.sa <- crop(pde, sArea, mask = T)
  plot(pde.sa)
  
  breaks <- global(pde.sa, quantile, na.rm = T, probs = seq(0,1,.1))
  v.breaks <- unname(breaks)
  t.breaks <- as.vector(t(v.breaks))
  pde.discrete <- classify(pde.sa, t.breaks, include.lowest=TRUE, brackets=TRUE)
  
  if(!is.null(saveName)){
    writeRaster(pde.discrete, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde.discrete)
}