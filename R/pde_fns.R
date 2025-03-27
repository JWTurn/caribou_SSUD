#' @title Functions for PDEs
#' @export
#' @author Julie W. Turner

#' 
mod2UD <- function(modpath, envlayers, studyArea, pde.saveName = NULL, map.saveName = NULL){
  mod <- readRDS(modpath)
  mod.tab <- make_betas_tab(mod)
  pde <- make_pde(mod.tab, envlayers, saveName = pde.saveName)
  map.pde <- as.numeric(make_pde_map(pde, studyArea, saveName = map.saveName))
  return(map.pde)
}

#' prepares pde and feeds into `make_pde_map()`
make_pde <- function(mod.tab, land, saveName = NULL){
  lf.cov<- (2*as.double(mod.tab[term %like% 'distlf_end', 
                                .(estimate)])*land$log_distlf)
  lfother.cov<- (2*as.double(mod.tab[term %like% 'distlf_other_end', 
                                     .(estimate)])*land$log_distlfother)
  tsf.cov<- (2*as.double(mod.tab[term %like% 'ts_fires_end', 
                                 .(estimate)])*land$log_tsf)
  tsh.cov<- (2*as.double(mod.tab[term %like% 'ts_harv_end', 
                                 .(estimate)])*land$log_tsh)
  needleleaf.cov <- (2*as.double(mod.tab[term %like% 'needleleaf_end', 
                                         .(estimate)])*land$prop_needleleaf)
  veg.cov <- (2*as.double(mod.tab[term %like% 'veg_end', 
                                  .(estimate)])*land$prop_veg)
  mixforest.cov <- (2*as.double(mod.tab[term %like% 'mixforest_end',
                                        .(estimate)])*land$prop_mixforest)
  wets.cov <- (2*as.double(mod.tab[term %like% 'wets_end', 
                                   .(estimate)])*land$prop_wets)
  disturb.cov <- (2*as.double(mod.tab[term %like% 'disturbance_end', 
                                      .(estimate)])*land$disturb)
  
  numerator <- exp(lf.cov + lfother.cov + 
                     tsf.cov + tsh.cov + 
                     needleleaf.cov + veg.cov + 
                     mixforest.cov + 
                     wets.cov +
                     disturb.cov)
  
  
  # the normalizing constant.
  C <- terra::global(numerator, sum, na.rm = T)
  pde <- numerator/C[[1]]
  
  if(!is.null(saveName)){
    writeRaster(pde, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde)
}


#' crops `make_pde()` to study area and descretizes to 10 bins
make_pde_map <- function(pde, sArea, saveName = NULL){
  pde.sa <- crop(pde, sArea, mask = T)
  
  
  breaks <- terra::global(pde.sa, quantile, na.rm = T, probs = seq(0,1,.1))
  v.breaks <- unname(breaks)
  t.breaks <- as.vector(t(v.breaks))
  pde.discrete <- terra::classify(pde.sa, t.breaks, include.lowest=TRUE, brackets=TRUE)
  
  if(!is.null(saveName)){
    writeRaster(pde.discrete, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde.discrete)
}

