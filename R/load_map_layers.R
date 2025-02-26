
# dPath <- '~/caribou_SSUD'
# #proplandPath <- 'https://drive.google.com/drive/u/1/folders/1bAuPuoZO9hgouAsXrmJ7YUhGGEIfn8XV'#2019
# #lfunpavedPath <- 'https://drive.google.com/file/d/1xLYK9evnyzL4Fo7aEIjIQKH8F_ykdhCo' # 2015
# #lfpavedPath <- 'https://drive.google.com/file/d/18vss_I4jMAgrdLFaYQE99O1Xa3O1dVrN'
# #disturbotherPath <- 'https://drive.google.com/file/d/1iJko2dGf9SmH5EiaGF8c0PA9arOtIhqB' #2015
# #firePath <- 'https://drive.google.com/file/d/1imnZT921zv1gIutwI2GCLl5HFLyKah2u' #2020
# harvPath <- 'https://drive.google.com/file/d/1yF-oIARALj6NRdCOn7a3gLZu8Gi9T2QK' #2020
# initDisturbyr <- 2015
# ts_else <- 100

load_map_layers <- function(proplandPath, lfunpavedPath, lfpavedPath, disturbotherPath, firePath,
                            harvPath, 
                            initDisturbyr, ts_else, dPath){
  prop.lc <- reproducible::prepInputs(url = extractURL(proplandPath), 
                                  #this isn't working    #destinaionPath = file.path(modPath, 'data', 'prepInputs'),
                                      fun = "terra::rast")
  
  
  prop_needleleaf <- prop.lc$needleleaf
  prop_mixforest <- prop.lc$deciduous + prop.lc$mixed + prop.lc$wet_treed
  prop_veg <- prop.lc$shrub + prop.lc$bryoids + prop.lc$herbs
  prop_wets <- prop.lc$wetland
  print("prop land prepped")
  
  linfeat_other <- reproducible::prepInputs(url = extractURL(lfunpavedPath), 
                                      #this isn't working    #destinaionPath = file.path(modPath, 'data', 'prepInputs'),
                                      fun = "terra::rast")
  #linfeat_other <- rast(file.path('data', 'raw-data', 'ECCC_disturbance', 
  #                                paste0('WB_lfother_', disturbyr, '_distto.tif')))
  disturb <- reproducible::prepInputs(url = extractURL(disturbotherPath), 
                                      #this isn't working    #destinaionPath = file.path(modPath, 'data', 'prepInputs'),
                                      fun = "terra::rast")
  #disturb <- rast(file.path('data', 'raw-data', 'ECCC_disturbance', 
  #                          paste0('WB_disturb_other_', disturbyr, '.tif')))
  
  
  
  fires <- reproducible::prepInputs(url = extractURL(firePath), 
                                      #this isn't working    #destinaionPath = file.path(modPath, 'data', 'prepInputs'),
                                      fun = "terra::rast")
  #fires <- rast(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', paste0('fires_', (disturbyr+5), '.tif')))
  
  lf.full <- reproducible::prepInputs(url = extractURL(lfpavedPath), 
                                    #this isn't working    #destinaionPath = file.path(modPath, 'data', 'prepInputs'),
                                    fun = "terra::rast")
  #lf.full <- rast(file.path('data', 'derived-data', 'distto_roadrail_500.tif'))
  lf <- terra::crop(lf.full, terra::ext(prop.lc))
  harv <- reproducible::prepInputs(url = extractURL(harvPath), 
                                      #this isn't working    #destinaionPath = file.path(modPath, 'data', 'prepInputs'),
                                      fun = "terra::rast")
  #harv <- rast(file.path('data', 'raw-data', 'WB_harv_1985-2020.tif'))
  
  lf_other <- terra::resample(linfeat_other, lf, method = 'average')
  lf_other.ext <- terra::extend(lf_other, terra::ext(lf))
  print("linear features prepped")
  
  disturb <- terra::resample(disturb, lf, method = 'max')
  disturb.ext <- terra::extend(disturb, terra::ext(lf))
  print("other anthro disturbances  prepped")
  
  harv <- terra::resample(harv, lf, method = 'max')
  harv.ext <- terra::extend(harv, terra::ext(lf))
  tsh <- (initDisturbyr + 5) - harv.ext
  tsh[is.na(tsh)] <- ts_else
  print("harvest prepped")
  
  
  fires.crop <- terra::resample(fires, lf, method = 'max')
  #names(land.brick) <- c("lf_dist", "lc")
  tsf <- (initDisturbyr + 5) - fires.crop
  tsf[is.na(tsf)] <- ts_else
  print("fires prepped")
  
  log_tsf <- log(tsf + 1)
  log_tsh <- log(tsh + 1)
  log_distlf <- log(lf + 1)
  log_distlfother <- log(lf_other.ext + 1)
  print("values transformed")
  
  
  land <- c(prop_veg, prop_needleleaf, prop_mixforest, prop_wets, log_tsf, log_tsh, log_distlf, 
            log_distlfother, disturb.ext)
  names(land) <- c('prop_veg', 'prop_needleleaf', 'prop_mixforest', 'prop_wets', 'log_tsf', 'log_tsh', 'log_distlf', 
                   'log_distlfother', 'disturb')
  return(land)
}