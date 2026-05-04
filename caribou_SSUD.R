defineModule(sim, list(
  name = "caribou_SSUD",
  description = "This is a module to create a steady-state utilization distribution from an iSSA based on Potts & Schlägel 2020 ",
  keywords = c('steady-state utilization distribution', 'iSSA'),
  authors = structure(list(list(given = c("Julie", "W"), family = "Turner", role = c("aut", "cre"), email = "julwturner@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(caribou_SSUD = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "caribou_SSUD.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9002)", "ggplot2", 'Require', 'reproducible', 'data.table', 'terra','sf',
                  'glmmTMB', 'broom.mixed', 'viridis', 'RColorBrewer',
                  'tidyterra', 'patchwork'),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    # defineParameter("disturbYear", "integer", 2020, NA, NA,
    #                 paste0("This is the year the initial disturbance layers are from",
    #                        "and used for time since variables.",
    #                        "This parameter would need to be updated if use a different year of data.")),
    defineParameter("ts_else", "integer", 100, NA, NA,
                    paste0("This is the value to fill in NAs in time since disturbance layers",
                           "This parameter would need to be updated if want a different default year.")),
    defineParameter("histLandYears", "integer", 2010:2023, NA, NA,
                    paste0("This is the year range we use past (not simulated) landscape layers.")),
    defineParameter("simulationProcess", "character", "static", NA, NA,
                    paste0("Should the simulation use LandR (dynamic) or land cover map (static)?",
                           "defaults to static")),
    defineParameter("predictionInterval", "numeric", 5, NA, NA, "Time between predictions"),
    defineParameter("predictStartYear", "numeric", 2025, NA, NA,
                    paste0("The first year to start forecasted simulations if dynamic.",
                           " This is because we start forecasting landcovers earlier.")),
    defineParameter("predictLastYear", "logical", TRUE, NA, NA,
                    paste0("If last year of simulation is not multiple of",
                           " predictionInterval, should it predict for the last year too?")),
    defineParameter("simulationScale", "character", "jurisdictional", NA, NA,
                    paste0("Should the simulation use jurisdictional or global scale?",
                           "defaults to jurisdictional")),
    defineParameter("normalizePDE", "logical", TRUE,
                    desc = "Whether to normalize PDE inside SSUD (FALSE for tmux workflow)"),
    defineParameter("jurisdiction", "character",c("BC","SK","MB", "ON", "NTYT", "NT"),
                    desc = "A list of jurisdictions to use for PDE creation"),
    defineParameter("ecotype", "character",c("boreal", 'northern_mountain'),
                    desc = "Which ecotype of caribou were modelled"),
    #####
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),

    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("outputFolderID", "character", "https://drive.google.com/drive/folders/1CRSY_tJucL3E8VDgUYEv9WXuG1nKImH3", NA, NA,
                    "Google Drive folder ID for workflow outputs")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = 'iSSAmodels', objectClass = 'list',
                 desc = 'A list of glmmTMB iSSA models by jurisdiction or global models'),
    expectsInput("cohortData", "data.table",
                 desc = paste("`data.table` with cohort-level information on age and biomass, by `pixelGroup` and ecolocation",
                              "(i.e., `ecoregionGroup`). If supplied, it must have the following columns: `pixelGroup` (integer),",
                              "`ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$), `age` (integer in years)")),
    expectsInput("pixelGroupMap", "SpatRaster",
                 desc = paste("A raster layer with `pixelGroup` IDs per pixel. Pixels are grouped" ,
                              "based on identical `ecoregionGroup`, `speciesCode`, `age` and `B` composition,",
                              "even if the user supplies other initial groupings (e.g., via the `Biomass_borealDataPrep`",
                              "module.")),
    expectsInput(objectName = "modelLand", objectClass = "list",
                  desc = "A list of rasters aligned with the model outputs to be used in UD modules"),
    expectsInput(objectName = "studyAreaCaribou", objectClass = "SpatVector",
                 desc = "a single polygon derived from the full extent of caribou locations used for the global model"),
    expectsInput(objectName = "rasterToMatch_SSUD", objectClass = "spatRaster",
                 desc = "Raster to match for SSUD")
  ),
  outputObjects = bindrows(
    createsOutput("timeSinceFire", "SpatRaster",
                  "If dynamic, map of time since last burn used in model - with pixels that never burn receiving NA"),
    createsOutput(objectName = 'pde', objectClass = 'SpatRaster',
                  desc = 'Current (pre simulation) Raw pde calculation'),
    createsOutput(objectName = 'pdeMap', objectClass = 'SpatRaster',
                  desc = 'Current (pre simulation) binned map of pde UD for intensity of selection'),
    createsOutput(objectName = 'simPde', objectClass = 'SpatRaster',
                  desc = 'Raw pde calculation from sim layers'),
    createsOutput(objectName = 'simPdeMap', objectClass = 'list',
                  desc = 'Forcast (simulated) binned map of pde UD for intensity of selection by year')

  )
))

doEvent.caribou_SSUD = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {


      # Baseline always runs
      sim <- scheduleEvent(sim, time(sim), "caribou_SSUD", "buildBaselineSSUD")

      if (Par$simulationProcess == "dynamic") {
        if(Par$ecotype == 'boreal'){
          sim$timeSinceFire <- postProcess(sim$modelLand$timeSinceFire,
                                           to = sim$rasterToMatch)
        }

        if(Par$ecotype == 'northern_mountain'){
          sim$timeSinceFire <- postProcess(sim$modelLand$timeSinceFire,
                                           to = sim$rasterToMatch)
        }


        # always run simLayers
        sim <- scheduleEvent(sim, P(sim)$predictStartYear, "caribou_SSUD", "simLayers")
        sim <- scheduleEvent(sim, P(sim)$predictStartYear, "caribou_SSUD", "calcSimPde")

      }

      if (Par$predictLastYear) {
        sim <- scheduleEvent(sim, end(sim), "caribou_SSUD", "simLayers")
      }


    },

    buildBaselineSSUD = {

      message("Building baseline landscape")

      # envlayers for PDE
      envlayers <- list2env(setNames(lapply(names(sim$modelLand), function(n) sim$modelLand[[n]]),names(sim$modelLand)),parent = baseenv())

      # PDEs
      if(Par$ecotype == 'boreal'){
        makeBorealPDE(mods = sim$iSSAmodels, studyAreaJuris = sim$studyArea_juris,  juris = Par$jurisdiction,
                      envLayers = envlayers, normalizePDE = Par$normalizePDE, simulationScale = Par$simulationScale)
      }

      if(Par$ecotype == 'northern_mountain'){
        ud <- mod2UD(
          mod = sim$iSSAmodels,
          envlayers = envlayers,
          studyArea = sim$studyAreaCaribou,
          normalize = Par$normalizePDE
        )

        if (Par$normalizePDE) {
          sim$pde <- ud$pde
          sim$pdeMap <- as.numeric(ud$map)

        } else {
          sim$pde <- ud$utility
          sim$pdeMap <- NULL
        }
      }

      outDir <- reproducible::checkPath(file.path(outputPath(sim), paste0('pde_', Par$.studyAreaName)), create = T)
      terra::writeRaster(sim$pde, file.path(outDir, paste0('pde_', Par$.studyAreaName, '.tif')), overwrite = T)

      if(!is.null(sim$pdeMap)){
        terra::writeRaster(sim$pdeMap, file.path(outDir, paste0('pdeMap_', Par$.studyAreaName, '.tif')), overwrite = T)
      }


      message("Baseline PDE complete")

      # forecast setup
      message("Setting up baseline landscape for forecasting")

      if (is.null(sim$baselineYear)){
        sim$baselineYear <- max(Par$histLandYears)
      }

  # sim$fixedSSUD <- list2env(list(
  #   prop_veg = envlayers$prop_veg,
  #   prop_wets = envlayers$prop_wets,
  #   distpaved = envlayers$distpaved,
  #   distunpaved = envlayers$distunpaved,
  #   distpolys = envlayers$distpolys,
  #   timeSinceHarvest0 = envlayers$timeSinceHarvest
  # ), parent = baseenv())

      # To be passed to fire forecasts for historic fire if dynamic
      if(Par$simulationProcess == 'dynamic') {

        if(Par$ecotype == 'boreal'){
          sim$fixedSSUD <- list2env(list(
            prop_veg = envlayers$prop_veg,
            prop_wets = envlayers$prop_wets,
            distpaved = envlayers$distpaved,
            distunpaved = envlayers$distunpaved,
            distpolys = envlayers$distpolys,
            timeSinceHarvest0 = envlayers$timeSinceHarvest
          ), parent = baseenv())


        }

        if(Par$ecotype == 'northern_mountain'){
          # sim$fixedSSUD <- list2env(list(
          #   prop_veg_250 = envlayers$prop_veg_250,
          #   dist_lf_250 = envlayers$dist_lf_250,
          #   #distunpaved = envlayers$distunpaved,
          #   dist_poly_250 = envlayers$dist_poly_250,
          #   timeSinceHarvest0 = envlayers$timeSinceHarvest
          # ), parent = baseenv())

          sim$fixedSSUD <- rast(
            list(
              prop_veg_250 = sim$modelLand$prop_veg_250,
            dist_lf_250 = sim$modelLand$dist_lf_250,
            #distunpaved = envlayers$distunpaved,
            dist_poly_250 = sim$modelLand$dist_poly_250
            )
          )

        }


        # TODO for when Harvest module integrated...
        #sim$timeSinceHarvest <- envlayers$timeSinceHarvest
      }

    },

    simLayers = {

      thisYear <- as.integer(time(sim))
      key <- paste0("year", thisYear)
      message(paste0("Simulating landscape for ", thisYear))

      template <- sim$rasterToMatch_SSUD # TODO this is a quick fix, need to do an if exists inputObject



      # ensure baseline fixed layers were created
      if (is.null(sim$fixedSSUD) || is.null(sim$baselineYear))
        stop("Missing sim$fixedSSUD / sim$baselineYear. Did buildBaselineSSUD run?")

      # Fixed layers from baseline

      fixedResamp <- terra::resample(sim$fixedSSUD,  template, method = "average")
      # prop_veg  <- terra::resample(sim$fixedSSUD$prop_veg,  template, method = "average")
      # prop_wets <- terra::resample(sim$fixedSSUD$prop_wets, template, method = "average")
      #
      # distpaved   <- terra::resample(sim$fixedSSUD$distpaved,   template, method = "average")
      # distunpaved <- terra::resample(sim$fixedSSUD$distunpaved, template, method = "average")
      # distpolys   <- terra::resample(sim$fixedSSUD$distpolys,   template, method = "average")

      # Dynamic forest layers from LandR
      reclassForest <- reclassifyCohortData(
        cohortData = sim$cohortData,
        sppEquivCol = "LandR",
        pixelGroupMap = sim$pixelGroupMap,
        mixedForestCutoffs = c(0.33, 0.66)
      )

      ft <- reclassForest$`forest type`

      if(Par$ecotype == 'boreal'){
        # Needleleaf
        needle_mask <- terra::classify(
          ft, rcl = matrix(c(210, 1), ncol = 2, byrow = TRUE), others = 0
        )
        prop_needleleaf <- terra::resample(needle_mask, template, method = "average")
        names(prop_needleleaf) <- "prop_needleleaf"

        # Mixedforest
        mixed_mask <- terra::classify(
          ft, rcl = matrix(c(220, 1, 230, 1), ncol = 2, byrow = TRUE),
          others = 0
        )
        prop_mixedforest <- terra::resample(mixed_mask, template, method = "average")
        names(prop_mixedforest) <- "prop_mixedforest"

        forests <- c(prop_needleleaf, prop_mixedforest)
      }

      if(Par$ecotype == 'northern_mountain'){
        # all forest
        forest_mask <- terra::classify(
          ft, rcl = matrix(c(210, 1, 220, 1, 230, 1), ncol = 2, byrow = TRUE), others = 0
        )
        prop_forest_250 <- terra::resample(forest_mask, template, method = "average")
        names(prop_forest_250) <- "prop_forest_250"

        forests <- c(prop_forest_250)

      }



      message("Reclassifying forest layers")
      # Dynamic timeSinceFire from scfmSpread
      tsf <- reproducible::postProcess(sim$timeSinceFire, to = template)
      tsf[is.na(tsf)] <- P(sim)$ts_else

      message("Update timeSinceFire using scfm")

      # age forward timeSinceHarvest
      dt <- thisYear - sim$baselineYear

      if(Par$ecotype == 'boreal'){
        names(tsf) <- "timeSinceFire"

        tsh <- terra::resample(sim$fixedSSUD$timeSinceHarvest0, template, method = "average")
        tsh <- tsh + dt
        tsh[is.na(tsh)] <- P(sim)$ts_else
        names(tsh) <- "timeSinceHarvest"
        message("Moving harvest forward")

        tsRasts <- c(tsf, tsh)
      }

      if(Par$ecotype == 'northern_mountain'){
        names(tsf) <- "ts_fires_250"

        tsRasts <- tsf
        #TODO update poly disturbances for harvest eventually
        # tsh <- terra::resample(sim$fixedSSUD$timeSinceHarvest0, template, method = "average")
        # tsh <- tsh + dt
        # tsh[is.na(tsh)] <- P(sim)$ts_else
        # names(tsh) <- "timeSinceHarvest"
        # message("Moving harvest forward")
      }


      # Bundle into env for make_pde eval()
      simLand <- c(fixedResamp, forests, tsRasts)
      # simLand needs to be exported to workflowOutputs for normalization model

      sim$simEnv[[key]] <- list2env(
        setNames(lapply(names(simLand), \(n) simLand[[n]]), names(simLand)),
        parent = baseenv()
      )
      #message("needleleaf mean: ", terra::global(prop_needleleaf, "mean", na.rm=TRUE)[1,1])
      #message("mixed mean: ", terra::global(prop_mixedforest, "mean", na.rm=TRUE)[1,1])
      message("tsf mean: ", terra::global(tsf, "mean", na.rm=TRUE)[1,1])
      #message("tsh mean: ", terra::global(tsh, "mean", na.rm=TRUE)[1,1])


      # save layers
      outDir <- reproducible::checkPath(file.path(outputPath(sim), paste0('pde_', Par$.studyAreaName), 'sims'), create = T)

      if(Par$ecotype == 'boreal'){
        jur <- paste(Par$jurisdiction, collapse = "")
        terra::writeRaster(simLand, file.path(outDir, paste0("pdeLayers_", jur, key, ".tif")), overwrite = TRUE)
      }

      if(Par$ecotype == 'northern_mountain'){

        terra::writeRaster(simLand, file.path(outDir, paste0("pdeLayers_", Par$.studyAreaName, "_", key, ".tif")), overwrite = TRUE)
      }

      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribou_SSUD", "simLayers")
    },

    calcSimPde = {
      thisYear <- as.integer(time(sim))
      key <- paste0("year", thisYear)
      message(paste0("Calculating simulated PDE for: ", thisYear))
      if (is.null(sim$simEnv[[key]]))
        stop("Missing sim$simEnv[['", key, "']]. Did simLayers run first?")


      outDir <- reproducible::checkPath(file.path(outputPath(sim), paste0('pde_', Par$.studyAreaName), 'sims'), create = T)


      # boreal
      if(Par$ecotype == 'boreal'){
        makeBorealPDE(mods = sim$iSSAmodels, studyAreaJuris = sim$studyArea_juris,  juris = Par$jurisdiction,
                      envLayers = sim$simEnv[[key]], normalizePDE = Par$normalizePDE, simulationScale = Par$simulationScale,
                      isSim = TRUE, key = key, savePath = outDir)
      }

      # northern mountain
      if(Par$ecotype == 'northern_mountain'){
        ud <- mod2UD(
          mod = sim$iSSAmodels,
          envlayers = sim$simEnv[[key]],
          studyArea = sim$studyAreaCaribou,
          normalize = Par$normalizePDE
        )

        if (Par$normalizePDE) {
          sim$simPde[[key]] <- ud$pde
          sim$simPdeMap[[key]] <- as.numeric(ud$map)

        } else {
          sim$simPde[[key]]<- ud$utility
          sim$simPdeMap[[key]] <- NULL
        }

        terra::writeRaster(sim$simPde[[key]], file.path(outDir, paste0("pde_", Par$.studyAreaName, '_', key, '.tif')), overwrite = TRUE)
        terra::writeRaster(sim$simPdeMap[[key]], file.path(outDir, paste0("pdeMap_", Par$.studyAreaName, '_', key, '.tif')), overwrite = TRUE)

      }


      message(paste0("Finished creating PDE for: ", thisYear))
      message(paste0("Outputs saved to: ", outDir))
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribou_SSUD", "calcSimPde")
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
  #return(sim)
}

.inputObjects <- function(sim) {

  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # load modelLand
  if (!suppliedElsewhere("modelLand", sim)) {

    message("Loading modelLand from cloud")

    modelLandFile <- prepInputs(
      targetFile = "modelLand.tif",
      destinationPath = dPath,
      fun = terra::rast,
      url = Par$outputFolderID
    )

    sim$modelLand <- as.list(modelLandFile)
  }

  # load iSSA models
  if (!suppliedElsewhere("iSSAmodels", sim)) {

    message("Loading iSSA models from cloud")

    modelFile <- prepInputs(
      targetFile = "iSSAmodels.RData",
      destinationPath = dPath,
      url = Par$outputFolderID,
      fun = load(targetFile)
    )

    load(modelFile)

    if (!exists("iSSAmodels")) {
      stop("iSSAmodels not found in loaded file")
    }

    sim$iSSAmodels <- iSSAmodels
  }

  # load jurisdictional study areas
  if (!suppliedElsewhere("studyArea_juris", sim)) {
    studyareaJuris<- prepInputs(
      targetFile = paste0("studyArea_juris_", Par$.studyAreaName, ".rds"),
      destinationPath = dPath,
      url = Par$outputFolderID,
      fun = terra::vect(x = targetFile)
    )
    sim$studyArea_juris <- studyArea_juris

  }

  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
