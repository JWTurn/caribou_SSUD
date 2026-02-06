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
                    "Should caching of events or module be used?")

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
    expectsInput(objectName = "landscapeYearly", objectClass = "SpatRaster",
                 desc = "spatRaster stack of the yearly landscape layers"),

    expectsInput(objectName = "landscape5Yearly", objectClass = "SpatRaster",
                 desc = "SpatRaster stacks of the 5 year interval anthropogenic landscape layers"),
    expectsInput(objectName = "studyAreaCaribou", objectClass = "SpatVector",
                 desc = "a single polygon derived from the full extent of caribou locations used for the global model"),
    expectsInput(objectName = "studyArea_juris", objectClass = "list",
                 desc = "Named list of jurisdiction-specific study areas (SpatVector) used for jurisdictional models")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    # createsOutput(objectName = 'issaBetasTable', objectClass = 'data.table',
    #               desc = 'data.table of modelled betas'),
    # createsOutput(objectName = 'pdeLand', objectClass = 'SpatRaster',
    #              desc = 'Stack of all layers for pde calculation'),
    createsOutput(objectName = 'pde', objectClass = 'SpatRaster',
                  desc = 'Current (pre simulation) Raw pde calculation'),
    createsOutput(objectName = 'pdeMap', objectClass = 'SpatRaster',
                  desc = 'Current (pre simulation) binned map of pde UD for intensity of selection'),
    # createsOutput(objectName = 'simPdeLand', objectClass = 'SpatRaster',
    #               desc = 'Stack of all layers for simulated pde calculation'),
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
      # Schedule baseline pde
      sim <- scheduleEvent(sim, time(sim),"caribou_SSUD","buildBaselineSSUD")

      # schedule future event(s)
      if (P(sim)$simulationProcess == "dynamic") {
        sim <- scheduleEvent(sim, P(sim)$predictStartYear, "caribou_SSUD", "simLayers")
        sim <- scheduleEvent(sim, P(sim)$predictStartYear, "caribou_SSUD", "calcSimPde")
      }
      if (P(sim)$predictLastYear) {
        sim <- scheduleEvent(sim, end(sim), "caribou_SSUD", "simLayers")
      }

    },

    buildBaselineSSUD = {
      # build baseline covariate environment once (latest observed)
      message("Building baseline landscape")
      envlayers <- composeLandscape(sim$landscapeYearly, sim$landscape5Yearly)

      # baseline PDE maps: jurisdiction models over their jurisdiction study areas,
      # global model over combined study area (to be changed)
      UDlist <- list()
      UDlist <- lapply(names(sim$iSSAmodels), function(mn) {
        sa <- getStudyAreaForModel(
          modelName = mn,
          studyArea = sim$studyAreaCaribou,
          studyArea_juris = sim$studyArea_juris
        )

        mod2UD(
          mod = sim$iSSAmodels[[mn]],
          envlayers = envlayers,
          studyArea = sa
        )
      })
      names(UDlist) <- names(sim$iSSAmodels)

      sim$pde    <- lapply(UDlist, `[[`, "pde")
      sim$pdeMap <- lapply(UDlist, `[[`, "map")
      message("Baseline pde is comeplete")
      # baseline setup for forecast years (store fixed layers once)
      message("Setting up baseline landscape for forcasting")
      sim$baselineYear <- max(as.integer(gsub("\\D", "", names(sim$landscapeYearly))), na.rm = TRUE)

      sim$fixedSSUD <- list2env(list(
        prop_veg = envlayers$prop_veg,
        prop_wets = envlayers$prop_wets,
        distpaved = envlayers$distpaved,
        distunpaved = envlayers$distunpaved,
        distpolys = envlayers$distpolys,
        timeSinceHarvest0 = envlayers$timeSinceHarvest
      ), parent = baseenv())
    },

    simLayers = {
      browser()
      thisYear <- as.integer(time(sim))
      key <- paste0("Year", thisYear)
      message(paste0("Simulating landscape for ", thisYear))
      template <- sim$rasterToMatch_extendedLandscapeCoarse

      # ensure baseline fixed layers were created
      if (is.null(sim$fixedSSUD) || is.null(sim$baselineYear))
        stop("Missing sim$fixedSSUD / sim$baselineYear. Did buildBaselineSSUD run?")

      # Fixed layers from baseline
      prop_veg  <- terra::resample(sim$fixedSSUD$prop_veg,  template, method = "average")
      prop_wets <- terra::resample(sim$fixedSSUD$prop_wets, template, method = "average")

      distpaved   <- terra::resample(sim$fixedSSUD$distpaved,   template, method = "average")
      distunpaved <- terra::resample(sim$fixedSSUD$distunpaved, template, method = "average")
      distpolys   <- terra::resample(sim$fixedSSUD$distpolys,   template, method = "average")

      # Dynamic forest layers from LandR
      reclassForest <- reclassifyCohortData(
        cohortData = sim$cohortData,
        sppEquivCol = "LandR",
        pixelGroupMap = sim$pixelGroupMap,
        mixedForestCutoffs = c(0.33, 0.66)
      )
      reclassForest$`forest type`[is.na(reclassForest$`forest type`)] <- 0

      # prop_needleleaf <- terra::resample(
      #   terra::classify(reclassForest$`forest type`,
      #                   rcl = matrix(c(210, 220, 230, 1,
      #                                  0,   0,   0, 0), ncol = 2, byrow = FALSE)),
      #   template, method = "average"
      # )
      # names(prop_needleleaf) <- "prop_needleleaf"
      #
      # prop_mixedforest <- terra::resample(
      #   terra::classify(reclassForest$`forest type`,
      #                   rcl = matrix(c(210, 220, 230, 0,
      #                                  1,   1,   1, 1), ncol = 2, byrow = FALSE)),
      #   template, method = "average"
      # )
      # names(prop_mixedforest) <- "prop_mixedforest"
      ft <- reclassForest$`forest type`

      prop_needleleaf <- terra::resample((ft %in% c(210,220,230)) * 1, template, method="average")
      names(prop_needleleaf) <- "prop_needleleaf"

      prop_mixedforest <- terra::resample((ft %in% c(240,250,260,270)) * 1, template, method="average")
      names(prop_mixedforest) <- "prop_mixedforest"
      message("Reclassifying forest layers")
      # Dynamic timeSinceFire from scfmSpread
      tsf <- terra::resample(sim$timeSinceFire, template, method = "average")
      tsf[is.na(tsf)] <- P(sim)$ts_else
      names(tsf) <- "timeSinceFire"
      message("Update timeSinceFire using scfm")

      # age forward timeSinceHarvest
      dt <- thisYear - sim$baselineYear
      tsh <- terra::resample(sim$fixedSSUD$timeSinceHarvest0, template, method = "average")
      tsh <- tsh + dt
      tsh[is.na(tsh)] <- P(sim)$ts_else
      names(tsh) <- "timeSinceHarvest"
      message("Moving harvest forward")
      # Bundle into env for make_pde eval()
      simLand <- c(prop_veg, prop_wets,
                   prop_needleleaf, prop_mixedforest,
                   tsf, tsh,
                   distpaved, distunpaved, distpolys)

      sim$simEnv[[key]] <- list2env(
        setNames(lapply(names(simLand), \(n) simLand[[n]]), names(simLand)),
        parent = baseenv()
      )
      message("needleleaf mean: ", terra::global(prop_needleleaf, "mean", na.rm=TRUE)[1,1])
      message("tsf mean: ", terra::global(tsf, "mean", na.rm=TRUE)[1,1])
      message("tsh mean: ", terra::global(tsh, "mean", na.rm=TRUE)[1,1])
      # save layers
      terra::writeRaster(simLand, file.path(outputPath(sim), paste0("pdeLayers_", key, ".tif")), overwrite = TRUE)

      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribou_SSUD", "simLayers")
    },

    calcSimPde = {

      thisYear <- as.integer(time(sim))
      key <- paste0("Year", thisYear)
      message(paste0("Calculating simulated PDE for: ", thisYear))
      if (is.null(sim$simEnv[[key]]))
        stop("Missing sim$simEnv[['", key, "']]. Did simLayers run first?")

      UDlist <- lapply(names(sim$iSSAmodels), function(mn) {

        sa <- getStudyAreaForModel(
          modelName = mn,
          studyArea = sim$studyAreaCaribou,
          studyArea_juris = sim$studyArea_juris
        )

        mod2UD(
          mod       = sim$iSSAmodels[[mn]],
          envlayers = sim$simEnv[[key]],
          studyArea = sa,
          prefix    = paste0(mn, "_", thisYear)
        )
      })
      names(UDlist) <- names(sim$iSSAmodels)

      # flatten: store BOTH pde + map (same as baseline pattern)
      sim$simPde[[key]]    <- lapply(UDlist, `[[`, "pde")
      sim$simPdeMap[[key]] <- lapply(UDlist, `[[`, "map")

      message(paste0("Finished creating PDE for: ", thisYear))
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

  # ! ----- EDIT BELOW ----- ! #
  # TODO give a smaller area
  # if (!suppliedElsewhere("studyArea", sim)){
  #   sim$studyArea <- Cache(prepInputs,
  #                                url = extractURL("studyArea_4maps"),
  #                                destinationPath = dataPath(sim),
  #                                targetFile = "studyArea_4maps.shp",
  #                                alsoExtract = "similar", fun = "terra::vect",
  #                                userTags = c("object:studyArea_4maps"))
  # }

  # if (!suppliedElsewhere("rasterToMatch", sim)){
  #   sim$rasterToMatch <- terra::rast(studyArea, res = c(250, 250), vals = 1)
  # }
  #
  # if (!suppliedElsewhere("rasterToMatchCoarse", sim)){
  #   sim$rasterToMatchCoarse <- terra::aggregate(sim$rasterToMatch, fact = 2)
  # }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
