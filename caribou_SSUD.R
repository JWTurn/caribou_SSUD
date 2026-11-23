defineModule(sim, list(
  name = "caribou_SSUD",
  description = "This is a module to create a steady-state utilization distribution from an iSSA based on Potts & Schlägel 2020 ",
  keywords = c('steady-state utilization distribution', 'iSSA'),
  authors = structure(list(list(given = c("Julie", "W"), family = "Turner", role = c("aut", "cre"), email = "julwturner@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(caribou_SSUD = "0.0.1.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "caribou_SSUD.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9002)", "ggplot2", 'Require', 'reproducible', 'data.table', 'terra','sf',
                  'glmmTMB', 'broom.mixed', 'googledrive', 'viridis', 'RColorBrewer',
                  'tidyterra', 'patchwork'),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    # defineParameter("disturbYear", "integer", 2020, NA, NA,
    #                 paste0("This is the year the initial disturbance layers are from",
    #                        "and used for time since variables.",
    #                        "This parameter would need to be updated if use a different year of data.")),
    defineParameter("ts_else", "integer", 200, NA, NA,
                    paste0("Years since disturbance given to cells with no recorded disturbance (e.g.",
                           " never burned) in the time-since-fire and time-since-harvest layers. Use the",
                           " value the model was fit with (hsaCEtool's `ts_nodata`; 200 for the northern",
                           " mountain models).")),
    defineParameter("baselineYear", "integer", 2025L, NA, NA,
                    paste0("The year modelLand describes: its time-since layers (e.g. ts_fire_240) count",
                           " years back from it. Forecasts count from it too, and the simulation should",
                           " start in it (LandR's stands at the start are compared with modelLand's).",
                           " 2025 for the northern mountain modelLand; 2022 for the boreal NWT one.")),
    defineParameter("noEventYear", "list", list(fire = NA, harvest = NA), NA, NA,
                    paste0("How modelLand's time-since layers store cells with no fire (or harvest),",
                           " by source: NA = such cells are NA (or ts_else or more), and forecasts give",
                           " them ts_else; a year = they hold the years since that stand-in year (e.g.",
                           " 1900: 122 with baselineYear 2022), and forecasts give them the years since",
                           " it (year - noEventYear), as the model was fit. Either way they are left out",
                           " of coarser layers' fire years. E.g. the boreal NWT modelLand (baselineYear",
                           " 2022): list(fire = 1900, harvest = 1900).")),
    defineParameter("dynamicLayers", "list", NULL, NA, NA,
                    paste0("Dynamic mode: which of the layers the models read are forecast, and from",
                           " what: a named list, source = a regular expression matched against layer",
                           " names. Sources: 'forest' (LandR's cohortData and pixelGroupMap), 'fire'",
                           " (scfm's timeSinceFire), 'harvest' (no forecaster yet: the layer only",
                           " ages) and 'disturbance' (distances to anthroDisturbance_Generator's",
                           " features, see disturbanceClasses). A layer",
                           " that matches no pattern stays as in modelLand (e.g. vegetation). NULL = the",
                           " ecotype's default: northern mountain list(forest = '^prop_forest_', fire =",
                           " '^ts_fire_', harvest = '^ts_harv(est)?_', disturbance = '^dist_'); boreal",
                           " list(forest = '^prop_(needleleaf|mixedforest)$', fire = '^timeSinceFire$',",
                           " harvest = '^timeSinceHarvest$', disturbance = '^dist').")),
    defineParameter("windowRadius", "list", list("240" = 500), NA, NA,
                    paste0("Dynamic mode: radius (m) of the circle forest shares are averaged in, by",
                           " layer resolution (m), as hsaCEtool's `window_m`: list('240' = 500) = a",
                           " 240 m layer (e.g. prop_forest_240) is the mean of the 240 m cells within",
                           " 500 m. A resolution not listed gets no circle (e.g. prop_forest_1200 is the",
                           " mean of the 240 m cells in each 1200 m block). Fire layers get no circle.")),
    defineParameter("fireCoarse", "character", "mean", NA, NA,
                    paste0("Dynamic mode: how a fire layer coarser than the grid (e.g. ts_fire_1200 on",
                           " a 240 m grid) is built from the grid cells' fire years (unburned cells left",
                           " out): 'mean' = the mean fire year, as hsaCEtool makes them for the V2",
                           " northern mountain models (e.g. fullV2_to2020) and their modelLand; 'recent' =",
                           " the most recent fire in the block, as for the earlier models (e.g.",
                           " full_to2020). Use the rule the model was fit with.")),
    defineParameter("harvestCoarse", "character", "mean", NA, NA,
                    paste0("Dynamic mode: as fireCoarse, for harvest layers coarser than the grid (e.g.",
                           " ts_harv_1200): 'mean' = the mean harvest year (unharvested cells left out);",
                           " 'recent' = the most recent harvest in the block. Use the rule the model was",
                           " fit with.")),
    defineParameter("disturbanceClasses", "list", NULL, NA, NA,
                    paste0("Dynamic mode: which of anthroDisturbance_Generator's classes each distance",
                           " covariate (the layer name without resolution or `_adj`, e.g. dist_paved)",
                           " measures to, as 'sector/class' in disturbanceList; several classes are",
                           " combined. A forecast distance is the smaller of modelLand's and the distance",
                           " to the class's features that year (features are only ever added), so",
                           " features outside the study area or in no Generator class still count. A",
                           " covariate not listed stays as in modelLand. NULL = the ecotype's default:",
                           " northern mountain list(dist_paved = 'roads/paved', dist_unpaved =",
                           " 'roads/unpaved', dist_placer = 'mining/placer', dist_quartz =",
                           " 'mining/quartz', dist_othermines = 'mining/otherMines') (dist_lfother and",
                           " dist_otherpoly have no Generator class); boreal none.")),
    defineParameter("distanceCellSize", "numeric", 30, NA, NA,
                    paste0("Dynamic mode: cell size (m) distances are measured on, as hsaCEtool does: the",
                           " distance from each cell of this size to the nearest one a feature touches,",
                           " then the mean over each grid cell (e.g. a 240 m cell's 64 30 m cells). Must",
                           " divide the grid's cell size evenly; the grid's cell size = distances from",
                           " cell centres (faster, but not as the model's layers were made).")),
    defineParameter("distanceCap", "list", list(beyond_km = 20, set_to_km = 21), NA, NA,
                    paste0("Dynamic mode: how capped distances (layers named with `_adj`, e.g.",
                           " dist_paved_240_adj) are capped, as hsaCEtool's `distance_cap`: more than",
                           " beyond_km km gives set_to_km km.")),
    defineParameter("forestMinBiomass", "numeric", 0, 0, NA,
                    paste0("Dynamic mode: a LandR cell is forest when its stand has more than this",
                           " much tree biomass (g/m2, LandR's unit for B); 0 = any tree biomass.")),
    defineParameter("forestForecast", "character", "change", NA, NA,
                    paste0("Dynamic mode: how forest layers are forecast. 'change' = modelLand's layer",
                           " plus LandR's change since the start of the simulation (LandR now minus",
                           " LandR at the start), kept within 0-1; 'LandR' = LandR's value alone.")),
    defineParameter("forestTypes", "list",
                    list(prop_forest = c("conifer", "deciduous", "mixed"), prop_needleleaf = "conifer",
                         prop_mixedforest = c("deciduous", "mixed")), NA, NA,
                    paste0("Dynamic mode: the LandR forest types each forest covariate counts, by",
                           " covariate (the layer name without its resolution). Types: 'conifer'",
                           " (conifer share of the stand's biomass at or above the upper",
                           " mixedForestCutoff, 0.66), 'mixed' (between the cutoffs) and 'deciduous'",
                           " (at or below the lower cutoff, 0.33).")),
    defineParameter("sppEquivCol", "character", "LandR", NA, NA,
                    paste0("Dynamic mode: the column of sppEquiv that holds cohortData's species",
                           " codes, used to tell conifers from broadleaf species.")),
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
    defineParameter("simulationScale", "character", "global", NA, NA,
                    paste0("Which models to predict: 'global' (the global model, over studyAreaCaribou),",
                           " 'regional' (one model per region in `jurisdiction`, each over its area in",
                           " studyArea_juris), or 'both'. 'jurisdictional' is read as 'regional'.")),
    defineParameter("baselineSource", "character", NA, NA, NA,
                    paste0("A premade baseline, used when `pde` is not supplied: a folder, a .zip file,",
                           " or a URL (e.g. of a .zip on Google Drive) holding `pde_<model>.tif` (and",
                           " `pdeMap_<model>.tif` if normalizePDE) for each model, as this module writes",
                           " them. If NA, the baseline is made from modelLand and saved to outputPath.")),
    defineParameter("pdeArea", "character", c("fullArea", "regionalArea"), NA, NA,
                    paste0("The area each PDE is predicted, normalized (summed to 1) and binned into",
                           " its map over, so a map compares cells within that area. One choice for",
                           " the global model: 'fullArea' (studyAreaCaribou, the area the global model",
                           " was fit on) or 'fullReportingArea' (studyAreaReporting, which may reach",
                           " beyond the fitted area); and one for the regional models: 'regionalArea'",
                           " (the model's region in studyArea_juris) or 'regionalReportingArea' (the",
                           " part of studyAreaReporting inside the model's region). Only the choice for",
                           " the models run (simulationScale) is used. The 'Reporting' choices need",
                           " studyAreaReporting.")),
    defineParameter("seasonMaps", "character", NULL, NA, NA,
                    paste0("For a model with seasons (terms such as `seasonwinter:prop_forest_240_end`):",
                           " the seasons that each get a map of their own, named `<model>_<season>` (e.g.",
                           " global_winter, written as pde_global_winter.tif). A season's map uses the",
                           " terms without a season and that season's terms. NULL = all the model's",
                           " seasons; character(0) = none. Models without seasons ignore it.")),
    defineParameter("fullYear", "character", NULL, NA, NA,
                    paste0("For a model with seasons: the seasons combined into one full-year map,",
                           " named as the model (e.g. global, written as pde_global.tif): the seasons'",
                           " PDEs, weighted by seasonWeights, so it still sums to 1. NULL = all the",
                           " model's seasons; character(0) = no full-year map. Needs normalizePDE.",
                           " Models without seasons ignore it.")),
    defineParameter("seasonWeights", c("character", "numeric"), "length", NA, NA,
                    paste0("How the seasons in fullYear are weighted: 'length' = by days per season,",
                           " from iSSAseasons; 'equal'; or numbers named by season (e.g. c(winter = 2,",
                           " summer = 1)), scaled to sum to 1.")),
    defineParameter("normalizePDE", "logical", TRUE,
                    desc = "Whether to normalize PDE inside SSUD (FALSE for tmux workflow)"),
    defineParameter("jurisdiction", "character",c("BC","SK","MB", "ON", "NTYT", "NT"),
                    desc = "A list of jurisdictions to use for PDE creation"),
    defineParameter("ecotype", "character", "boreal",
                    desc = "Which ecotype of caribou were modelled: 'boreal' or 'northern_mountain'"),
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
                    "Google Drive folder ID for workflow outputs"),
    defineParameter("coefficientsModel", "character", "bioseason_fullV2_to2020", NA, NA,
                    "Name of the northern mountain iSSA model whose coefficients are read"),
    defineParameter("modelsFolderID", "character",
                    "https://drive.google.com/drive/folders/1NMhR1zwWc6jYjhKjd7o53VsSWJ9wP5ZM", NA, NA,
                    "Google Drive folder holding the northern mountain model zips")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = 'iSSAmodels', objectClass = 'list',
                 desc = 'A list of glmmTMB iSSA models by jurisdiction or global models (boreal ecotype)'),
    expectsInput(objectName = 'iSSAcoefficients', objectClass = 'data.table',
                 desc = paste('Fixed-effect coefficients of the northern mountain iSSA model: one row per',
                              'term, with columns `term` and `estimate` (others, e.g. those of hsaCEtool\'s',
                              '`<model>_fixed.csv`, are ignored). If not supplied, it is taken from',
                              '`iSSAmodels` when that is supplied, else read from `<coefficientsModel>_fixed.csv`',
                              'in `NMC_model_<coefficientsModel>.zip` in `modelsFolderID`.')),
    expectsInput(objectName = 'iSSAseasons', objectClass = 'data.table',
                 desc = paste('For a model with seasons and seasonWeights \'length\': the model\'s season',
                              'table, one row per season, with columns `season` (as in the terms, e.g.',
                              '`winter` for `seasonwinter`) and `days` (days per season), as hsaCEtool\'s',
                              '`<model>_seasons.csv` gives it. If not supplied, read from',
                              '`<coefficientsModel>_seasons.csv` in the model\'s zip, when it has one (models',
                              'fit before hsaCEtool wrote this table do not).')),
    expectsInput("cohortData", "data.table",
                 desc = paste("`data.table` with cohort-level information on age and biomass, by `pixelGroup` and ecolocation",
                              "(i.e., `ecoregionGroup`). If supplied, it must have the following columns: `pixelGroup` (integer),",
                              "`ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$), `age` (integer in years)")),
    expectsInput("pixelGroupMap", "SpatRaster",
                 desc = paste("A raster layer with `pixelGroup` IDs per pixel. Pixels are grouped" ,
                              "based on identical `ecoregionGroup`, `speciesCode`, `age` and `B` composition,",
                              "even if the user supplies other initial groupings (e.g., via the `Biomass_borealDataPrep`",
                              "module.")),
    expectsInput(objectName = "timeSinceFire", objectClass = "SpatRaster",
                 desc = paste("Dynamic mode: years since each cell last burned in the simulation, NA where",
                              "it has not burned (as scfmSpread keeps it), on the forecast grid. Fire",
                              "layers take a cell's fire year from it where that is more recent than",
                              "modelLand's and in baselineYear or later, so values it started with (e.g.",
                              "stand age) are not read as fires. If absent, fire layers only age.")),
    expectsInput(objectName = "disturbanceList", objectClass = "list",
                 desc = paste("Dynamic mode: anthroDisturbance_Generator's disturbances, a list by sector",
                              "of lists by class, each holding all of that class's features (existing and",
                              "simulated) as a SpatVector (or sf). Distance layers are measured to the",
                              "classes in disturbanceClasses. If absent, distance layers stay as in",
                              "modelLand.")),
    expectsInput(objectName = "sppEquiv", objectClass = "data.table",
                 desc = paste("Dynamic mode: LandR's species-equivalence table, with a `Type` column",
                              "(Conifer/Deciduous) and the species codes in column sppEquivCol. If",
                              "absent, LandR::sppEquivalencies_CA.")),
    expectsInput(objectName = "forestBaseline", objectClass = "list",
                 desc = paste("Dynamic mode, forestForecast 'change': LandR's forest at the start of the",
                              "simulation, a named list of 0/1 SpatRasters on the forecast grid, one per",
                              "forest covariate (e.g. prop_forest). If not supplied, made in init from",
                              "cohortData and pixelGroupMap.")),
    expectsInput(objectName = "modelLand", objectClass = "list",
                  desc = paste("The landscape layers the models read: a named list of SpatRasters on one",
                               "grid (or one multi-layer SpatRaster, made into that list), each named as",
                               "in the model's terms without `_end` (e.g.",
                               "`prop_forest_240` for the term `prop_forest_240_end`). The layers needed",
                               "are read from the terms, and a missing one stops the run, naming it.",
                               "Capped distances carry `_adj` (e.g. `dist_paved_240_adj`). If not",
                               "supplied, `modelLand.tif` is downloaded from outputFolderID.")),
    expectsInput(objectName = "studyAreaCaribou", objectClass = "SpatVector",
                 desc = paste("The area of the global model: a polygon of the full extent of the caribou",
                              "locations it was fit on. If not supplied, studyArea is used.")),
    expectsInput(objectName = "studyAreaReporting", objectClass = "SpatVector",
                 desc = paste("Optional: the area the results are reported for (e.g. a range buffer",
                              "clipped to a jurisdiction), a SpatVector or sf polygon. Used only when",
                              "pdeArea is 'fullReportingArea' or 'regionalReportingArea'.")),
    expectsInput(objectName = "rasterToMatch", objectClass = "SpatRaster",
                 desc = paste("Dynamic mode: the simulation's grid, e.g. the one LandR and scfm use. The",
                              "forecast layers are built on it. Every modelLand layer must be on it",
                              "(same extent, rows and columns, cell size and coordinate system), or the",
                              "run stops. If the model has layers coarser than the grid's cells (e.g.",
                              "ts_fire_1200 on a 240 m grid), the grid's top-left corner (xmin, ymax) must",
                              "also be a multiple of each of those resolutions, so the coarse blocks line",
                              "up with modelLand's. When cropping a template to a study area, snap the crop",
                              "outward to the coarsest template resolution, not just the model's, if the",
                              "resolutions nest (e.g. 4800 m for the northern mountain templates), so later",
                              "models with coarser layers fit too. If not supplied, modelLand's grid is used.")),
    expectsInput(objectName = "studyArea_juris", objectClass = "list",
                 desc = paste("Named list of SpatVectors, one area per region (e.g. jurisdiction), for the",
                              "regional models. Not needed when simulationScale is 'global'.")),
    expectsInput(objectName = "pde", objectClass = "list",
                 desc = paste("Baseline PDE (raw utility if normalizePDE is FALSE): a named list with one",
                              "SpatRaster per model ('global' and/or a region), or for a model with seasons,",
                              "per season map and the full-year map (see seasonMaps, fullYear). If not",
                              "supplied, read from baselineSource, else made from modelLand.")),
    expectsInput(objectName = "pdeMap", objectClass = "list",
                 desc = "Baseline binned PDE maps, named as `pde`; NULL if normalizePDE is FALSE."),
    expectsInput(objectName = "fixedSSUD", objectClass = "SpatRaster",
                 desc = paste("Dynamic mode: the layers the models read that do not change in forecasts",
                              "(those no dynamicLayers pattern matches), named as in the terms. If not",
                              "supplied, taken from modelLand."))
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "fixedSSUD", objectClass = "SpatRaster",
                  desc = "Dynamic mode: the layers that do not change in forecasts (see inputs)"),
    createsOutput(objectName = "forestBaseline", objectClass = "list",
                  desc = "Dynamic mode: LandR's forest at the start of the simulation (see inputs)"),
    createsOutput(objectName = 'pde', objectClass = 'list',
                  desc = 'Current (pre simulation) Raw pde calculation, one SpatRaster per map (see the input `pde`)'),
    createsOutput(objectName = 'pdeMap', objectClass = 'list',
                  desc = 'Current (pre simulation) binned map of pde UD for intensity of selection, one SpatRaster per map'),
    createsOutput(objectName = 'simPde', objectClass = 'list',
                  desc = 'Raw pde calculation from sim layers, by year, then one SpatRaster per map, as `pde`'),
    createsOutput(objectName = 'simPdeMap', objectClass = 'list',
                  desc = 'Forcast (simulated) binned map of pde UD for intensity of selection by year')

  )
))

doEvent.caribou_SSUD = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {


      # The baseline (sim$pde, sim$pdeMap) is made in .inputObjects(), or here if it needed objects
      # that other modules make; static mode stops there
      sim <- setupBaseline(sim)

      # a baseline this module made is saved, and registered as outputs, here rather than where it is
      # made: a cache hit for .inputObjects() would not write its files to this run's outputPath, and
      # simInit() replaces outputs(sim) with its `outputs` argument after .inputObjects()
      if (is.na(P(sim)$baselineSource) && !suppliedElsewhere("pde", sim, where = c("user", "initEvent"))) {
        outDir <- reproducible::checkPath(file.path(outputPath(sim), paste0('pde_', P(sim)$.studyAreaName)), create = T)
        sim <- registerPdeFiles(sim, writePDEs(sim$pde, sim$pdeMap, outDir))
        message("Baseline PDEs saved to: ", outDir)
      }

      if (P(sim)$simulationProcess == "dynamic") {
        # the forecast grid; stops if modelLand is not on it
        grid <- ssudGrid(sim$modelLand, sim$rasterToMatch)

        # LandR's forest at the start, which forest layers' change is measured from
        forestStems <- forecastForestStems(sim)
        if (P(sim)$forestForecast == "change" && length(forestStems) && is.null(sim$forestBaseline) &&
            !is.null(sim$cohortData) && !is.null(sim$pixelGroupMap)) {
          checkOnGrid(sim$pixelGroupMap, grid, "pixelGroupMap")
          if (time(sim) != P(sim)$baselineYear)
            message("The simulation starts in ", time(sim), ", not baselineYear (", P(sim)$baselineYear,
                    "): forest change is measured from LandR's stands in ", time(sim))
          sim$forestBaseline <- makeForestBaseline(forestStems, sim$cohortData, sim$pixelGroupMap,
                                                   P(sim), sim$sppEquiv)
        }

        # the sim's disturbance features at the start should all be in modelLand's distances
        if (!is.null(sim$disturbanceList)) checkDisturbanceStart(sim, grid)

        # always run simLayers
        sim <- scheduleEvent(sim, P(sim)$predictStartYear, "caribou_SSUD", "simLayers")
        sim <- scheduleEvent(sim, P(sim)$predictStartYear, "caribou_SSUD", "calcSimPde")

        # the last year, unless the predictionInterval already lands on it
        if (P(sim)$predictLastYear && (end(sim) - P(sim)$predictStartYear) %% P(sim)$predictionInterval != 0) {
          sim <- scheduleEvent(sim, end(sim), "caribou_SSUD", "simLayers")
          sim <- scheduleEvent(sim, end(sim), "caribou_SSUD", "calcSimPde")
        }
      }

    },

    simLayers = {

      thisYear <- as.integer(time(sim))
      key <- paste0("year", thisYear)
      message(paste0("Simulating landscape for ", thisYear))

      grid <- ssudGrid(sim$modelLand, sim$rasterToMatch)

      # the layers the forecast models read, and the source that forecasts each (NA = fixed)
      sources <- forecastSources(sim)

      # fixed layers from fixedSSUD, the rest from modelLand
      land <- sim$modelLand
      if (!is.null(sim$fixedSSUD)) land[names(sim$fixedSSUD)] <- as.list(sim$fixedSSUD)

      simLand <- buildForecastLayers(
        layers = names(sources), sources = sources, year = thisYear, modelLand = land, grid = grid,
        objs = list(cohortData = sim$cohortData, pixelGroupMap = sim$pixelGroupMap, sppEquiv = sim$sppEquiv,
                    timeSinceFire = sim$timeSinceFire, forestBaseline = sim$forestBaseline,
                    disturbanceList = sim$disturbanceList),
        p = P(sim)
      )

      # Bundle into env for make_pde eval()
      sim$simEnv[[key]] <- list2env(
        setNames(lapply(names(simLand), \(n) simLand[[n]]), names(simLand)),
        parent = baseenv()
      )

      # save layers
      outDir <- reproducible::checkPath(file.path(outputPath(sim), paste0('pde_', P(sim)$.studyAreaName), 'sims'), create = T)

      if(P(sim)$ecotype == 'boreal'){
        jur <- paste(P(sim)$jurisdiction, collapse = "")
        terra::writeRaster(simLand, file.path(outDir, paste0("pdeLayers_", jur, key, ".tif")), filetype = "GTiff", overwrite = TRUE)
      }

      if(P(sim)$ecotype == 'northern_mountain'){

        terra::writeRaster(simLand, file.path(outDir, paste0("pdeLayers_", P(sim)$.studyAreaName, "_", key, ".tif")), filetype = "GTiff", overwrite = TRUE)
      }

      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribou_SSUD", "simLayers")
    },

    calcSimPde = {
      thisYear <- as.integer(time(sim))
      key <- paste0("year", thisYear)
      message(paste0("Calculating simulated PDE for: ", thisYear))
      if (is.null(sim$simEnv[[key]]))
        stop("Missing sim$simEnv[['", key, "']]. Did simLayers run first?")


      outDir <- reproducible::checkPath(file.path(outputPath(sim), paste0('pde_', P(sim)$.studyAreaName), 'sims'), create = T)


      # forecast the same models, and maps, as the baseline
      mods <- pdeModels(pdeModelNames(P(sim)$simulationScale, P(sim)$jurisdiction), P(sim)$ecotype,
                        sim$iSSAmodels, sim$iSSAcoefficients)
      ud <- makePDEs(mods = mods, envLayers = sim$simEnv[[key]], studyAreaCaribou = sim$studyAreaCaribou,
                     studyArea_juris = sim$studyArea_juris, normalizePDE = P(sim)$normalizePDE,
                     savePath = outDir, key = key, studyAreaReporting = sim$studyAreaReporting,
                     pdeArea = P(sim)$pdeArea, seasonMaps = P(sim)$seasonMaps, fullYear = P(sim)$fullYear,
                     seasonWeights = P(sim)$seasonWeights, seasonTable = sim$iSSAseasons)
      sim$simPde[[key]] <- ud$pde
      sim$simPdeMap[[key]] <- ud$pdeMap
      sim <- registerPdeFiles(sim, ud$files, objectNames = c(pde = "simPde", pdeMap = "simPdeMap"))


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

  if (length(P(sim)$ecotype) != 1 || !P(sim)$ecotype %in% c("boreal", "northern_mountain")) {
    stop("ecotype must be one of 'boreal' or 'northern_mountain'")
  }
  for (prm in c("fireCoarse", "harvestCoarse")) {
    if (length(P(sim)[[prm]]) != 1 || !P(sim)[[prm]] %in% c("recent", "mean")) {
      stop(prm, " must be one of 'recent' or 'mean'")
    }
  }
  badSrc <- setdiff(names(P(sim)$noEventYear), c("fire", "harvest"))
  if (length(badSrc)) {
    stop("noEventYear takes the sources 'fire' and 'harvest', not: ", paste(badSrc, collapse = ", "))
  }
  if (length(P(sim)$forestForecast) != 1 || !P(sim)$forestForecast %in% c("change", "LandR")) {
    stop("forestForecast must be one of 'change' or 'LandR'")
  }
  if (length(P(sim)$distanceCellSize) != 1 || !isTRUE(P(sim)$distanceCellSize > 0)) {
    stop("distanceCellSize must be one cell size in m, more than 0")
  }
  cap <- P(sim)$distanceCap
  if (!is.list(cap) || !all(c("beyond_km", "set_to_km") %in% names(cap))) {
    stop("distanceCap must be a list with beyond_km and set_to_km, e.g. list(beyond_km = 20, set_to_km = 21)")
  }

  # the models the baseline (and forecasts) predict
  modelNames <- pdeModelNames(P(sim)$simulationScale, P(sim)$jurisdiction)
  # stops early for a pdeArea that does not fit the models run
  checkPdeArea(P(sim)$pdeArea, modelNames, suppliedElsewhere("studyAreaReporting", sim))

  # load caribou studyArea
  if (!suppliedElsewhere("studyAreaCaribou", sim)) {
    
    message("using studyArea for studyAreaCaribou maps")
    
    sim$studyAreaCaribou <- sim$studyArea
  }
  
  # load modelLand
  if (!suppliedElsewhere("modelLand", sim)) {

    message("Loading modelLand from cloud")

    modelLandFile <- tryCatch(
      prepInputs(
        targetFile = "modelLand.tif",
        destinationPath = dPath,
        fun = terra::rast,
        url = P(sim)$outputFolderID
      ),
      error = function(e) stop(
        "Could not download modelLand.tif from outputFolderID (", P(sim)$outputFolderID, "): ",
        conditionMessage(e), "\nSupply modelLand, or set outputFolderID to a Google Drive folder that",
        " holds modelLand.tif.", call. = FALSE)
    )

    # as.list() drops the layer names, which make_pde() needs
    sim$modelLand <- setNames(as.list(modelLandFile), names(modelLandFile))
  }

  # whether iSSAmodels was supplied, recorded before the boreal block below can fill it in
  modelsSupplied <- suppliedElsewhere("iSSAmodels", sim)

  # load iSSA models (the boreal ecotype; northern mountain uses iSSAcoefficients)
  if ("boreal" %in% P(sim)$ecotype && !modelsSupplied) {

    message("Loading iSSA models from cloud")

    modelFile <- tryCatch(
      prepInputs(
        targetFile = "iSSAmodels.RData",
        destinationPath = dPath,
        url = P(sim)$outputFolderID,
        fun = load(targetFile)
      ),
      error = function(e) stop(
        "Could not download iSSAmodels.RData from outputFolderID (", P(sim)$outputFolderID, "): ",
        conditionMessage(e), "\nSupply iSSAmodels, or set outputFolderID to a Google Drive folder that",
        " holds iSSAmodels.RData.", call. = FALSE)
    )

    load(modelFile)

    if (!exists("iSSAmodels")) {
      stop("iSSAmodels not found in loaded file")
    }

    sim$iSSAmodels <- iSSAmodels
  }

  # load the northern mountain model's coefficients: from a supplied model, else from the cloud
  if ("northern_mountain" %in% P(sim)$ecotype && !suppliedElsewhere("iSSAcoefficients", sim)) {
    if (modelsSupplied) {
      sim$iSSAcoefficients <- coef_tab(sim$iSSAmodels)
    } else {
      message("Loading iSSA coefficients from cloud")

      modelZip <- paste0("NMC_model_", P(sim)$coefficientsModel, ".zip")
      coefficients <- prepInputs(
        # the zip's own url: given the folder's, reproducible would download every zip in it
        url = drive_file_in_folder(P(sim)$modelsFolderID, modelZip),
        archive = modelZip,
        targetFile = paste0(P(sim)$coefficientsModel, "_fixed.csv"),
        destinationPath = dPath,
        fun = data.table::fread(targetFile)
      )

      sim$iSSAcoefficients <- coef_tab(coefficients)

      # the model's season table, if its zip has one
      if (!suppliedElsewhere("iSSAseasons", sim)) {
        sim$iSSAseasons <- readZipSeasons(file.path(dPath, modelZip), P(sim)$coefficientsModel)
      }
    }
  }

  # load jurisdictional study areas, needed only for regional models
  if (any(modelNames != "global") && !suppliedElsewhere("studyArea_juris", sim)) {
    jurisFile <- paste0("studyArea_juris_", P(sim)$.studyAreaName, ".rds")
    studyareaJuris <- tryCatch(
      prepInputs(
        targetFile = jurisFile,
        destinationPath = dPath,
        url = P(sim)$outputFolderID,
        fun = readStudyAreaJuris(targetFile)
      ),
      error = function(e) stop(
        "Could not download ", jurisFile, " from outputFolderID (", P(sim)$outputFolderID, "): ",
        conditionMessage(e), "\nSupply studyArea_juris (e.g. from caribouLocPrep), or set",
        " outputFolderID to a Google Drive folder that holds it.", call. = FALSE)
    )
    sim$studyArea_juris <- studyareaJuris

  }

  # the baseline and forecast setup; init makes what needs objects that other modules make later
  ready <- !is.null(sim$modelLand) &&
    !is.null(if (P(sim)$ecotype == "boreal") sim$iSSAmodels else sim$iSSAcoefficients)
  if (ready) {
    sim <- setupBaseline(sim, objects = c("fixedSSUD", "pde")[
      !c(suppliedElsewhere("fixedSSUD", sim), suppliedElsewhere("pde", sim))
    ])
  }

  return(invisible(sim))
}

## The baseline and forecast setup, made from modelLand and the models: the `objects` named, each
## only if not already in sim. Called from .inputObjects() when modelLand and the models are already
## there, and from init for anything still missing (e.g. when other modules make them).
setupBaseline <- function(sim, objects = c("fixedSSUD", "pde")) {

  # modelLand supplied as one multi-layer SpatRaster (e.g. YT_caribou's modelLand.tif): the named
  # list of its layers the module declares, as for the file .inputObjects() downloads
  if (inherits(sim$modelLand, "SpatRaster")) {
    sim$modelLand <- setNames(as.list(sim$modelLand), names(sim$modelLand))
  }

  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  modelNames <- pdeModelNames(P(sim)$simulationScale, P(sim)$jurisdiction)
  mods <- pdeModels(modelNames, P(sim)$ecotype, sim$iSSAmodels, sim$iSSAcoefficients)

  # forecast setup: the landscape layers that stay fixed (those no dynamicLayers pattern matches)
  if (P(sim)$simulationProcess == "dynamic") {

    if ("fixedSSUD" %in% objects && is.null(sim$fixedSSUD)) {
      sources <- forecastSources(sim, modelNames)
      sim$fixedSSUD <- makeFixedSSUD(sim$modelLand, names(sources)[is.na(sources)])
    }
  }

  # baseline PDEs and maps: from a premade baseline if one is given, else made from modelLand
  if ("pde" %in% objects && is.null(sim$pde)) {

    if (!is.na(P(sim)$baselineSource)) {
      message("Reading the premade baseline from ", P(sim)$baselineSource)
      # one map per model, or for a model with seasons, per season and the full year
      mapNames <- pdeMapNames(mods, P(sim)$seasonMaps, P(sim)$fullYear)
      ud <- readBaseline(baselineDir(P(sim)$baselineSource, dPath), mapNames, P(sim)$normalizePDE)

    } else {
      message("Building baseline PDEs for: ", paste(modelNames, collapse = ", "))

      envlayers <- list2env(setNames(lapply(names(sim$modelLand), function(n) sim$modelLand[[n]]), names(sim$modelLand)),
                            parent = baseenv())

      # stops naming any layer the models read that modelLand lacks; warns if one may need `_adj`
      checkModelLayers(mods, names(sim$modelLand), warnCapped = TRUE)

      # saved in init (see there)
      ud <- makePDEs(mods = mods,
                     envLayers = envlayers, studyAreaCaribou = sim$studyAreaCaribou,
                     studyArea_juris = sim$studyArea_juris, normalizePDE = P(sim)$normalizePDE,
                     studyAreaReporting = sim$studyAreaReporting, pdeArea = P(sim)$pdeArea,
                     seasonMaps = P(sim)$seasonMaps, fullYear = P(sim)$fullYear,
                     seasonWeights = P(sim)$seasonWeights, seasonTable = sim$iSSAseasons)
    }

    sim$pde <- ud$pde
    sim$pdeMap <- ud$pdeMap
  }

  return(invisible(sim))
}

## The layers the forecast models read (`modelNames`; by default the models run), named by layer,
## each with the dynamicLayers source that forecasts it, or NA (fixed, from modelLand)
forecastSources <- function(sim, modelNames = pdeModelNames(P(sim)$simulationScale, P(sim)$jurisdiction)) {
  mods <- pdeModels(modelNames, P(sim)$ecotype, sim$iSSAmodels, sim$iSSAcoefficients)
  layers <- unique(unlist(lapply(mods, modelLayerNames)))
  dyn <- P(sim)$dynamicLayers
  if (is.null(dyn)) dyn <- defaultDynamicLayers(P(sim)$ecotype)
  layerSources(layers, dyn)
}

## Warns when, at the start, distances to the sim's disturbance features are shorter than modelLand's:
## a disturbanceClasses class that holds features modelLand puts in another category
checkDisturbanceStart <- function(sim, grid) {
  sources <- forecastSources(sim)
  layers <- names(sources)[sources %in% "disturbance"]
  if (!length(layers)) return(invisible(NULL))
  classes <- P(sim)$disturbanceClasses
  if (is.null(classes)) classes <- defaultDisturbanceClasses(P(sim)$ecotype)
  shorter <- disturbanceMismatch(layers, sim$modelLand, grid, sim$disturbanceList, classes, P(sim))
  if (length(shorter))
    warning("At the start, distances to disturbanceList's features are shorter than modelLand's (by",
            " more than a grid cell) in some cells: ", paste0(names(shorter), " (",
            signif(100 * shorter, 2), "% of cells)", collapse = ", "), ". Check that disturbanceClasses",
            " links each covariate to the Generator classes of the same disturbance category.",
            call. = FALSE)
  invisible(shorter)
}

## The forest covariates (e.g. prop_forest) of the layers the forecast builds from LandR
forecastForestStems <- function(sim) {
  sources <- forecastSources(sim)
  unique(parseLayerName(names(sources)[sources %in% "forest"])$stem)
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
