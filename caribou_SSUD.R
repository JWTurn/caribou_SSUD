## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
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
                  'glmmTMB', 'viridis', 'RColorBrewer',
                  'tidyterra', 'patchwork'),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("disturbYear", "integer", 2020, NA, NA,
                    paste0("This is the year the initial disturbance layers are from", 
                           "and used for time since variables.",
                           "This parameter would need to be updated if use a different year of data.")),
    defineParameter("ts_else", "integer", 100, NA, NA,
                    paste0("This is the value to fill in NAs in time since disturbance layers",
                           "This parameter would need to be updated if want a different default year.")),
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
    expectsInput(objectName = 'studyArea_4maps', objectClass = 'SpatVector', 
                 desc = 'Study area of telemetry data + herd areas', 
                 sourceURL = 'https://drive.google.com/file/d/1XduunieEoZLcNPQphGXnKG7Ql9MF1bme/view?usp=share_link'),
    expectsInput(objectName = 'propLand', objectClass = 'SpatRaster', 
                 desc = 'Proportion of landcover rasters based on NTEMS LCC. Default if not provided are 2019 at 500m resolution', 
                 sourceURL = 'https://drive.google.com/drive/u/1/folders/1bAuPuoZO9hgouAsXrmJ7YUhGGEIfn8XV'),
    expectsInput(objectName = 'lfUnpaved', objectClass = 'SpatRaster', 
                 desc = 'Distance to unpaved linear features. Default if not provided are 2015 (from ECCC data) at 500m resolution', 
                 sourceURL = 'https://drive.google.com/file/d/1xLYK9evnyzL4Fo7aEIjIQKH8F_ykdhCo'),
    expectsInput(objectName = 'lfPaved', objectClass = 'SpatRaster', 
                 desc = 'Distance to paved linear features. Default if not provided are 2015 (from ECCC data) at 500m resolution', 
                 sourceURL = 'https://drive.google.com/file/d/18vss_I4jMAgrdLFaYQE99O1Xa3O1dVrN'),
    expectsInput(objectName = 'disturbOther', objectClass = 'SpatRaster', 
                 desc = 'Rasterized polygonal disturbance, not including harvest or fires. Default if not provided are 2015 (from ECCC data) at 500m resolution', 
                 sourceURL = 'https://drive.google.com/file/d/1iJko2dGf9SmH5EiaGF8c0PA9arOtIhqB'),
    expectsInput(objectName = 'historicalFires', objectClass = 'SpatRaster', 
                 desc = 'Rasterized year of last fire based on NBAC. Default if not provided is 2020 at 30m resolution', 
                 sourceURL = 'https://drive.google.com/file/d/1imnZT921zv1gIutwI2GCLl5HFLyKah2u'),
    expectsInput(objectName = 'harv', objectClass = 'SpatRaster', 
                 desc = 'Rasterized year of last harvest based on NBAC. Default if not provided is 2020 at 30m resolution', 
                 sourceURL = 'https://drive.google.com/file/d/1yF-oIARALj6NRdCOn7a3gLZu8Gi9T2QK'),
    expectsInput(objectName = 'modelOutput', objectClass = 'glmmTMB', 
                 desc = '`glmmTMB` model output for iSSA. If none provided, default is the global 2015 WBI model.', 
                 sourceURL = 'https://drive.google.com/file/d/16V6bMUC42GdHVx0Kep0IotItBxyKQLYN/view?usp=share_link'),
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = 'pdeLand', objectClass = 'SpatRaster', 
                  desc = 'Stack of all layers for pde calculation'),
    createsOutput(objectName = 'pde', objectClass = 'SpatRaster', 
                  desc = 'Raw pde calculation'),
    createsOutput(objectName = 'pdeMap', objectClass = 'SpatRaster', 
                  desc = 'Binned map of pde UD for intensity of selection')
    
  )
))

doEvent.caribou_SSUD = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      # prep land layers for UD calculation/map
      sim$pdeLand <- load_map_layers(propLC = sim$propLand, lfOther = sim$lfUnpaved, 
                                     lfPaved = sim$lfPaved, disturbOther= sim$disturbOther, 
                                     fire = sim$historicalFires, harv = sim$harv, 
                                     year = P(sim)$disturbYear, ts_else = P(sim)$ts_else)
      
      # create table of betas from model
      sim$issaBetasTable <- make_betas_tab(sim$issaModel)
      
      # calculate the pde UD following Potts and Schlaegel
      sim$pde <- make_pde(sim$issaBetasTable, sim$pdeLand) 
      
      # make binned map of pde
      sim$pdeMap <- make_pde_map(sim$pde, sim$studyArea_4maps)
      
      writeRaster(sim$pdeMap, file.path(outputPath(sim), paste0('pde_global2015','.tif')),
                  overwrite =TRUE)
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "caribou_SSUD", "plot")
      # sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "caribou_SSUD", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      plot(sim$pdeMap, breaks=0:10)
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "caribou_SSUD", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    # save = {
    #   # ! ----- EDIT BELOW ----- ! #
    #   # do stuff for this event
    # 
    #   # e.g., call your custom functions/methods here
    #   # you can define your own methods below this `doEvent` function
    # 
    #   # schedule future event(s)
    # 
    #   # e.g.,
    #   # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "caribou_SSUD", "save")
    # 
    #   # ! ----- STOP EDITING ----- ! #
    # },
    # event1 = {
    #   # ! ----- EDIT BELOW ----- ! #
    #   # do stuff for this event
    # 
    #   # e.g., call your custom functions/methods here
    #   # you can define your own methods below this `doEvent` function
    # 
    #   # schedule future event(s)
    # 
    #   # e.g.,
    #   # sim <- scheduleEvent(sim, time(sim) + increment, "caribou_SSUD", "templateEvent")
    # 
    #   # ! ----- STOP EDITING ----- ! #
    # },
    # event2 = {
    #   # ! ----- EDIT BELOW ----- ! #
    #   # do stuff for this event
    # 
    #   # e.g., call your custom functions/methods here
    #   # you can define your own methods below this `doEvent` function
    # 
    #   # schedule future event(s)
    # 
    #   # e.g.,
    #   # sim <- scheduleEvent(sim, time(sim) + increment, "caribou_SSUD", "templateEvent")
    # 
    #   # ! ----- STOP EDITING ----- ! #
    # },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
  #return(sim)
}

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
 
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}
### template for save events
# Save <- function(sim) {
#   # ! ----- EDIT BELOW ----- ! #
#   # do stuff for this event
#   sim <- saveFiles(sim)
# 
#   # ! ----- STOP EDITING ----- ! #
#   return(invisible(sim))
# }

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
# Event1 <- function(sim) {
#   # ! ----- EDIT BELOW ----- ! #
#   # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
#   # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
#   # sim$event1Test2 <- 999 # for dummy unit test
# 
#   # ! ----- STOP EDITING ----- ! #
#   return(invisible(sim))
# }
# 
# ### template for your event2
# Event2 <- function(sim) {
#   # ! ----- EDIT BELOW ----- ! #
#   # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
#   # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
#   # sim$event2Test2 <- 777  # for dummy unit test
# 
#   # ! ----- STOP EDITING ----- ! #
#   return(invisible(sim))
# }

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }
  
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  # ! ----- EDIT BELOW ----- ! #
  
  if (!suppliedElsewhere("studyArea_4maps", sim)){
    sim$studyArea_4maps <- Cache(prepInputs,
                                 url = extractURL("studyArea_4maps"),
                                 destinationPath = dataPath(sim),
                                 targetFile = "studyArea_4maps.shp",  
                                 alsoExtract = "similar", fun = "terra::vect",
                                 userTags = c("object:studyArea_4maps"))
  }
  
  if (!suppliedElsewhere("propLand", sim)){
    sim$propLand <- Cache(prepInputs,
                          url = extractURL("propLand"),
                          destinationPath = file.path(dataPath(sim), '500grid'),
                          fun = "terra::rast",
                          userTags = c("object:propLand"))
  }
  
  if (!suppliedElsewhere("lfPaved", sim)){
    sim$lfPaved <- Cache(prepInputs,
                         url = extractURL("lfPaved"),
                         destinationPath = dataPath(sim),
                         fun = "terra::rast",
                         userTags = c("object:lfPaved"))
  }
  
  if (!suppliedElsewhere("lfUnpaved", sim)){
    sim$lfUnpaved <- Cache(prepInputs,
                          url = extractURL("lfUnpaved"),
                          destinationPath = dataPath(sim),
                          fun = "terra::rast",
                          userTags = c("object:lfUnaved"))
  }
  
  if (!suppliedElsewhere("disturbOther", sim)){
    sim$disturbOther <- Cache(prepInputs,
                              url = extractURL("disturbOther"),
                              destinationPath = dataPath(sim),
                              fun = "terra::rast",
                              userTags = c("object:disturbOther"))
  }
  
  if (!suppliedElsewhere("historicalFires", sim)){
    sim$historicalFires <- Cache(prepInputs,
                                 url = extractURL("historicalFires"),
                                 destinationPath = dataPath(sim),
                                 fun = "terra::rast",
                                 userTags = c("object:historicalFires"))
  }
  
  if (!suppliedElsewhere("harv", sim)){
    sim$harv <- Cache(prepInputs,
                      url = extractURL("harv"),
                      destinationPath = dataPath(sim),
                      fun = "terra::rast",
                      userTags = c("object:harv"))
  }
  
  if (!suppliedElsewhere("modelOutput", sim)){
    sim$issaModel <- Cache(prepInputs,
                           targetFile = "mod_selmove_2015-2020_HPC_noTA.RDS",
                           url = extractURL("modelOutput"),
                           destinationPath = dataPath(sim),
                           fun = "readRDS",
                           userTags = c("object:modelOutput"))
  }
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

