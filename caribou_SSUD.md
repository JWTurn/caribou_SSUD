caribou_SSUD Manual
================
Version 0.0.1.9000, last updated: 2026-11-23

- [caribou_SSUD Module](#caribou_ssud-module)
  - [Module Overview](#module-overview)
    - [Module summary](#module-summary)
    - [Module inputs and parameters](#module-inputs-and-parameters)
    - [Events](#events)
    - [Plotting](#plotting)
    - [Saving](#saving)
    - [Module outputs](#module-outputs)
    - [Links to other modules](#links-to-other-modules)
    - [Getting help](#getting-help)
    - [References](#references)

# caribou_SSUD Module

<!-- the following are text references used in captions for LaTeX compatibility -->

#### Authors:

Julie W Turner <julwturner@gmail.com> \[aut, cre\]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

caribou_SSUD maps caribou habitat use from an integrated step-selection
analysis (iSSA). It turns the model’s fixed-effect coefficients into a
steady-state utilization distribution ([Potts and Schlägel
2020](#ref-PottsSchlagel2020)): the long-run density of use the model’s
habitat and movement terms imply over a landscape. The result is a
probability density of use (PDE) for each model, normalized to sum to 1
over the area it is predicted for, and a binned map of it (`pdeMap`)
that compares the cells within that area.

This module currently handles two caribou ecotypes (`ecotype`): northern
mountain caribou (NMC), whose coefficients are read from the
fixed-effect table of a fitted model, and boreal caribou, whose
`glmmTMB` models are supplied one per jurisdiction. With
`simulationScale` it maps the global model, one model per region, or
both.

In static mode (`simulationProcess = "static"`, the default) it maps the
baseline landscape only. In dynamic mode it also rebuilds the landscape
layers that change during a simulation every `predictionInterval` years
from `predictStartYear`, and maps habitat use on each. Each layer is
matched by its name to what forecasts it (`dynamicLayers`):

- forest cover (NMC: `prop_forest_240`, `prop_forest_1200`; boreal:
  `prop_needleleaf`, `prop_mixedforest`), from the simulated tree
  cohorts (LandR);
- time since fire (NMC: `ts_fire_240`, `ts_fire_1200`; boreal:
  `timeSinceFire`), from the simulated fires (e.g. `scfm`);
- time since harvest (NMC: `ts_harv_240`; boreal: `timeSinceHarvest`),
  which only ages for now, as no module is integrated to simulate
  harvest yet;
- distance to human disturbance (NMC: e.g. `dist_paved_240`; boreal:
  e.g. `distpaved`), from the roads and mines
  `anthroDisturbance_Generator` adds. This is only set up for NMC in
  Yukon at this time; boreal distances stay as in `modelLand`.

Every other layer (e.g. NMC’s land cover, boreal’s `prop_veg` and
`prop_wets`) stays as in `modelLand`. If part of the model, layers keep
the resolution in their names: a coarser layer (e.g. NMC’s 1200 m layers
on a 240 m grid) is built from the grid’s cells, as the model’s layers
were. A layer with no resolution in its name (e.g. the boreal layers) is
on the simulation’s grid.

For a model fit with seasons (terms such as
`seasonwinter:prop_forest_240_end`; currently NMC only), the module
makes a map for each season (`seasonMaps`) and a full-year map that
combines them, weighted by season length or as chosen (`fullYear`,
`seasonWeights`).

### Module inputs and parameters

The module needs the model and the landscape the model reads (Table
<a href="#tab:moduleInputs-caribou-SSUD">1</a>):

- `iSSAmodels`, the models, named `global` and/or by region (e.g. `NT`).
  Each can be the fitted model (`glmmTMB`) or a table of its
  fixed-effect coefficients (columns `term` and `estimate`, as in
  hsaCEtool’s `<model>_fixed.csv`), for either ecotype. If not supplied,
  the NMC global model’s table is read from
  `<coefficientsModel>_fixed.csv` in the model’s zip file in the Google
  Drive folder `modelsFolderID`, and boreal models from
  `iSSAmodels.RData` in `outputFolderID`. (`iSSAcoefficients`, one table
  for the global model, still works but is deprecated.)
- For a model with seasons (NMC), `iSSAseasons`, the days in each
  season, used to weight the full-year map; read from the model’s zip
  file when it has one.
- `modelLand`, the landscape layers, named as in the model’s terms; a
  layer the model needs and the list lacks stops the run, naming it. For
  NMC, distances capped for the model (more than 20 km set to 21 km)
  carry `_adj` in their names (e.g. `dist_paved_240_adj`); the boreal
  distances are not capped.
- `studyAreaCaribou`, the area the global model was fit on, and, for the
  regional models, `studyArea_juris`, one area per region. Optionally,
  `studyAreaReporting`, an area to normalize and bin the maps over
  instead (see `pdeArea`).
- Optionally, a baseline made earlier (`pde` and `pdeMap`, or a folder,
  zip file or URL in `baselineSource`), so the baseline is read rather
  than made again.

In dynamic mode it also uses, from the other modules of the simulation:

- the simulated forest (`cohortData` and `pixelGroupMap`, with
  `sppEquiv` to tell conifers from broadleaf species);
- the time since fire (`timeSinceFire`): only fires burned in the
  simulation (from `baselineYear` on) are read from it, so a map that
  starts from stand age is not read as fire history;
- the human disturbance (`disturbanceList`);
- the simulation’s grid (`rasterToMatch`), which every `modelLand` layer
  must match. If the model has layers coarser than the grid’s cells, the
  grid’s top-left corner must also be a multiple of each of those
  resolutions. A template cropped to a study area should be snapped
  outward to the coarsest template resolution, not just the model’s
  (e.g. 4800 m on the NMC templates, though the current NMC models go up
  to 1200 m), so later models with coarser layers fit too.

A forecaster that is missing leaves its layers as they are: fire and
harvest layers only age, and distances stay as in `modelLand`.

| objectName | objectClass | desc | sourceURL |
|:---|:---|:---|:---|
| iSSAmodels | list | The iSSA models, as a list named by model: `global` and/or one per region in `jurisdiction` (e.g. list(global = m1, NT = m2)). Each is a fitted model (glmmTMB) or a table of its fixed-effect coefficients, one row per term, with columns `term` and `estimate` (others, e.g. those of hsaCEtool’s `<model>_fixed.csv`, are ignored); the two can be mixed. One model or table on its own is the global model. If not supplied: for the northern mountain ecotype, the global model’s table is read from `<coefficientsModel>_fixed.csv` in `NMC_model_<coefficientsModel>.zip` in `modelsFolderID`; for the boreal ecotype, the models are read from `iSSAmodels.RData` in `outputFolderID`. | NA |
| iSSAcoefficients | data.table | Deprecated: supply the table in `iSSAmodels` instead. One coefficient table (as in `iSSAmodels`), used as the global model when `iSSAmodels` is not supplied. | NA |
| iSSAseasons | data.table | For a model with seasons and seasonWeights ‘length’: the model’s season table, one row per season, with columns `season` (as in the terms, e.g. `winter` for `seasonwinter`) and `days` (days per season), as hsaCEtool’s `<model>_seasons.csv` gives it. If not supplied, read from `<coefficientsModel>_seasons.csv` in the model’s zip, when it has one (models fit before hsaCEtool wrote this table do not). | NA |
| cohortData | data.table | `data.table` with cohort-level information on age and biomass, by `pixelGroup` and ecolocation (i.e., `ecoregionGroup`). If supplied, it must have the following columns: `pixelGroup` (integer), `ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$), `age` (integer in years) | NA |
| pixelGroupMap | SpatRaster | A raster layer with `pixelGroup` IDs per pixel. Pixels are grouped based on identical `ecoregionGroup`, `speciesCode`, `age` and `B` composition, even if the user supplies other initial groupings (e.g., via the `Biomass_borealDataPrep` module. | NA |
| timeSinceFire | SpatRaster | Dynamic mode: years since each cell last burned in the simulation, NA where it has not burned (as scfmSpread keeps it), on the forecast grid. Fire layers take a cell’s fire year from it where that is more recent than modelLand’s and in baselineYear or later, so values it started with (e.g. stand age) are not read as fires. If absent, fire layers only age. | NA |
| buffer | numeric | Dynamic mode: the moving-window radius (m) that other modules in the workflow use (e.g. prepTracks’ median step length, passed to prepLandscape). movingWindow rows with radius NA take it, and a row with another radius stops, so modules cannot use different windows. Optional. | NA |
| disturbanceList | list | Dynamic mode: anthroDisturbance_Generator’s disturbances, a list by sector of lists by class, each holding all of that class’s features (existing and simulated) as a SpatVector (or sf). Distance layers are measured to the classes in disturbanceClasses. If absent, distance layers stay as in modelLand. | NA |
| sppEquiv | data.table | Dynamic mode: LandR’s species-equivalence table, with a `Type` column (Conifer/Deciduous) and the species codes in column sppEquivCol. If absent, LandR::sppEquivalencies_CA. | NA |
| forestBaseline | list | Dynamic mode, forestForecast ‘change’: LandR’s forest at the start of the simulation, a named list of 0/1 SpatRasters on the forecast grid, one per forest covariate (e.g. prop_forest). If not supplied, made in init from cohortData and pixelGroupMap. | NA |
| modelLand | list | The landscape layers the models read: a named list of SpatRasters on one grid (or one multi-layer SpatRaster, made into that list), each named as in the model’s terms without `_end` (e.g. `prop_forest_240` for the term `prop_forest_240_end`). The layers needed are read from the terms, and a missing one stops the run, naming it. Capped distances carry `_adj` (e.g. `dist_paved_240_adj`). If not supplied, `modelLand.tif` is downloaded from outputFolderID. | NA |
| studyAreaCaribou | SpatVector | The area of the global model: a polygon of the full extent of the caribou locations it was fit on. If not supplied, studyArea is used. | NA |
| studyAreaReporting | SpatVector | Optional: the area the results are reported for (e.g. a range buffer clipped to a jurisdiction), a SpatVector or sf polygon. Used only when pdeArea is ‘fullReportingArea’ or ‘regionalReportingArea’. | NA |
| rasterToMatch | SpatRaster | Dynamic mode: the simulation’s grid, e.g. the one LandR and scfm use. The forecast layers are built on it. Every modelLand layer must be on it (same extent, rows and columns, cell size and coordinate system), or the run stops. If the model has layers coarser than the grid’s cells (e.g. ts_fire_1200 on a 240 m grid), the grid’s top-left corner (xmin, ymax) must also be a multiple of each of those resolutions, so the coarse blocks line up with modelLand’s. When cropping a template to a study area, snap the crop outward to the coarsest template resolution, not just the model’s, if the resolutions nest (e.g. 4800 m for the northern mountain templates), so later models with coarser layers fit too. If not supplied, modelLand’s grid is used. | NA |
| studyArea_juris | list | Named list of SpatVectors, one area per region (e.g. jurisdiction), for the regional models. Not needed when simulationScale is ‘global’. | NA |
| pde | list | Baseline PDE (raw utility if normalizePDE is FALSE): a named list with one SpatRaster per model (‘global’ and/or a region), or for a model with seasons, per season map and the full-year map (see seasonMaps, fullYear). If not supplied, read from baselineSource, else made from modelLand. | NA |
| pdeMap | list | Baseline binned PDE maps, named as `pde`; NULL if normalizePDE is FALSE. | NA |
| fixedSSUD | SpatRaster | Dynamic mode: the layers the models read that do not change in forecasts (those no dynamicLayers pattern matches), named as in the terms. If not supplied, taken from modelLand. | NA |

<span id="tab:moduleInputs-caribou-SSUD"></span>Table 1: List of
*caribou_SSUD* input objects and their description.

The parameters (Table <a href="#tab:moduleParams-caribou-SSUD">2</a>)
choose the ecotype, the models and the area each is mapped over
(`ecotype`, `simulationScale`, `pdeArea`), static or dynamic mode and
the prediction years (`simulationProcess`, `predictStartYear`,
`predictionInterval`, `predictLastYear`), and where the model and a
premade baseline come from (`coefficientsModel`, `modelsFolderID`,
`baselineSource`). In dynamic mode, further parameters say how each
forecast layer is built; set them to match how the model’s layers were
made:

- all layers: `dynamicLayers` (which layers are forecast, and from what;
  each ecotype has its own default, matching the layer names above) and
  `baselineYear` (the year `modelLand` describes, and the simulation
  should start in: 2025 for the NMC `modelLand`, 2022 for the boreal NWT
  one);
- forest: `forestForecast` (`"change"`, the default, adds the simulated
  change since the start to `modelLand`’s layer; `"LandR"` uses the
  simulated forest alone), `forestTypes` (the LandR forest types each
  layer counts: NMC `prop_forest` counts all forest; boreal
  `prop_needleleaf` counts conifer stands and `prop_mixedforest`
  deciduous and mixed ones) and `forestMinBiomass`;
- moving windows: `movingWindow` (the circles forecast layers are
  averaged in, as the model’s layers were built: one row per window, a
  pattern on layer names and a radius in m; by default NMC’s 240 m land
  cover in 500 m; for the boreal NWT `modelLand` use
  `data.frame(layers = "^prop_(needleleaf|mixedforest)$", radius = 720)`).
  A radius of `NA` takes the input `buffer`, the radius other modules in
  the workflow use (e.g. prepTracks); a different radius stops, so the
  modules cannot disagree. Only forest layers take a window so far;
- time since fire and harvest: `ts_else` (the value given to cells that
  never burned or were never harvested; 200 for the NMC models),
  `noEventYear` (for a `modelLand` that counts such cells from a
  stand-in year instead: the boreal NWT one counts them from 1900, so
  use `list(fire = 1900, harvest = 1900)`), and `fireCoarse` and
  `harvestCoarse` (for layers coarser than the grid, such as NMC’s 1200
  m layers: whether they take the mean, the default, or the most recent
  fire or harvest year of their cells; use the rule the model was fit
  with: the mean for the V2 NMC models such as `fullV2_to2020`, the most
  recent for the earlier ones);
- distances (NMC): `disturbanceClasses` (which
  `anthroDisturbance_Generator` classes each distance is measured to;
  set by default for NMC in Yukon, none for boreal), `distanceCellSize`
  (the cell size distances are measured on, 30 m by default, as for the
  NMC model’s layers) and `distanceCap` (the cap for `_adj` layers).

| paramName | paramClass | default | min | max | paramDesc | cacheKey |
|:---|:---|:---|:---|:---|:---|:---|
| ts_else | integer | 200 | NA | NA | Years since disturbance given to cells with no recorded disturbance (e.g. never burned) in the time-since-fire and time-since-harvest layers. Use the value the model was fit with (hsaCEtool’s `ts_nodata`; 200 for the northern mountain models). | TRUE |
| baselineYear | integer | 2025 | NA | NA | The year modelLand describes: its time-since layers (e.g. ts_fire_240) count years back from it. Forecasts count from it too, and the simulation should start in it (LandR’s stands at the start are compared with modelLand’s). 2025 for the northern mountain modelLand; 2022 for the boreal NWT one. | TRUE |
| noEventYear | list | NA, NA | NA | NA | How modelLand’s time-since layers store cells with no fire (or harvest), by source: NA = such cells are NA (or ts_else or more), and forecasts give them ts_else; a year = they hold the years since that stand-in year (e.g. 1900: 122 with baselineYear 2022), and forecasts give them the years since it (year - noEventYear), as the model was fit. Either way they are left out of coarser layers’ fire years. E.g. the boreal NWT modelLand (baselineYear 2022): list(fire = 1900, harvest = 1900). | TRUE |
| dynamicLayers | list |  | NA | NA | Dynamic mode: which of the layers the models read are forecast, and from what: a named list, source = a regular expression matched against layer names. Sources: ‘forest’ (LandR’s cohortData and pixelGroupMap), ‘fire’ (scfm’s timeSinceFire), ‘harvest’ (no forecaster yet: the layer only ages) and ‘disturbance’ (distances to anthroDisturbance_Generator’s features, see disturbanceClasses). A layer that matches no pattern stays as in modelLand (e.g. vegetation). NULL = the default, which matches layer names with a resolution (northern mountain, e.g. prop_forest_240, ts_fire_1200) and without (boreal NWT, e.g. prop_needleleaf, timeSinceFire): list(forest = ‘^prop\_(forest\_\|needleleaf$&#124;mixedforest$)’, fire = ‘^(ts_fire\_\|timeSinceFire$)', harvest = '^(ts_harv(est)?_&#124;timeSinceHarvest$)’, disturbance = ‘<sup>dist(\[</sup>u\]\|u\[^r\]\|ur\[^b\])’, i.e. names that start with ‘dist’ but not ‘disturb’, so distDisturbance is a distance and disturbance_240 is not). list() = no forecast layers. | TRUE |
| movingWindow | data.frame | ^prop\_.\*…. | NA | NA | Dynamic mode: the circular moving windows forecast layers are averaged in, as the model’s layers were built: a table with one row per window, columns `layers` (a pattern on layer names) and `radius` (m). The default matches hsaCEtool’s `window_m` for the NMC models: 240 m land cover (e.g. prop_forest_240) is the mean of the 240 m cells within 500 m. Boreal NWT: data.frame(layers = ‘^prop\_(needleleaf\|mixedforest)\$’, radius = 720). A radius of NA takes `buffer`; a radius that differs from a supplied `buffer` stops. Layers that match no row get no window (e.g. prop_forest_1200 is the mean of the 240 m cells in each 1200 m block). Only forest layers can take a window so far. NULL = none. | TRUE |
| fireCoarse | character | mean | NA | NA | Dynamic mode: how a fire layer coarser than the grid (e.g. ts_fire_1200 on a 240 m grid) is built from the grid cells’ fire years (unburned cells left out): ‘mean’ = the mean fire year, as hsaCEtool makes them for the V2 northern mountain models (e.g. fullV2_to2020) and their modelLand; ‘recent’ = the most recent fire in the block, as for the earlier models (e.g. full_to2020). Use the rule the model was fit with. | TRUE |
| harvestCoarse | character | mean | NA | NA | Dynamic mode: as fireCoarse, for harvest layers coarser than the grid (e.g. ts_harv_1200): ‘mean’ = the mean harvest year (unharvested cells left out); ‘recent’ = the most recent harvest in the block. Use the rule the model was fit with. | TRUE |
| disturbanceClasses | list |  | NA | NA | Dynamic mode: which of anthroDisturbance_Generator’s classes each distance covariate (the layer name without resolution or `_adj`, e.g. dist_paved) measures to, as ‘sector/class’ in disturbanceList; several classes are combined. A forecast distance is the smaller of modelLand’s and the distance to the class’s features that year (features are only ever added), so features outside the study area or in no Generator class still count. A covariate not listed stays as in modelLand. NULL = the default, the Yukon categories the northern mountain models were fit on: list(dist_paved = ‘roads/paved’, dist_unpaved = ‘roads/unpaved’, dist_placer = ‘mining/placer’, dist_quartz = ‘mining/quartz’, dist_othermines = ‘mining/otherMines’) (dist_lfother and dist_otherpoly have no Generator class). Boreal distances (e.g. distpaved) are in none of them, so they stay as in modelLand. | TRUE |
| distanceCellSize | numeric | 30 | NA | NA | Dynamic mode: cell size (m) distances are measured on, as hsaCEtool does: the distance from each cell of this size to the nearest one a feature touches, then the mean over each grid cell (e.g. a 240 m cell’s 64 30 m cells). Must divide the grid’s cell size evenly; the grid’s cell size = distances from cell centres (faster, but not as the model’s layers were made). | TRUE |
| distanceCap | list | 20, 21 | NA | NA | Dynamic mode: how capped distances (layers named with `_adj`, e.g. dist_paved_240_adj) are capped, as hsaCEtool’s `distance_cap`: more than beyond_km km gives set_to_km km. | TRUE |
| forestMinBiomass | numeric | 0 | 0 | NA | Dynamic mode: a LandR cell is forest when its stand has more than this much tree biomass (g/m2, LandR’s unit for B); 0 = any tree biomass. | TRUE |
| forestForecast | character | change | NA | NA | Dynamic mode: how forest layers are forecast. ‘change’ = modelLand’s layer plus LandR’s change since the start of the simulation (LandR now minus LandR at the start), kept within 0-1; ‘LandR’ = LandR’s value alone. | TRUE |
| forestTypes | list | c(“conif…. | NA | NA | Dynamic mode: the LandR forest types each forest covariate counts, by covariate (the layer name without its resolution). Types: ‘conifer’ (conifer share of the stand’s biomass at or above the upper mixedForestCutoff, 0.66), ‘mixed’ (between the cutoffs) and ‘deciduous’ (at or below the lower cutoff, 0.33). | TRUE |
| sppEquivCol | character | LandR | NA | NA | Dynamic mode: the column of sppEquiv that holds cohortData’s species codes, used to tell conifers from broadleaf species. | TRUE |
| simulationProcess | character | static | NA | NA | Should the simulation use LandR (dynamic) or land cover map (static)?defaults to static | TRUE |
| predictionInterval | numeric | 5 | NA | NA | Time between predictions | TRUE |
| predictStartYear | numeric | 2025 | NA | NA | The first year to start forecasted simulations if dynamic. This is because we start forecasting landcovers earlier. | TRUE |
| predictLastYear | logical | TRUE | NA | NA | If last year of simulation is not multiple of predictionInterval, should it predict for the last year too? | TRUE |
| simulationScale | character | global | NA | NA | Which models to predict: ‘global’ (the global model, over studyAreaCaribou), ‘regional’ (one model per region in `jurisdiction`, each over its area in studyArea_juris), or ‘both’. ‘jurisdictional’ is read as ‘regional’. | TRUE |
| baselineSource | character | NA | NA | NA | A premade baseline, used when `pde` is not supplied: a folder, a .zip file, or a URL (e.g. of a .zip on Google Drive) holding `pde_<model>.tif` (and `pdeMap_<model>.tif` if normalizePDE) for each model, as this module writes them. If NA, the baseline is made from modelLand and saved to outputPath. | TRUE |
| pdeArea | character | fullArea…. | NA | NA | The area each PDE is predicted, normalized (summed to 1) and binned into its map over, so a map compares cells within that area. One choice for the global model: ‘fullArea’ (studyAreaCaribou, the area the global model was fit on) or ‘fullReportingArea’ (studyAreaReporting, which may reach beyond the fitted area); and one for the regional models: ‘regionalArea’ (the model’s region in studyArea_juris) or ‘regionalReportingArea’ (the part of studyAreaReporting inside the model’s region). Only the choice for the models run (simulationScale) is used. The ‘Reporting’ choices need studyAreaReporting. | TRUE |
| seasonMaps | character |  | NA | NA | For a model with seasons (terms such as `seasonwinter:prop_forest_240_end`): the seasons that each get a map of their own, named `<model>_<season>` (e.g. global_winter, written as pde_global_winter.tif). A season’s map uses the terms without a season and that season’s terms. NULL = all the model’s seasons; character(0) = none. Models without seasons ignore it. | TRUE |
| fullYear | character |  | NA | NA | For a model with seasons: the seasons combined into one full-year map, named as the model (e.g. global, written as pde_global.tif): the seasons’ PDEs, weighted by seasonWeights, so it still sums to 1. NULL = all the model’s seasons; character(0) = no full-year map. Needs normalizePDE. Models without seasons ignore it. | TRUE |
| seasonWeights | characte…. | length | NA | NA | How the seasons in fullYear are weighted: ‘length’ = by days per season, from iSSAseasons; ‘equal’; or numbers named by season (e.g. c(winter = 2, summer = 1)), scaled to sum to 1. | TRUE |
| normalizePDE | logical | TRUE | NA | NA | Whether to normalize PDE inside SSUD (FALSE for tmux workflow) | TRUE |
| jurisdiction | character | BC, SK, …. | NA | NA | A list of jurisdictions to use for PDE creation | TRUE |
| ecotype | character | boreal | NA | NA | Which ecotype of caribou were modelled: ‘boreal’ or ‘northern_mountain’ | TRUE |
| .plots | character | screen | NA | NA | Used by Plots function, which can be optionally used here | TRUE |
| .plotInitialTime | numeric | 0 | NA | NA | Describes the simulation time at which the first plot event should occur. | TRUE |
| .plotInterval | numeric | NA | NA | NA | Describes the simulation time interval between plot events. | TRUE |
| .saveInitialTime | numeric | NA | NA | NA | Describes the simulation time at which the first save event should occur. | TRUE |
| .saveInterval | numeric | NA | NA | NA | This describes the simulation time interval between save events. | TRUE |
| .studyAreaName | character | NA | NA | NA | Human-readable name for the study area used - e.g., a hash of the studyarea obtained using `reproducible::studyAreaName()` | TRUE |
| .seed | list |  | NA | NA | Named list of seeds to use for each event (names). | TRUE |
| .useCache | logical | FALSE | NA | NA | Should caching of events or module be used? | TRUE |
| outputFolderID | character | <https://>…. | NA | NA | Google Drive folder ID for workflow outputs | TRUE |
| coefficientsModel | character | bioseaso…. | NA | NA | Name of the northern mountain iSSA model whose coefficients are read | TRUE |
| modelsFolderID | character | <https://>…. | NA | NA | Google Drive folder holding the northern mountain model zips | TRUE |

<span id="tab:moduleParams-caribou-SSUD"></span>Table 2: List of
*caribou_SSUD* parameters and their description.

### Events

- `init`: makes the baseline PDE and map for the current landscape, or
  reads a premade one, and saves a baseline it made to the output
  folder. In dynamic mode it also schedules the two events below, from
  `predictStartYear` every `predictionInterval` years, and in the last
  year when `predictLastYear` is `TRUE`.
- `simLayers` (dynamic mode): rebuilds the layers that change, on top of
  the layers that do not (`fixedSSUD`), and writes the stack:
  - forest cover: the simulated cohorts classed into forest types, as a
    share of each cell at the layer’s resolution (and of a circle around
    it, for layers `movingWindow` picks);
  - time since fire: the most recent of `modelLand`’s fire and the
    simulation’s, in years; cells that never burned get `ts_else` (NMC)
    or keep counting from `noEventYear` (boreal NWT);
  - time since harvest: `modelLand`’s, aged to the year;
  - distances (NMC): the smaller of `modelLand`’s distance and the
    distance to the simulated features of that class, so features
    outside the simulated area, or of classes the simulation does not
    add, still count; `_adj` layers are capped again. Distances with no
    class in `disturbanceClasses` (all boreal ones) stay as in
    `modelLand`.
- `calcSimPde` (dynamic mode): maps habitat use on the rebuilt layers
  with the same models as the baseline, into `simPde` and `simPdeMap`,
  and writes them.

### Plotting

The module does not plot.

### Saving

A baseline the module makes is written to `pde_<.studyAreaName>/` in the
output folder, as `pde_<model>.tif` and, when `normalizePDE` is `TRUE`,
`pdeMap_<model>.tif`; each file is registered as an output. The model is
`global` or a region (e.g. `pde_global.tif`, or `pde_NT.tif` for a
boreal regional model). For a model with seasons (NMC), a season’s maps
are named `<model>_<season>` (e.g. `pde_global_winter.tif`), and the
full-year map as the model. In dynamic mode, each prediction year’s
layers and maps go to its `sims/` subfolder, with the year in their
names (e.g. `pde_global_year2030.tif`).

### Module outputs

Description of the module outputs (Table
<a href="#tab:moduleOutputs-caribou-SSUD">3</a>).

| objectName | objectClass | desc |
|:---|:---|:---|
| fixedSSUD | SpatRaster | Dynamic mode: the layers that do not change in forecasts (see inputs) |
| forestBaseline | list | Dynamic mode: LandR’s forest at the start of the simulation (see inputs) |
| pde | list | Current (pre simulation) Raw pde calculation, one SpatRaster per map (see the input `pde`) |
| pdeMap | list | Current (pre simulation) binned map of pde UD for intensity of selection, one SpatRaster per map |
| simPde | list | Raw pde calculation from sim layers, by year, then one SpatRaster per map, as `pde` |
| simPdeMap | list | Forcast (simulated) binned map of pde UD for intensity of selection by year |

<span id="tab:moduleOutputs-caribou-SSUD"></span>Table 3: List of
*caribou_SSUD* outputs and their description.

### Links to other modules

In dynamic mode, for both ecotypes, the simulated forest comes from the
LandR Biomass modules
([`Biomass_borealDataPrep`](https://github.com/PredictiveEcology/Biomass_borealDataPrep),
[`Biomass_core`](https://github.com/PredictiveEcology/Biomass_core):
`cohortData` and `pixelGroupMap`;
[`Biomass_regeneration`](https://github.com/PredictiveEcology/Biomass_regeneration)),
and the time since fire from the fire model
([`scfm`](https://github.com/PredictiveEcology/scfm)’s `scfmSpread`:
`timeSinceFire`).

Where the model and the landscape come from currently differs by
ecotype:

- NMC: the iSSA is fit, and its landscape layers (`modelLand`) and
  coefficients made, outside SpaDES with
  [`hsaCEtool`](https://github.com/JWTurn/hsaCEtool). In dynamic mode,
  the human disturbance comes from
  [`anthroDisturbance_Generator`](https://github.com/FOR-CAST/anthroDisturbance_Generator)
  (`disturbanceList`).
- Boreal: the iSSA is fit within SpaDES, by a chain of modules:
  [`caribouLocPrep`](https://github.com/gc-rmcinnes/caribouLocPrep)
  (cleans the collar locations and makes the study areas,
  `studyAreaCaribou` and `studyArea_juris`),
  [`prepTracks`](https://github.com/gc-rmcinnes/prepTracks) (makes the
  steps), [`prepLandscape`](https://github.com/JWTurn/prepLandscape)
  (makes the yearly landscape layers),
  [`extractLand`](https://github.com/gc-rmcinnes/extractLand) (extracts
  the layers at the steps) and
  [`caribouiSSA`](https://github.com/gc-rmcinnes/caribouiSSA) (fits the
  `glmmTMB` models, `iSSAmodels`, and hands on the landscape they read,
  `modelLand`). Distances to human disturbance are not forecast yet.

### Getting help

- <https://github.com/JWTurn/caribou_SSUD/issues>

### References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-PottsSchlagel2020" class="csl-entry">

Potts, Jonathan R., and Ulrike E. Schlägel. 2020. “Parametrizing
Diffusion-Taxis Equations from Animal Movement Trajectories Using Step
Selection Analysis.” *Methods in Ecology and Evolution* 11 (9):
1092–105. <https://doi.org/10.1111/2041-210X.13406>.

</div>

</div>
