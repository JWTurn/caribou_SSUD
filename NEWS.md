# caribou_SSUD (development version)

- The northern mountain ecotype predicts from a table of the iSSA model's fixed-effect coefficients (`iSSAcoefficients`: columns `term` and `estimate`, e.g. hsaCEtool's `<model>_fixed.csv`), read from `<coefficientsModel>_fixed.csv` in `NMC_model_<coefficientsModel>.zip` in the `modelsFolderID` Drive folder (default model `full_to2020`; only that zip is downloaded, found by name, which also works for an account that is not a member of the folder's shared drive) unless supplied, or taken from a supplied `iSSAmodels` holding one model. `iSSAmodels` is downloaded only for the boreal ecotype, and boreal models the module downloads are not taken as a supplied northern mountain model.
- The baseline (the PDE and its binned map before any forecast) is now an input. Supply it as `pde` and `pdeMap`, or point the new `baselineSource` parameter at a folder, .zip file or URL holding a baseline this module saved; otherwise the module makes it from `modelLand` and saves it. In static mode (`simulationProcess = "static"`) the module now makes only the baseline and stops; it no longer fails trying to forecast.
- `pde`, `pdeMap`, `simPde` and `simPdeMap` are named lists with one map per model, for both ecotypes. `simulationScale` picks the models: `"global"` (the new default; the global model over `studyAreaCaribou`), `"regional"` (one model per region in `jurisdiction`, each over its area in `studyArea_juris`), or `"both"`. `"jurisdictional"` still works, as `"regional"`. The northern mountain ecotype has one model, `global`, for now.
- Saved maps are named by model, e.g. `pde_global.tif` and `pdeMap_NT.tif` for the baseline, and `pde_global_year2030.tif` for forecasts. Each map is one GeoTIFF that keeps its coordinate system.
- `ecotype` now defaults to `"boreal"` and must be a single ecotype.
- `studyArea_juris` is downloaded only for regional models. The downloaded file may hold areas packed with `terra::wrap()` or `sf` shapes; a file of unpacked SpatVectors (as `caribouLocPrep` saved them before) cannot be read back, so the module stops and says to make the file again.
- Fixed: the boreal PDEs were made but then lost; layer names were lost from a downloaded `modelLand`; `studyArea_juris` downloaded from Drive was not stored.

# caribou_SSUD 0.0.1 (21 February 2025)

- initial module version
