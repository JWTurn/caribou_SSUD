# caribou_SSUD (development version)

- `reclassifyCohortData()` classes each stand by its conifer share of the biomass: a mostly-conifer stand with some broadleaf is conifer (210), not broadleaf, whatever the order of its cohorts; the mixed band (`mixedForestCutoffs`) is compared with the conifer share only.
- The northern mountain ecotype predicts from a table of the iSSA model's fixed-effect coefficients (`iSSAcoefficients`: columns `term` and `estimate`, e.g. hsaCEtool's `<model>_fixed.csv`), read from `<coefficientsModel>_fixed.csv` in `NMC_model_<coefficientsModel>.zip` in the `modelsFolderID` Drive folder (default model `full_to2020`; only that zip is downloaded, found by name, which also works for an account that is not a member of the folder's shared drive) unless supplied, or taken from a supplied `iSSAmodels` holding one model. `iSSAmodels` is downloaded only for the boreal ecotype, and boreal models the module downloads are not taken as a supplied northern mountain model.

# caribou_SSUD 0.0.1 (21 February 2025)

- initial module version
