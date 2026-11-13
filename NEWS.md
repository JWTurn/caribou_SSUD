# caribou_SSUD (development version)

- The northern mountain ecotype predicts from a table of the iSSA model's fixed-effect coefficients (`iSSAcoefficients`: columns `term` and `estimate`, e.g. hsaCEtool's `<model>_fixed.csv`), read from `<coefficientsModel>_fixed.csv` in `NMC_model_<coefficientsModel>.zip` in the `modelsFolderID` Drive folder (default model `full_to2020`; only that zip is downloaded, found by name, which also works for an account that is not a member of the folder's shared drive) unless supplied, or taken from a supplied `iSSAmodels` holding one model. `iSSAmodels` is downloaded only for the boreal ecotype, and boreal models the module downloads are not taken as a supplied northern mountain model.
- `studyArea_juris` is declared as an input and downloaded only for the boreal ecotype, which is the only one that uses it; the download is now kept (it used to stop with "object 'studyArea_juris' not found").

# caribou_SSUD 0.0.1 (21 February 2025)

- initial module version
