# DoDL

The Division of Defense Labor (DoDL) github repository contains all the data and code necessary to reproduce and replicate the DoDL data and paper. The repository, data, and article are produced by [J Andres Gannon](https://jandresgannon.com/).

Gannon, J Andrés. ["Complementarity in alliances: How strategic compatibility and hierarchy promote efficient cooperation in international security"] _American Journal of Political Science_ (forthcoming)

## Installation

You can install DoDL from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("jandresgannon/DoDL")
```

## Important files
### Paper
- pre-print

- appendix

### Tabular Data
- 01_DV.rds - Dependent variable data

- 02_EV.rds - Explanatory variable and control data

- 03_df-full.rds - Full data set

### Replication Code
- 01_dv-dol.qmd - produces dependent variable measure (01_DV.rds)

- 02_ev-alliance.qmd - produces explanatory variable measures and merges control variables (02_EV.rds)

- 03_full-data.qmd - merges the first two dataframes and does minor pre-processing to produce the full dataset (03_df-full.rds)

- DoDL_preprint.qmd - code for the manuscript's figures, tables, and results is embedded in the manuscript itself