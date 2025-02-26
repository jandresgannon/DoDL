# DoDL

The Division of Defense Labor (DoDL) github repository contains all the data and code necessary to reproduce and replicate the DoDL data and paper. The repository, data, and article are produced by [J Andres Gannon](https://jandresgannon.com/).

Gannon, J Andrés. "Complementarity in alliances: How strategic compatibility and hierarchy promote efficient cooperation in international security" _American Journal of Political Science_ (forthcoming)

## Installation

You can install DoDL from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("jandresgannon/DoDL")
```

## Important files
### Paper
- [pre-print]([url](https://github.com/jandresgannon/DoDL/blob/main/paper/DoDL_preprint.pdf))

- [appendix]([url](https://github.com/jandresgannon/DoDL/blob/main/paper/DoDL_Appendix.pdf))

### Tabular Data
- [01_DV.rds]([url](https://github.com/jandresgannon/DoDL/blob/main/data/01_DV.rds)) - Dependent variable data

- [02_EV.rds]([url](https://github.com/jandresgannon/DoDL/blob/main/data/02_EV.rds)) - Explanatory and control variable data

- [03_df-full.rds]([url](https://github.com/jandresgannon/DoDL/blob/main/data/03_df-full.rds)) - Full data set

### Replication Code
- [01_dv-dol.qmd](https://github.com/jandresgannon/DoDL/blob/main/docs/01_dv-dol.qmd) - produces dependent variable measure (01_DV.rds)

- [02_ev-alliance.qmd](https://github.com/jandresgannon/DoDL/blob/main/docs/02_ev-alliance.qmd) - produces explanatory variable measures and merges control variables (02_EV.rds)

- [03_full-data.qmd]([url](https://github.com/jandresgannon/DoDL/blob/main/docs/03_full-df.qmd)) - merges the first two dataframes and does minor pre-processing to produce the full dataset (03_df-full.rds)

- [DoDL_preprint.qmd](https://github.com/jandresgannon/DoDL/blob/main/paper/DoDL_preprint.qmd) - code for the manuscript's figures, tables, and results is embedded in the manuscript itself
