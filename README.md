## Project structure

- `R/`: Contains the R scripts for data analysis.
- `data/`: Contains the raw and prepared data.
- `models/`: Contains penalized logistic regression models.
- `output/`: Contains the results of the analysis, including figures and tables.
- `topics/`: Contains scripts and data related to topic modeling.
- `power/`: Contains scripts related to power analysis.

## How to run

Please change all instances of `Set working directory here` in script files to your working directory before running the scripts.

The analysis is performed by running the R scripts in the `R/` directory in the following order:

1.  `01_data_prep_descriptives.R`
2.  `03_binary-logistic-regression_bayesian.R`
3.  `04_combined_effects_risk_diff.R`
4.  `06_conditional_outcomes.R`
5.  `07_ga_preference_check.R`
6.  `08_model_comparisons.R`
7.  `09_commercial.R`

For the topic analysis, run the Python scripts in the `topics/` directory first before running `05_topic_prevalence_heatmap.R` in the `R/` directory.

The `renv.lock` file lists the specific R package versions used in the analysis. You can use the `renv` package to restore the project's dependencies:

```R
renv::restore()
```
