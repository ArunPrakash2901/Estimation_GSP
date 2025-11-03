# Quarterly Estimation of GSP
This repo delves into estimating and nowcasting Australia’s **Gross State Product (GSP)**, with a focus on transparent data sourcing, and mixed-frequency indicators.

Economic activity at the state level arrives at different frequencies and with revisions, yet decisions often can’t wait for the “final” quarterly releases. This project provides a minimal scaffold to:
- align mixed-frequency indicators,
- run a deterministic, restartable pipeline,
- export figures and tables for reporting,
- and keep the whole process easy to implement
or extend.

## Project structure
- `R/`                        — R helpers and pipeline code (targets).
- `_targets_DataSourcing.R`   — targets pipeline config and plan.
- `data/`                     — input data files.
- `data/schema_mfbvar.csv`    - describe series metadata and help keep indicator handling consistent
- `images/`                   — generated figures.
- `Estimation_GSP.qmd`,       - Project presentation
- `GSP_report.qmd`            - Project report
- `Data_sourcing_GSP.qmd`     — A quarto document where the data pipeline is executed.
- `QGSP.ipynb`                — The notebook contains data preprocessing, Model fit and EDA scripts.


## Data Pipeline

```{r}
library(targets)
tar_make()
```

This executes the workflow defined in `_targets_DataSourcing.R`. Intermediate artefacts are stored under data/, and charts are written to images/. You can visualise the pipeline with:

```{r}
tar_visnetwork()
```

An extensive instruction is provided in `Data_sourcing_GSP.qmd`. Follow the notebook for further context.

The pipeline is declarative, rerunning `tar_make()` only rebuilds what changed.Data acquisition steps (when included) are scripted so sources and transforms are traceable.
If you modify inputs (e.g., add a new indicator to a registry/schema), just update the relevant CSV / function and rerun the pipeline.



