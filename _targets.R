# _targets.R

# Load packages required to define the pipeline:
library(targets)


# Set target options:
tar_option_set(
  packages = c(
    "data.table",
    "parallel",
    "dplyr",
    "tidyr",
    "tidyselect",
    "snakecase",
    "afcharts",
    "ggplot2"
  ),
  format = "qs"
)

# Run the R scripts in the R/ folder
tar_source()

list(
  tar_target(
    name = main_table_file,
    "data/K3anon_FullDataset_for_VfM.csv",
    format = "file"
  ),
  tar_target(
    name = sir_table_file,
    "data/SIR_table_for_VfM_linked.csv",
    format = "file"
  ),
  tar_target(
    main_dt,
    command = data.table::fread(main_table_file)
  ),
  tar_target(
    sir_dt,
    command = data.table::fread(sir_table_file)
  ),
  tar_target(
    name = yrs,
    command = seq(2021L, 2024L, 1L)
  ),
  tar_target(
    name = annual_dt,
    calculate_annual_datasets(
      yrs = yrs,
      main_dt = main_dt,
      sir_dt = sir_dt
    )
  ),
  tar_target(
    name = mortality_summary,
    command = summarise_outcome_by_year(annual_dt, "died")
  ),
  tar_target(
    mortality_rate_plot,
    command = plot_annual_outcome(mortality_summary, "died")
  )
)
