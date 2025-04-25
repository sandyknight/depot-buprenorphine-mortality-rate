# Make a one-year data set ---------------------------------------------------
make_annual_dataset <- function(yr, # Integer, calendar year
                                main_dt,
                                sir_dt) {
  # The final month of `yr`
  cutoff <- as.IDate(sprintf("%d-12-31", yr))


  # Check if each journey was active during year == `yr`
  dt <- main_dt[
    , .(
      intx = as.integer(
        year(triaged) <= yr & # Triage before or during yr
          (is.na(disd) | # Continued in treatment
            year(disd) >= yr) # Discharge during or after yr
      ),
      # Keep required columns
      triaged,
      disd,
      disrsn,
      utla23cd,
      sex,
      age,
      ethnic,
      sexualo,
      disable,
      housing_start,
      homeless_start,
      drug_alcohol,
      drug_heroin,
      drug_crack,
      drug_cocaine,
      drug_benzodiazepine,
      injstat
    ),
    by = .(client_random_id, n_jy)
  ]

  # If discharge date is NA, use final month of yr
  dt[is.na(disd) | disd > cutoff, `:=`(
    disd   = cutoff,
    disrsn = "Continued in treatment"
  )]

  # Use latest journeys in year
  # i.e. if a client has two journey active in yr, use only the later one
  dt <-
    dt[intx == 1, # Filter to journey flagged as active in yr
      .SD[which.max(n_jy)], # Subset to max on journey number...
      by = client_random_id # ...for each individial client
    ]

  # Intervention
  # The intervention of interest is `phbudi_any` in the SIR
  # table. We want to construct a binary variable that indicates
  # whether the client is recorded as recieving phbudi_any in
  # the preceding 12 months.
  last_phb <-
    sir_dt[phbudi_any == 1 & year(submoddt) <= yr, # Not in the future
      .(last_phbudi = max(submoddt)), # Subset to latest SIR that indicates LAB
      by = client_random_id # for each client
    ]

  # Merge latest phbudi_any indicator with main table
  dt <- merge(dt, last_phb, by = "client_random_id", all.x = TRUE)

  # New binary variable `t` indicating LAB in last 12 months
  # default as zero to avoid NAs later.
  dt[, t := 0L]

  # `t` == 1 if:
  dt[
    !is.na(last_phbudi) & # Client has receieved phbudi_any AND...
      # the latest recorded phbudi_any is within 12 months of discharge
      # or the end of the period (yr)
      as.integer((year(disd) - year(last_phbudi)) * 12 +
        (month(disd) - month(last_phbudi))) <= 12,
    t := 1L
  ]

  # Select columns for model fits
  dt[, .(
    client_random_id, triaged, disd,
    disrsn, utla23cd, sex, age, ethnic,
    sexualo, disable, housing_start, homeless_start,
    drug_alcohol, drug_heroin, drug_crack, drug_cocaine,
    drug_benzodiazepine, injstat,
    t
  )]
}

# main_dt  <- fread("data/K3anon_FullDataset_for_VfM.csv")
# sir_dt  <- fread("data/SIR_table_for_VfM_linked.csv")
# make_annual_dataset(2022, main_dt, sir_dt)

# Make all annual datasets for `yrs` -----------------------------------------
# Iterate `make_annual_dataset()` over `yrs` and add
# binary variables for mortality and successful completion
calculate_annual_datasets <- function(yrs, main_dt, sir_dt) {
  annual_datasets <- # List of annual datasets
    parallel::mclapply(
      X = yrs,
      FUN = make_annual_dataset,
      main_dt = main_dt,
      sir_dt = sir_dt,
      mc.cores = 1
    )

  names(annual_datasets) <- yrs # Give list items year as name

  # Combine annual datasets
  annual_dt <- data.table::rbindlist(annual_datasets, idcol = "year")

  # Add outcome variables
  annual_dt[, `:=`(died = data.table::fifelse(
    disrsn == "Died", 1, 0 # `died` == 1 if `disrn` == "Died"
  ), successful_completion = data.table::fifelse(
    # `successful_completion` == 1 if `disrsn` == "Succesful completion"
    disrsn == "Successful completion", 1, 0
  ))]

  annual_dt
}

# Create an annual summary table for an outcome variable ---------------------
summarise_outcome_by_year <-
  function(annual_dt, outcome) {
    # `outcome` == "died" or "successful_completion"
    # Calculate count by outcome, treatment group (`t`) and year.
    switch(outcome,
      "died" = {
        outcome_summary <-
          annual_dt[, .N, by = .(year, t, died)]
      },
      "successful_completion" = {
        outcome_summary <-
          annual_dt[, .N, by = .(year, t, successful_completion)]
      }
    )

    # Pivot to wide and calculate outcome %
    outcome_summary <-
      outcome_summary |>
      tidyr::pivot_wider(
        names_from = tidyselect::all_of(outcome),
        values_from = N
      ) |>
      dplyr::mutate(total = `0` + `1`) |>
      dplyr::mutate(
        p = `1` / total,
      )

    # Rename and clean up column names
    colnames(outcome_summary) <- c(
      "year",
      "treatment group",
      paste0(outcome, ": no"),
      paste0(outcome, ": yes"),
      "total",
      paste0(outcome, " pct")
    ) |>
      snakecase::to_sentence_case()

    outcome_summary
  }

# Plot rate of outcome by treatment group and year ---------------------------
# Also saves plot to plots/
plot_annual_outcome <- function(outcome_summary, outcome) {
  outcome_label <- to_sentence_case(outcome)
  pct_col_name <- paste0(outcome_label, " pct")

  afcharts::use_afcharts()

  p <- outcome_summary %>%
    mutate(Year = factor(Year)) %>%
    select(Year, `Treatment group`, all_of(pct_col_name)) %>%
    pivot_wider(
      names_from  = `Treatment group`,
      values_from = all_of(pct_col_name)
    ) %>%
    rename(
      Control = `0`,
      `Long-acting buprenorphine` = `1`
    ) %>%
    pivot_longer(
      cols       = c(Control, `Long-acting buprenorphine`),
      names_to   = "Treatment group",
      values_to  = "Rate"
    ) %>%
    # plot
    ggplot(aes(x = Year, y = Rate, fill = `Treatment group`)) +
    geom_col(position = "dodge", width = 0.6) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title    = paste0(outcome_label, " rate"),
      subtitle = "Long-acting buprenorphine vs any other opioid treatment",
      y        = NULL,
      fill     = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position      = "bottom",
      plot.title.position  = "plot"
    )

  ggsave(
    filename = paste0("plots/", paste0(outcome, "-plot.png")),
    plot     = p,
    width    = 24,
    height   = 24,
    units    = "cm",
    dpi      = 200
  )
}
