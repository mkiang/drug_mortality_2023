## 01_create_analytic_data.R ----
##
## Takes the raw CDC WONDER data and then generate data in the format that
## the NCI Joinpoint Regression Program can take it. Note that I saw one subset
## with pre-2020 data for fitting joinpoint models and then another version
## with all data.

## Imports ----
library(tidyverse)
library(here)

## CONSTANTS ----
COL_NAMES <- c(
    "remove1",
    "state",
    "st_fips",
    "remove2",
    "year",
    "n_deaths",
    "pop",
    "crude_rate",
    "crude_lower",
    "crude_upper",
    "crude_se",
    "adj_rate",
    "adj_rate_lower",
    "adj_rate_upper",
    "adj_rate_se"
)

## Death data ----
death_df <- dplyr::bind_rows(
    readr::read_delim(
        here::here(
            "data_raw",
            c(
                "Multiple Cause of Death, 1999-2020.txt",
                "Provisional Mortality Statistics, 2018 through Last Week.txt"
            )
        ),
        skip = 1,
        delim = "\t",
        col_names = COL_NAMES
    ) |>
        dplyr::mutate(
            crude_rate = as.numeric(crude_rate),
            adj_rate = as.numeric(adj_rate)
        ),
    readr::read_delim(
        here::here(
            "data_raw",
            c(
                "US_Multiple Cause of Death, 1999-2020.txt",
                "US_Provisional Mortality Statistics, 2018 through Last Week.txt"
            )
        ),
        skip = 1,
        delim = "\t",
        col_names = COL_NAMES[-c(2:3)]
    ) |>
        dplyr::mutate(
            state = "US",
            st_fips = "99",
            .before = 1
        )
)

death_df <- death_df |>
    dplyr::filter(is.na(remove1), year < 2024) |>
    dplyr::select(-dplyr::starts_with("remove")) |>
    dplyr::arrange(st_fips, year) |>
    dplyr::select(state,
                  st_fips,
                  year,
                  n_deaths,
                  pop,
                  adj_rate,
                  adj_rate_se,
                  dplyr::everything())

## Save ----
readr::write_csv(
    death_df |> dplyr::filter(year < 2020),
    here::here("data", "drug_deaths_1999_2019.csv")
)
readr::write_csv(death_df, here::here("data", "drug_deaths_1999_2023.csv"))
saveRDS(death_df, here::here("data", "drug_deaths_1999_2023.RDS"))
