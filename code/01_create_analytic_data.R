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
death_df <- bind_rows(
    read_delim(
        here(
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
        mutate(crude_rate = as.numeric(crude_rate),
               adj_rate = as.numeric(adj_rate)),
    read_delim(
        here(
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
        mutate(
            state = "US",
            st_fips = "99",
            .before = 1
        )
)

death_df <- death_df |>
    filter(is.na(remove1),
           year < 2024) |>
    select(-starts_with("remove")) |> 
    arrange(st_fips, year) |> 
    select(state, st_fips, year, n_deaths, pop, adj_rate, adj_rate_se, everything())

## Save ----
write_csv(death_df |> filter(year < 2020),
          here("data", "drug_deaths_1999_2019.csv"))
write_csv(death_df, here("data", "drug_deaths_1999_2023.csv"))
saveRDS(death_df, here("data", "drug_deaths_1999_2023.RDS"))
