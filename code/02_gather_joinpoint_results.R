## 02_gather_joinpoint_results.R ----
## 
## After running the joinpoint regressions (see ./joinpoint), this file will
## take the exported text results and convert them into a unified dataframe. 

## Imports ----
library(tidyverse)
library(here)

## Data ----
death_df <- readRDS(here("data", "drug_deaths_1999_2023.RDS"))

apc_df <-  read_delim(
    here("joinpoint", "drug_deaths_1999_2019.apc.txt"),
    delim = ";",
    skip = 1,
    col_names = c(
        "state",
        "remove1",
        "remove2",
        "year",
        "remove3", 
        "apc",
        "apc_lower",
        "apc_upper",
        "remove4",
        "remove5",
        "remove6"
    )
) |> 
    select(-starts_with("remove"))

jp_df <- read_delim(
    here("joinpoint", "drug_deaths_1999_2019.data.txt"),
    delim = ";",
    skip = 1,
    col_names = c(
        "state",
        "year",
        "adj_rate",
        "model_rate",
        "adj_rate_se",
        "remove1",
        "remove4",
        "remove2",
        "remove3"
    )
) |> 
    select(-starts_with("remove"))

## Based on joinpoint model, extrapolate 2020-2023
extrapolated_rates <- jp_df |>
    left_join(apc_df) |>
    group_by(state) |>
    fill(starts_with("apc")) |>
    ungroup()

extrapolated_rates <- extrapolated_rates |>
    filter(year == 2019) |>
    mutate(
        `2020` = model_rate * (1 + apc / 100) ^ 1,
        `2021` = model_rate * (1 + apc / 100) ^ 2,
        `2022` = model_rate * (1 + apc / 100) ^ 3,
        `2023` = model_rate * (1 + apc / 100) ^ 4
    ) |>
    select(state, `2020`, `2021`, `2022`, `2023`) |>
    pivot_longer(`2020`:`2023`, names_to = "year", values_to = "model_rate") |>
    mutate(year = as.integer(year)) |>
    left_join(
        extrapolated_rates |>
            filter(year == 2019) |>
            mutate(
                `2020` = model_rate * (1 + apc_lower / 100) ^ 1,
                `2021` = model_rate * (1 + apc_lower / 100) ^ 2,
                `2022` = model_rate * (1 + apc_lower / 100) ^ 3,
                `2023` = model_rate * (1 + apc_lower / 100) ^ 4
            ) |>
            select(state, `2020`, `2021`, `2022`, `2023`) |>
            pivot_longer(`2020`:`2023`, names_to = "year", values_to = "model_rate_lower") |>
            mutate(year = as.integer(year))
    ) |>
    left_join(
        extrapolated_rates |>
            filter(year == 2019) |>
            mutate(
                `2020` = model_rate * (1 + apc_upper / 100) ^ 1,
                `2021` = model_rate * (1 + apc_upper / 100) ^ 2,
                `2022` = model_rate * (1 + apc_upper / 100) ^ 3,
                `2023` = model_rate * (1 + apc_upper / 100) ^ 4
            ) |>
            select(state, `2020`, `2021`, `2022`, `2023`) |>
            pivot_longer(`2020`:`2023`, names_to = "year", values_to = "model_rate_upper") |>
            mutate(year = as.integer(year))
    )

## Combine with death data
death_df <- death_df |>
    left_join(
        jp_df |>
            select(state, year, model_rate) |>
            bind_rows(extrapolated_rates) |> 
            arrange(state, year)
    )

## Add state abbreviations
death_df <- death_df |> 
    left_join(
        tibble(
            state = c("US", state.name, "District of Columbia"),
            abbrev = c("US", state.abb, "DC")
        )
    ) 

## Save ----
saveRDS(death_df, here("data", "joinpoint_results.RDS"))
