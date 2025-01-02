## 02_gather_joinpoint_results.R ----
##
## After running the joinpoint regressions (see ./joinpoint), this file will
## take the exported text results and convert them into a unified dataframe.

## Imports ----
library(tidyverse)
library(here)

## Data ----
death_df <- readRDS(here::here("data", "drug_deaths_1999_2023.RDS"))

apc_df <-  readr::read_delim(
    here::here("joinpoint", "drug_deaths_1999_2019.apc.txt"),
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
    dplyr::select(-dplyr::starts_with("remove"))

jp_df <- readr::read_delim(
    here::here("joinpoint", "drug_deaths_1999_2019.data.txt"),
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
    dplyr::select(-dplyr::starts_with("remove"))

## Based on joinpoint model, extrapolate 2020-2023
extrapolated_rates <- jp_df |>
    dplyr::left_join(apc_df) |>
    dplyr::group_by(state) |>
    tidyr::fill(dplyr::starts_with("apc")) |>
    dplyr::ungroup()

extrapolated_rates <- extrapolated_rates |>
    dplyr::filter(year == 2019) |>
    dplyr::mutate(
        `2020` = model_rate * (1 + apc / 100)^1,
        `2021` = model_rate * (1 + apc / 100)^2,
        `2022` = model_rate * (1 + apc / 100)^3,
        `2023` = model_rate * (1 + apc / 100)^4
    ) |>
    dplyr::select(state, `2020`, `2021`, `2022`, `2023`) |>
    tidyr::pivot_longer(`2020`:`2023`, names_to = "year", values_to = "model_rate") |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::left_join(
        extrapolated_rates |>
            dplyr::filter(year == 2019) |>
            dplyr::mutate(
                `2020` = model_rate * (1 + apc_lower / 100)^1,
                `2021` = model_rate * (1 + apc_lower / 100)^2,
                `2022` = model_rate * (1 + apc_lower / 100)^3,
                `2023` = model_rate * (1 + apc_lower / 100)^4
            ) |>
            dplyr::select(state, `2020`, `2021`, `2022`, `2023`) |>
            tidyr::pivot_longer(`2020`:`2023`, names_to = "year", values_to = "model_rate_lower") |>
            dplyr::mutate(year = as.integer(year))
    ) |>
    dplyr::left_join(
        extrapolated_rates |>
            dplyr::filter(year == 2019) |>
            dplyr::mutate(
                `2020` = model_rate * (1 + apc_upper / 100)^1,
                `2021` = model_rate * (1 + apc_upper / 100)^2,
                `2022` = model_rate * (1 + apc_upper / 100)^3,
                `2023` = model_rate * (1 + apc_upper / 100)^4
            ) |>
            dplyr::select(state, `2020`, `2021`, `2022`, `2023`) |>
            tidyr::pivot_longer(`2020`:`2023`, names_to = "year", values_to = "model_rate_upper") |>
            dplyr::mutate(year = as.integer(year))
    )

## Combine with death data
death_df <- death_df |>
    dplyr::left_join(
        jp_df |>
            dplyr::select(state, year, model_rate) |>
            dplyr::bind_rows(extrapolated_rates) |>
            dplyr::arrange(state, year)
    )

## Add state abbreviations
death_df <- death_df |>
    dplyr::left_join(tibble::tibble(
        state = c("US", datasets::state.name, "District of Columbia"),
        abbrev = c("US", datasets::state.abb, "DC")
    ))

## Save ----
saveRDS(death_df, here::here("data", "joinpoint_results.RDS"))
