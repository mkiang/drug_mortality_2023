## Imports ----
library(tidyverse)
library(here)
library(usmap)

## Data ----
death_df <- readRDS(here::here("data", "joinpoint_results.RDS")) |>
    dplyr::filter(state != "US")

summarized_df <- death_df |>
    dplyr::filter(year >= 2020) |>
    dplyr::group_by(state, st_fips) |>
    dplyr::summarize(
        n = dplyr::n_distinct(year),
        n_gt_expected = sum(adj_rate > model_rate_upper),
        n_lt_expected = sum(adj_rate < model_rate_lower)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(n_expected = n - n_gt_expected - n_lt_expected) |>
    dplyr::mutate(conflicting = (n_gt_expected > 0 &
                                     n_lt_expected > 0) + 0) |>
    dplyr::mutate(
        status = dplyr::case_when(
            n_gt_expected == 4 ~ "consistently_high_4",
            n_gt_expected == 3 &
                n_expected == 1 ~ "consistently_high_3",
            n_lt_expected == 4 ~ "consistently_low_4",
            TRUE ~ "mixed"
        )
    ) |>
    dplyr::mutate(status_cat = factor(
        status,
        levels = c(
            "consistently_high_4",
            "consistently_high_3",
            "mixed",
            "consistently_low_4"
        ),
        labels = c(
            "Higher than expected all four years (N=26)",
            "Higher than expected 3/4 years (N=9)",
            "Mixed / as expected (N=14)",
            "Lower than expected all four years (N=2)"
        ),
        ordered = TRUE
    )) |>
    dplyr::left_join(usmapdata::us_map() |>
                         dplyr::select(state = full, st_fips = fips),)

## Plots ----
p1 <- ggplot2::ggplot(summarized_df, ggplot2::aes(geometry = geom, fill = status_cat)) +
    ggplot2::geom_sf(color = "black") +
    ggplot2::scale_fill_manual(
        "Observed mortality rate compared\nto expected mortality rates, 2020-2023",
        values = c("#D7191C", "#FDAE61", "#FFFFBF", "#2B83BA")
    ) +
    ggplot2::theme_void()

## Save ----
ggplot2::ggsave(
    here::here("plots", "fig1_map.pdf"),
    p1,
    width = 7,
    height = 3.5,
    scale = 1.4,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "fig1_map.jpg"),
    p1,
    width = 7,
    height = 3.5,
    scale = 1.4,
    dpi = 600
)
