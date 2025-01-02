## Imports ----
library(tidyverse)
library(here)
library(geofacet)
source(here::here("code", "mk_nytimes.R"))

## Data ----
death_df <- readRDS(here::here("data", "joinpoint_results.RDS"))

## Plot ----
p0_with_CI <- ggplot2::ggplot(data = death_df) +
    ggplot2::geom_point(
        data = death_df |>
            dplyr::filter(year < 2020) |>
            dplyr::filter(abbrev != "ND", abbrev != "SD", state != "US"),
        ggplot2::aes(x = year, y = adj_rate),
        alpha = .2,
        size = .5
    ) +
    ggplot2::geom_point(
        data = death_df |>
            dplyr::filter(year >= 2020) |>
            dplyr::filter(abbrev != "ND", abbrev != "SD", state != "US"),
        ggplot2::aes(x = year, y = adj_rate),
        alpha = .75,
        size = .5
    ) +
    ggplot2::geom_line(
        data = death_df |>
            dplyr::filter(year < 2020) |>
            dplyr::filter(abbrev != "ND", abbrev != "SD", state != "US"),
        ggplot2::aes(x = year, y = model_rate),
        alpha = .5
    ) +
    ggplot2::geom_ribbon(
        data = death_df |>
            dplyr::filter(year >= 2020) |>
            dplyr::filter(abbrev != "ND", abbrev != "SD", state != "US"),
        ggplot2::aes(x = year, ymin = model_rate_lower, ymax = model_rate_upper),
        alpha = .5
    ) +
    ggplot2::geom_line(
        data = death_df |>
            dplyr::filter(year >= 2020) |>
            dplyr::filter(abbrev != "ND", abbrev != "SD", state != "US"),
        ggplot2::aes(x = year, y = model_rate),
        alpha = .9
    ) +
    mk_nytimes(
        panel.border = ggplot2::element_rect(fill = NA, color = "grey20"),
        strip.background = ggplot2::element_rect(fill = "grey90", color = "grey20")
    ) +
    ggplot2::scale_x_continuous(NULL,
                                breaks = seq(2000, 2020, 5),
                                labels = c("'00", "", "'10", "", "'20")) +
    ggplot2::scale_y_continuous(
        "Drug-related mortality rate (per 100,000)",
        breaks = seq(0, 100, 25),
        labels = c("0", "", "50", "", "100")
    ) +
    geofacet::facet_geo( ~ abbrev)

## Save ----
ggplot2::ggsave(
    here::here("plots", "figS1_fitted_vs_observed_with_ci.pdf"),
    p0_with_CI,
    width = 14,
    height = 10,
    scale = .8,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS1_fitted_vs_observed_with_ci.jpg"),
    p0_with_CI,
    width = 14,
    height = 10,
    scale = .8,
    dpi = 600
)
