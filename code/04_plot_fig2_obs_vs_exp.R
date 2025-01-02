## Imports ----
library(tidyverse)
library(here)
library(ggsci)
source(here::here("code", "mk_nytimes.R"))

## Data ----
jp_df <- readRDS(here::here("data", "joinpoint_results.RDS")) |>
    dplyr::filter(state != "US") |>
    dplyr::filter(year >= 2020) |>
    dplyr::mutate(
        significant = dplyr::case_when(
            adj_rate > model_rate_upper ~ "Higher than predicted",
            adj_rate < model_rate_lower ~ "Lower than predicted",
            TRUE ~ "No difference"
        )
    ) |>
    dplyr::mutate(significant_cat = factor(
        significant,
        levels = c("Higher than predicted", "Lower than predicted", "No difference"),
        ordered = TRUE
    ))

p1 <- ggplot2::ggplot(
    jp_df,
    ggplot2::aes(
        x = model_rate,
        y = adj_rate,
        xmin = model_rate_lower,
        xmax = model_rate_upper,
        color = significant_cat
    )
) +
    ggplot2::geom_abline(intercept = 0,
                         slope = 1,
                         linetype = "dotted") +
    ggplot2::geom_errorbarh(ggplot2::aes(height = 0), alpha = .5) +
    ggplot2::geom_point(alpha = .75) +
    ggplot2::scale_x_continuous("Predicted mortality rate (per 100,000)") +
    ggplot2::scale_y_continuous("Observed mortality rate (per 100,000)") +
    ggplot2::scale_color_manual(name = NULL,
                                values = c(ggsci::pal_jama()(2), "grey70")) +
    ggplot2::facet_wrap(~ year) +
    ggplot2::coord_equal() +
    mk_nytimes(
        panel.border = ggplot2::element_rect(fill = NA, color = "grey20"),
        strip.background = ggplot2::element_rect(fill = "grey90", color = "grey20"),
        legend.position = "right"
    )

ggplot2::ggsave(
    here::here("plots", "fig2_obs_vs_predicted.pdf"),
    p1,
    width = 7,
    height = 4,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "fig2_obs_vs_predicted.jpg"),
    p1,
    width = 7,
    height = 4,
    scale = 1,
    dpi = 600
)
