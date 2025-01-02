library(tidyverse)
library(here)
library(geofacet)
source(here::here("code", "mk_nytimes.R"))

## Data ----
jp_df <- readRDS(here::here("data", "joinpoint_results.RDS"))

avg_2016_2019 <- jp_df |>
    dplyr::filter(year %in% 2016:2019) |>
    dplyr::group_by(state, st_fips) |>
    dplyr::summarize(avg_rate = mean(adj_rate)) |>
    dplyr::ungroup()

jp_df <- jp_df |>
    dplyr::left_join(avg_2016_2019)

## Plot ----
p2 <- ggplot2::ggplot(
    jp_df |>
        dplyr::filter(year %in% 2020:2022, state != "US"),
    ggplot2::aes(x = avg_rate, y = adj_rate, size = pop)
) +
    ggplot2::geom_abline(intercept = 0,
                         slope = 1,
                         linetype = "dotted") +
    ggplot2::geom_point(alpha = .4) +
    ggplot2::scale_size_area(
        "Population\n(in millions)",
        labels = function(x)
            round(x / 1000000)
    ) +
    ggplot2::scale_x_continuous("Average mortality rate, 2016-2019 (per 100,000)") +
    ggplot2::scale_y_continuous("Observed mortality rate (per 100,000)") +
    ggplot2::facet_grid( ~ year) +
    mk_nytimes(
        panel.border = ggplot2::element_rect(fill = NA, color = "grey20"),
        strip.background = ggplot2::element_rect(fill = "grey90", color = "grey20"),
        legend.position = "right"
    )

## Save ----
ggplot2::ggsave(
    here::here("plots", "figS2_avg_rate_vs_observed.pdf"),
    p2,
    width = 11,
    height = 4,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS2_avg_rate_vs_observed.jpg"),
    p2,
    width = 11,
    height = 4,
    scale = 1,
    dpi = 600
)
