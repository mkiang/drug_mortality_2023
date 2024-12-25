library(tidyverse)
library(here)
library(geofacet)
source(here("code", "mk_nytimes.R"))

## Data ----
jp_df <- readRDS(here("data", "joinpoint_results.RDS"))

avg_2016_2019 <- jp_df |>
    filter(year %in% 2016:2019) |>
    group_by(state, st_fips) |>
    summarize(avg_rate = mean(adj_rate)) |>
    ungroup()

jp_df <- jp_df |>
    left_join(avg_2016_2019)

p2 <- ggplot(
    jp_df |>
        filter(year %in% 2020:2022, state != "US"),
    aes(x = avg_rate, y = adj_rate, size = pop)
) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dotted") +
    geom_point(alpha = .4) +
    scale_size_area(
        "Population\n(in millions)",
        labels = function(x)
            round(x / 1000000)
    ) +
    scale_x_continuous("Average mortality rate, 2016-2019 (per 100,000)") +
    scale_y_continuous("Observed mortality rate (per 100,000)") +
    facet_grid(~ year) +
    # coord_equal() +
    mk_nytimes(
        panel.border = element_rect(fill = NA, color = "grey20"),
        strip.background = element_rect(fill = "grey90", color = "grey20"),
        legend.position = "right"
    ) 

ggsave(
    here("plots", "figS2_avg_rate_vs_observed.pdf"),
    p2,
    width = 11,
    height = 4,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS2_avg_rate_vs_observed.jpg"),
    p2,
    width = 11,
    height = 4,
    scale = 1,
    dpi = 600
)
