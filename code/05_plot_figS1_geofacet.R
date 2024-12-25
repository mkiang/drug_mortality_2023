## Imports ----
library(tidyverse)
library(here)
library(geofacet)
source(here("code", "mk_nytimes.R"))

## Data ----
death_df <- readRDS(here("data", "joinpoint_results.RDS"))

p0_with_CI <- ggplot(data = death_df) +
    geom_point(
        data = death_df |>
            filter(year < 2020) |>
            filter(abbrev != "ND", abbrev != "SD", state != "US"),
        aes(x = year, y = adj_rate),
        alpha = .2,
        size = .5
    ) +
    geom_point(
        data = death_df |>
            filter(year >= 2020) |>
            filter(abbrev != "ND", abbrev != "SD", state != "US"),
        aes(x = year, y = adj_rate),
        alpha = .75,
        size = .5
    ) +
    geom_line(
        data = death_df |>
            filter(year < 2020) |>
            filter(abbrev != "ND", abbrev != "SD", state != "US"),
        aes(x = year, y = model_rate),
        alpha = .5
    ) +
    geom_ribbon(
        data = death_df |>
            filter(year >= 2020) |>
            filter(abbrev != "ND", abbrev != "SD", state != "US"),
        aes(x = year, ymin = model_rate_lower, ymax = model_rate_upper),
        alpha = .5
    ) +
    geom_line(
        data = death_df |>
            filter(year >= 2020) |>
            filter(abbrev != "ND", abbrev != "SD", state != "US"),
        aes(x = year, y = model_rate),
        alpha = .9
    ) +
    mk_nytimes(
        panel.border = element_rect(fill = NA, color = "grey20"),
        strip.background = element_rect(fill = "grey90", color = "grey20")
    ) +
    scale_x_continuous(NULL,
                       breaks = seq(2000, 2020, 5),
                       labels = c("'00", "", "'10", "", "'20")) +
    scale_y_continuous(
        "Drug-related mortality rate (per 100,000)",
        breaks = seq(0, 100, 25),
        labels = c("0", "", "50", "", "100")
    ) +
    facet_geo(~ abbrev)

ggsave(
    here("plots", "figS1_fitted_vs_observed_with_ci.pdf"),
    p0_with_CI,
    width = 14,
    height = 10,
    scale = .8,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS1_fitted_vs_observed_with_ci.jpg"),
    p0_with_CI,
    width = 14,
    height = 10,
    scale = .8,
    dpi = 600
)
