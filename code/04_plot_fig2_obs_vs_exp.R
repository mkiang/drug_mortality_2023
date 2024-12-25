
## Imports ----
library(tidyverse)
library(here)
library(ggsci)
source(here("code", "mk_nytimes.R"))

## Data ----
jp_df <- readRDS(here("data", "joinpoint_results.RDS")) |>
    filter(state != "US") |>
    filter(year >= 2020) |>
    mutate(
        significant = case_when(
            adj_rate > model_rate_upper ~ "Higher than predicted",
            adj_rate < model_rate_lower ~ "Lower than predicted",
            TRUE ~ "No difference"
        )
    ) |>
    mutate(significant_cat = factor(
        significant,
        levels = c("Higher than predicted", "Lower than predicted", "No difference"),
        ordered = TRUE
    ))

p1 <- ggplot(
    jp_df,
    aes(
        x = model_rate,
        y = adj_rate,
        xmin = model_rate_lower,
        xmax = model_rate_upper,
        color = significant_cat
    )
) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dotted") +
    geom_errorbarh(aes(height = 0), alpha = .5) +
    geom_point(alpha = .75) +
    scale_x_continuous("Predicted mortality rate (per 100,000)") +
    scale_y_continuous("Observed mortality rate (per 100,000)") +
    scale_color_manual(name = NULL,
                       values = c(ggsci::pal_jama()(2), "grey70")) +
    facet_wrap( ~ year) +
    coord_equal() +
    mk_nytimes(
        panel.border = element_rect(fill = NA, color = "grey20"),
        strip.background = element_rect(fill = "grey90", color = "grey20"),
        legend.position = "right"
    )

ggsave(
    here("plots", "fig2_obs_vs_predicted.pdf"),
    p1,
    width = 7,
    height = 4,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "fig2_obs_vs_predicted.jpg"),
    p1,
    width = 7,
    height = 4,
    scale = 1,
    dpi = 600
)
