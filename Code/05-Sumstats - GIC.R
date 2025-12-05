library(dplyr)
library(tidyr)
library(ggplot2)
library(statar)

# ----------------------------------------------------
# FILTER SURVEYS
# ----------------------------------------------------

dftemp <- df %>%
    filter(survey %in% c("HIES_19", "LFS_23_imp"))

# ====================================================
# PART A — NATIONAL PERCENTILES
# ====================================================

dftemp_nat <- dftemp %>%
    group_by(survey) %>%
    mutate(
        pctile_nat = xtile(welfare, n = 100, w = popwt)
    ) %>%
    ungroup()

mean_nat <- dftemp_nat %>%
    group_by(survey, pctile_nat) %>%
    summarise(
        welfare_avg = weighted.mean(welfare, popwt, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = survey,
        values_from = welfare_avg
    ) %>%
    mutate(
        group = "National",
        growth_rate = (`LFS_23_imp` / `HIES_19`)^(1/4) - 1,
        pctile = pctile_nat
    ) %>%
    select(group, pctile, growth_rate)


# ====================================================
# PART B — URBAN / RURAL PERCENTILES (WITHIN SURVEY × URBAN)
# ====================================================

dftemp_urb <- dftemp %>%
    group_by(survey, urban) %>%
    mutate(
        pctile_urb = xtile(welfare, n = 100, w = popwt)
    ) %>%
    ungroup()

mean_urb <- dftemp_urb %>%
    group_by(survey, urban, pctile_urb) %>%
    summarise(
        welfare_avg = weighted.mean(welfare, popwt, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = survey,
        values_from = welfare_avg
    ) %>%
    mutate(
        growth_rate = (`LFS_23_imp` / `HIES_19`)^(1/4) - 1,
        group = urban,
        pctile = pctile_urb
    ) %>%
    select(group, pctile, growth_rate)

# ====================================================
# COMBINE NATIONAL + URBAN + RURAL
# ====================================================

final_plot_df <- bind_rows(mean_nat, mean_urb) %>%
    filter(pctile > 2, pctile < 98)

# ====================================================
# PLOT
# ====================================================

ggplot(final_plot_df, aes(x = pctile, y = growth_rate, color = group)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
        title = "Growth Incidence Curve (2019–2023)",
        x = "Percentile",
        y = "Annualized Growth Rate",
        color = "Population"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
    scale_color_manual(values = c("National" = "black", "Urban" = "blue", "Rural" = "darkgreen"))
