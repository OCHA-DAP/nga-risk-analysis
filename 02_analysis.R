source("01_aggregation.R")
library(sf)
library(tmaptools)
library(gghdx)
library(ggrepel)
library(biscale)
library(cowplot)
library(ggcorrplot)
gghdx()

##################################
#### ADDITIONAL DATA AND DIRS ####
##################################

# adm2 geography for plotting
adm2 <- st_read(
  file.path(
    data_dir,
    "geoBoundaries-NGA-ADM2.geojson"
  )
)

bay_adm2 <- adm2 %>%
  filter(
    shapeName %in% unique(df_constraints$lga)
  ) %>%
  rename(
    lga = shapeName
  )

# create adm1 dataset since HDX admin1 data is misaligned
bay_adm1 <- bay_adm2 %>%
  left_join(
    distinct(df_constraints, state, lga),
    by = "lga"
  ) %>%
  group_by(state) %>%
  summarize(
    level = "ADM1"
  )

# get aspect ratio for BAY states to ensure proper saving of maps
p_rat <- get_asp_ratio(bay_adm2)

plot_dir <- file.path(
  nga_dir,
  "plots"
)

############################
#### LABELING FUNCTIONS ####
############################

# function to convert first string to upper case used for labeling
# some of the likert scales with ggplot
proper <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}

likert_labeller <- function(x) {
  str_replace(x, "_", " ") %>%
    proper()
}

constraints_labeller <- c(
  movement_restriction = "Restriction of movement",
  needs_denial = "Denial of humanitarian needs",
  forced_movement = "Forced movement of populations",
  access_restrictions = "Restriction/delay of access for humanitarians",
  scale_down = "Closure/scale down of operations",
  asset_destruction = "Destruction of humanitarian assets",
  supplies_inaccessible = "Inability to acquire relief supplies",
  cost_increases = "Increase in cost of operations",
  humanitarian_footprint_reduction = "Reduction in humanitarian footprint",
  loss_road_access = "Loss of road access",
  other = "Other"
)

risk_labeller <- c(
  price_inflation = "Price inflation",
  floods_natural = "Floods",
  nsag_activities = "Escalation of NSAGs activities",
  disease_outbreak = "Disease outbreak",
  camp_closure = "Closure of camps",
  sandstorms = "Sandstorms",
  election_activities = "Election activities",
  other = "Other",
  communal_violence = "Communal violence",
  drought = "Drought"
)

#######################################
#### GENERAL CHECKING OF RESPONSES ####
#######################################

p_responses_map <- df_constraints %>%
  group_by(
    lga
  ) %>%
  summarize(
    n = n()
  ) %>%
  left_join(
    bay_adm2,
    by = "lga"
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    aes(
      fill = n
    )
  ) +
  geom_sf(
    data = bay_adm1,
    fill = NA,
    lwd = 0.75
  ) +
  coord_sf(
    datum = NA
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("sapphire-hdx"),
    labels = scales::percent,
    breaks = scales::breaks_pretty(3)
  ) +
  geom_sf_text(
    aes(
      label = n
    ),
    color = "black",
    size = 2
  ) +
  labs(
    title = "Survey coverage",
    fill = "# of\nrespondents",
    x = "",
    y = ""
  )

ggsave(
  file.path(
    plot_dir,
    "coverage_map.png"
  ),
  p_responses_map,
  height = 6,
  width = p_rat * 5,
  units = "in"
)

#################################
#### ANALYSIS OF CONSTRAINTS ####
#################################

# constraints overall

p_constraints <- df_constraints %>%
  group_by(
    state, constraints
  ) %>%
  summarize(
    n = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    freq = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = constraints,
      y = state
    )
  ) +
  geom_tile(
    aes(
      fill = freq
    ),
    color = "white"
  ) +
  geom_text_hdx(
    aes(
      label = scales::percent(freq, 1)
    ),
    color = "white"
  ) +
  scale_fill_gradient_hdx_tomato(
    labels = scales::percent
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  labs(
    y = "",
    x = "",
    fill = "% of\nresponses",
    title = "Reported severity of operational constraints"
  ) +
  theme(
    axis.line.x = element_blank()
  )

ggsave(
  file.path(
    plot_dir,
    "constraints.png"
  ),
  p_constraints,
  height = 2.5,
  width = 5,
  units = "in"
)

# constraints cause

df_plot_causes <- df_constraints %>%
  group_by(
    state
  ) %>%
  summarize(
    across(
      .cols = movement_restriction:scale_down,
      .fns = ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    -state
  )

lvls <- df_plot_causes %>%
  group_by(
    name
  ) %>%
  summarize(
    value = mean(value)
  ) %>%
  arrange(
    value
  ) %>%
  pull(name)

p_causes <- df_plot_causes %>%
  ggplot(
    aes(
      y = factor(name, levels = lvls),
      x = fct_rev(state)
    )
  ) +
  geom_tile(
    aes(
      fill = value
    ),
    color = "white"
  ) +
  geom_text_hdx(
    aes(
      label = scales::percent(value, 1)
    ),
    color = "white"
  ) +
  scale_fill_gradient_hdx_tomato(
    labels = scales::percent
  ) +
  scale_y_discrete(
    labels = constraints_labeller
  ) +
  labs(
    y = "",
    x = "",
    fill = "% of\nresponses",
    title = "Cause of operational constraints",
    subtitle = "Only asked to respondents indicating medium to very high constraints"
  ) +
  theme(
    axis.line.x = element_blank()
  )

ggsave(
  file.path(
    plot_dir,
    "causes.png"
  ),
  p_causes,
  height = 6,
  width = 6,
  units = "in"
)

# plot map of severity of operational constraints

p_constraints_map <- df_constraints %>%
  mutate(
    constraints_high = constraints %in% c("high", "very_high")
  ) %>%
  group_by(
    lga
  ) %>%
  summarize(
    constraints_high = mean(constraints_high)
  ) %>%
  left_join(
    bay_adm2,
    by = "lga"
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    aes(
      fill = constraints_high
    )
  ) +
  geom_sf(
    data = bay_adm1,
    fill = NA,
    lwd = 0.75
  ) +
  # geom_text_repel(
  #   aes(
  #     label = round(100 * constraints_high, 0),
  #     geometry = geometry
  #   ),
  #   stat = "sf_coordinates",
  #   color = "black",
  #   size = 2,
  #   force = 0.001
  # ) +
  coord_sf(
    datum = NA
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    labels = scales::percent
  ) +
  labs(
    title = "Operational constraints",
    subtitle = "LGAs reporting medium to very high constraints",
    fill = "% of\nrespondents"
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "constraints_map.png"
  ),
  p_constraints_map,
  height = 6,
  width = p_rat * 5,
  units = "in"
)

# produce maps of different causes of constraints

iwalk(
  .x = constraints_labeller,
  .f = function(x, y) {
    p <- df_constraints %>%
      group_by(
        lga
      ) %>%
      summarize(
        pct = mean(.data[[y]], na.rm = TRUE)
      ) %>%
      left_join(
        bay_adm2,
        by = "lga"
      ) %>%
      st_as_sf() %>%
      ggplot() +
      geom_sf(
        aes(
          fill = pct
        )
      ) +
      geom_sf(
        data = bay_adm1,
        fill = NA,
        lwd = 0.75
      ) +
      coord_sf(
        datum = NA
      ) +
      scale_fill_gradient(
        low = "white",
        high = hdx_hex("tomato-hdx"),
        labels = scales::percent,
        breaks = scales::breaks_pretty(3)
      ) +
      labs(
        title = "Operational constraints:",
        subtitle = x,
        fill = "% of\nrespondents"
      ) +
      expand_limits(
        fill = 0
      )

      ggsave(
        filename = file.path(
          plot_dir,
          paste0("constraints_", y, "_map.png")
        ),
        plot = p,
        height = 6,
        width = p_rat * 5,
        units = "in"
      )
  }
)

########################
#### ANALYZING RISK ####
########################

# were the methods understood?

p_risk_corr <- df_risk %>%
  filter(
    !is.na(severity_current)
  ) %>%
  group_by(
    severity_current,
    severity_future
  ) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    freq = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = severity_current,
      y = severity_future
    )
  ) +
  geom_tile(
    aes(
      fill = freq
    ),
    color = "white"
  ) +
  geom_text_hdx(
    aes(
      label = scales::percent(freq, 1)
    ),
    color = "white"
  ) +
  scale_fill_gradient_hdx_tomato(
    labels = scales::label_percent()
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  scale_y_discrete(
    labels = likert_labeller
  ) +
  labs(
    x = "Risk (current)",
    y = "Risk (future)",
    fill = "% of\nresponses",
    title = "Correlation between responses of future and current risk"
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "risk_corr.png"
  ),
  p_risk_corr,
  height = 6,
  width = 6,
  units = "in"
)


# were the methods understood? normalizing

p_risk_corr_norm <- df_risk %>%
  filter(
    !is.na(severity_current)
  ) %>%
  group_by(
    severity_current,
    severity_future
  ) %>%
  summarize(
    n = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    freq = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = severity_current,
      y = severity_future
    )
  ) +
  geom_tile(
    aes(
      fill = freq
    ),
    color = "white"
  ) +
  geom_text_hdx(
    aes(
      label = scales::percent(freq, 1)
    ),
    color = "white"
  ) +
  scale_fill_gradient_hdx_tomato(
    labels = scales::label_percent()
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  scale_y_discrete(
    labels = likert_labeller
  ) +
  labs(
    x = "Risk (current)",
    y = "Risk (future)",
    fill = "% of\nresponses",
    title = "Correlation between responses of future and current risk",
    subtitle = "% calculated for current severity separately"
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "risk_corr_norm.png"
  ),
  p_risk_corr_norm,
  height = 6,
  width = 6,
  units = "in"
)

# look at the current risk because it doesn't seem like the idea of future risk
# was properly caught by the population

df_risk_plot <- df_risk %>%
  filter(
    !is.na(severity_current)
  ) %>%
  distinct(
    `_id`, risk, severity_current
  ) %>%
  group_by(risk, severity_current) %>%
  summarize(
    n = n(),
    freq = n / nrow(df)
  )

# get levels to sort the risks by those with highest severity
risk_lvls_current <- df_risk_plot %>%
  filter(
    severity_current == "very_high"
  ) %>%
  arrange(
    freq
  ) %>%
  pull(
    risk
  )

p_risk_current <- df_risk_plot %>%
  ggplot(
    aes(
      x = severity_current,
      y = factor(risk, levels = risk_lvls_current)
    )
  ) +
  geom_tile(
    aes(
      fill = freq
    ),
    color = "white"
  ) +
  geom_text_hdx(
    aes(
      label = scales::percent(freq, 1)
    ),
    color = "white"
  ) +
  scale_fill_gradient_hdx_tomato(
    labels = scales::percent
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  scale_y_discrete(
    labels = risk_labeller
  ) +
  labs(
    y = "",
    x = "",
    fill = "% of\nresponses",
    title = "Reported current risk severity",
    subtitle = "Considering all responses across the BAY states"
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "risk_current.png"
  ),
  p_risk_current,
  height = 7,
  width = 6,
  units = "in"
)

# look at the future risk just to check

df_risk_plot_future <- df_risk %>%
  filter(
    !is.na(severity_current)
  ) %>%
  distinct(
    `_id`, risk, severity_future
  ) %>%
  group_by(risk, severity_future) %>%
  summarize(
    n = n(),
    freq = n / nrow(df)
  )

# get levels to sort the risks by those with highest severity
risk_lvls_future <- df_risk_plot_future %>%
  filter(
    severity_future == "very_high"
  ) %>%
  arrange(
    freq
  ) %>%
  pull(
    risk
  )

p_risk_future <- df_risk_plot_future %>%
  ggplot(
    aes(
      x = severity_future,
      y = factor(risk, levels = risk_lvls_future)
    )
  ) +
  geom_tile(
    aes(
      fill = freq
    ),
    color = "white"
  ) +
  geom_text_hdx(
    aes(
      label = scales::percent(freq, 1)
    ),
    color = "white"
  ) +
  scale_fill_gradient_hdx_tomato(
    labels = scales::percent
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  scale_y_discrete(
    labels = risk_labeller
  ) +
  labs(
    y = "",
    x = "",
    fill = "% of\nresponses",
    title = "Reported future risk severity (evolution)",
    subtitle = "Considering all responses across the BAY states"
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "risk_future.png"
  ),
  p_risk_future,
  height = 7,
  width = 6,
  units = "in"
)

# normalize data by looking across LGA reporting to see most widespread risks
df_risk_hist <- df_risk %>%
  select(
    lga:severity_current
  ) %>%
  group_by(
    lga,
    risk
  ) %>%
  summarize(
    freq = sum(severity_current %in% c("medium", "high", "very_high"), na.rm = TRUE) / n()
  )

hist_lvls <- df_risk_hist %>%
  group_by(
    risk
  ) %>%
  summarize(
    freq = mean(freq)
  ) %>%
  arrange(
    -freq
  ) %>%
  pull(risk)

p_risk_hist <- df_risk_hist %>%
  ggplot(
    aes(
      x = freq
    )
  ) +
  geom_histogram() +
  facet_wrap(
    ~factor(risk, levels = hist_lvls),
    ncol = 1,
    labeller = as_labeller(risk_labeller)
  ) +
  scale_x_continuous(
    labels = scales::percent
  ) +
  labs(
    x = "% of respondents reporting\nmedium to very high severity, by LGA",
    y = "# of LGAs",
    title = "Distribution of risk severity across LGAs"
  )

ggsave(
  file.path(
    plot_dir,
    "risk_hist.png"
  ),
  p_risk_hist,
  height = 8,
  width = 4.5,
  units = "in"
)

# distribution of risks across the country

iwalk(
  .x = risk_labeller,
  .f = function(x, y) {
    p <- df_risk %>%
      filter(
        risk == y
      ) %>%
      group_by(
        lga
      ) %>%
      summarize(
        pct = sum(
          severity_current %in% c("medium", "high", "very_high"),
          na.rm = TRUE
        ) / n()
      ) %>%
      left_join(
        bay_adm2,
        by = "lga"
      ) %>%
      st_as_sf() %>%
      ggplot() +
      geom_sf(
        aes(
          fill = pct
        )
      ) +
      geom_sf(
        data = bay_adm1,
        fill = NA,
        lwd = 0.75
      ) +
      coord_sf(
        datum = NA
      ) +
      scale_fill_gradient(
        low = "white",
        high = hdx_hex("tomato-hdx"),
        labels = scales::percent,
        breaks = scales::breaks_pretty(3)
      ) +
      labs(
        title = "Medium to very high severity:",
        subtitle = x,
        fill = "% of\nrespondents"
      ) +
      expand_limits(
        fill = 0
      )

    ggsave(
      filename = file.path(
        plot_dir,
        paste0("risks_m_to_vh_", y, "_map.png")
      ),
      plot = p,
      height = 6,
      width = p_rat * 5,
      units = "in"
    )
  }
)

# also look at high to very high severity

iwalk(
  .x = risk_labeller,
  .f = function(x, y) {
    p <- df_risk %>%
      filter(
        risk == y
      ) %>%
      group_by(
        lga
      ) %>%
      summarize(
        pct = sum(
          severity_current %in% c("medium", "high", "very_high"),
          na.rm = TRUE
        ) / n()
      ) %>%
      left_join(
        bay_adm2,
        by = "lga"
      ) %>%
      st_as_sf() %>%
      ggplot() +
      geom_sf(
        aes(
          fill = pct
        )
      ) +
      geom_sf(
        data = bay_adm1,
        fill = NA,
        lwd = 0.75
      ) +
      coord_sf(
        datum = NA
      ) +
      scale_fill_gradient(
        low = "white",
        high = hdx_hex("tomato-hdx"),
        labels = scales::percent,
        breaks = scales::breaks_pretty(3)
      ) +
      labs(
        title = "High to very high severity:",
        subtitle = x,
        fill = "% of\nrespondents"
      ) +
      expand_limits(
        fill = 0
      )

    ggsave(
      filename = file.path(
        plot_dir,
        paste0("risks_h_to_vh_", y, "_map.png")
      ),
      plot = p,
      height = 6,
      width = p_rat * 5,
      units = "in"
    )
  }
)

# also look at just very high severity

iwalk(
  .x = risk_labeller,
  .f = function(x, y) {
    p <- df_risk %>%
      filter(
        risk == y
      ) %>%
      group_by(
        lga
      ) %>%
      summarize(
        pct = sum(
          severity_current %in% c("very_high"),
          na.rm = TRUE
        ) / n()
      ) %>%
      left_join(
        bay_adm2,
        by = "lga"
      ) %>%
      st_as_sf() %>%
      ggplot() +
      geom_sf(
        aes(
          fill = pct
        )
      ) +
      geom_sf(
        data = bay_adm1,
        fill = NA,
        lwd = 0.75
      ) +
      coord_sf(
        datum = NA
      ) +
      scale_fill_gradient(
        low = "white",
        high = hdx_hex("tomato-hdx"),
        labels = scales::percent,
        breaks = scales::breaks_pretty(3)
      ) +
      labs(
        title = "Very high severity:",
        subtitle = x,
        fill = "% of\nrespondents"
      ) +
      expand_limits(
        fill = 0
      )

    ggsave(
      filename = file.path(
        plot_dir,
        paste0("risks_vh_", y, "_map.png")
      ),
      plot = p,
      height = 6,
      width = p_rat * 5,
      units = "in"
    )
  }
)

# checking the reported risks all together, how frequently respondents answered
# the same level of severity for all risks
df_risk_rep <- df_risk %>%
  filter(
    !is.na(severity_current)
  ) %>%
  distinct(
    `_id`,
    risk,
    severity_current
  ) %>%
  group_by(
    `_id`
  ) %>%
  summarize(
    risks = paste(likert_labeller(severity_current), collapse = ", "),
    risks_sd = sd(as.numeric(severity_current)),
    risks_sd = ifelse(is.na(risks_sd), 0, risks_sd),
    num_responses = n(),
    num_unique = length(unique(severity_current))
  )

df_risk_rep %>%
  count(
    risks,
    risks_sd
  ) %>%
  arrange(
    desc(n)
  ) %>%
  mutate(
    perc = n / sum(n),
    risks = factor(risks, levels = rev(risks))
  ) %>%
  ggplot(
    aes(
      y = risks,
      x = perc,
      fill = risks_sd
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  theme(
    axis.text.y = element_blank(),
    plot.background = element_rect(
      fill = "white"
    )
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  )

p_risk_tend <- df_risk_rep %>%
  count(
    num_responses,
    num_unique
  ) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = num_responses,
      y = num_unique
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    ),
    color = "white"
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("sapphire-hdx"),
    labels = scales::label_percent(1),
    breaks = scales::breaks_pretty(3)
  ) +
  geom_text_hdx(
    aes(
      label = scales::label_percent(1)(perc)
    ),
    color = "black"
  ) +
  labs(
    x = "# of risks reported",
    y = "# of unique severity values reported",
    fill = "% of\nresponses",
    title = "Tendency to report multiple risks"
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "risk_tendency.png"
  ),
  p_risk_tend,
  height = 5,
  width = 5,
  units = "in"
)

# aggregating risk
df_risk_max <- df_risk %>%
  group_by(
    `_id`,
    lga
  ) %>%
  summarize(
    severity_current = max(severity_current, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    !is.na(severity_current)
  )

# how often are we getting extremely high values using max

p_max_skew <- df_risk_max %>%
  count(
    severity_current
  ) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  ggplot(
    aes(
      y = severity_current,
      x = perc
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  scale_y_discrete(
    labels = likert_labeller
  ) +
  scale_x_continuous(
    labels = scales::label_percent(1)
  ) +
  labs(
    x = "% of responses",
    y = "Maximum severity reported by respondent",
    title = "Looking at maximum severity skews high"
  )

ggsave(
  file.path(
    plot_dir,
    "risk_max_skew.png"
  ),
  p_max_skew,
  height = 5,
  width = 5,
  units = "in"
)

# look only at places based on reporting of very high severity
p_vh_risk_map <- df_risk_max %>%
  group_by(
    lga
  ) %>%
  summarize(
    perc = sum(severity_current == "very_high") / n()
  ) %>%
  left_join(
    bay_adm2,
    by = "lga"
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    aes(
      fill = perc
    )
  ) +
  geom_sf(
    data = bay_adm1,
    fill = NA,
    lwd = 0.75
  ) +
  coord_sf(
    datum = NA
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    labels = scales::percent,
    breaks = scales::breaks_pretty(3)
  ) +
  labs(
    title = "Presence of very high risks",
    fill = "% of\nresponses",
    x = "",
    y = ""
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "vh_risk_map.png"
  ),
  p_vh_risk_map,
  height = 6,
  width = p_rat * 5,
  units = "in"
)

p_vh_risk_map <- df_risk_max %>%
  group_by(
    lga
  ) %>%
  summarize(
    perc = sum(severity_current == "very_high") / n()
  ) %>%
  left_join(
    bay_adm2,
    by = "lga"
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    aes(
      fill = perc
    )
  ) +
  geom_sf(
    data = bay_adm1,
    fill = NA,
    lwd = 0.75
  ) +
  coord_sf(
    datum = NA
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    labels = scales::percent,
    breaks = scales::breaks_pretty(3)
  ) +
  labs(
    title = "Presence of very high risks",
    fill = "% of\nresponses",
    x = "",
    y = ""
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "vh_risk_map.png"
  ),
  p_vh_risk_map,
  height = 6,
  width = p_rat * 5,
  units = "in"
)

##################
#### CAPACITY ####
##################

# is it related to risk?

p_risk_cap <- df_risk %>%
  distinct(
    `_id`,
    risk,
    severity_current,
    capacity
  ) %>%
  filter(
    !is.na(capacity)
  ) %>%
  count(
    severity_current,
    capacity
  ) %>%
  group_by(
    severity_current
  ) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = severity_current,
      y = capacity
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    )
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    labels = scales::label_percent(1),
    breaks = scales::breaks_pretty(3)
  ) +
  geom_text_hdx(
    aes(
      label = scales::label_percent(1)(perc)
    ),
    color = "black"
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  scale_y_discrete(
    labels = likert_labeller
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  labs(
    x = "Reported severity of risk",
    y = "Reported capacity to respond",
    fill = "% of responses",
    title = "Relationship between risk severity and capacity"
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "risk_cap.png"
  ),
  p_risk_cap,
  height = 6,
  width = 6,
  units = "in"
)

# is it related to risk? normalize

p_risk_cap_norm <- df_risk %>%
  distinct(
    `_id`,
    risk,
    severity_current,
    capacity
  ) %>%
  filter(
    !is.na(capacity)
  ) %>%
  count(
    severity_current,
    capacity
  ) %>%
  group_by(
    severity_current
  ) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = severity_current,
      y = capacity
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    )
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    labels = scales::label_percent(1),
    breaks = scales::breaks_pretty(3)
  ) +
  geom_text_hdx(
    aes(
      label = scales::label_percent(1)(perc)
    ),
    color = "black"
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  scale_y_discrete(
    labels = likert_labeller
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  labs(
    x = "Reported severity of risk",
    y = "Reported capacity to respond",
    fill = "% of responses\nreporting that level of severity",
    title = "Relationship between risk severity and capacity",
    subtitle = "Looking across reported levels of severity"
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "risk_cap_norm.png"
  ),
  p_risk_cap_norm,
  height = 6,
  width = 6,
  units = "in"
)

# let's ignore those problems and just look across the types of risks to see
# capacity to respond in general
df_cap <- df_risk %>%
  filter(
    !is.na(severity_current)
  ) %>%
  group_by(
    risk,
    capacity
  ) %>%
  summarize(
    n = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    perc = n / sum(n)
  )

cap_lvls <- df_cap %>%
  summarize(
    cap = sum(perc[capacity %in% c("very_low", "low")])
  ) %>%
  arrange(
    cap
  ) %>%
  pull(risk)

p_cap <- df_cap %>%
  ggplot(
    aes(
      x = capacity,
      y = factor(risk, cap_lvls)
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    ),
    color = "white"
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  scale_y_discrete(
    labels = risk_labeller
  ) +
  labs(
    y = "",
    x = "Capacity to respond",
    fill = "% of\nresponses",
    title = "Capacity to respond to various risks"
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("mint-hdx"),
    labels = scales::label_percent(1)
  ) +
  geom_text_hdx(
    aes(
      label = scales::label_percent(1)(perc)
    ),
    color = "black"
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "capacities.png"
  ),
  p_cap,
  height = 6,
  width = 6,
  units = "in"
)

#############################
#### ANTICIPATORY ACTION ####
#############################

p_aa <- df_risk %>%
  distinct(
    `_id`,
    risk,
    anticipatory_yn
  ) %>%
  filter(
    !is.na(anticipatory_yn)
  ) %>%
  group_by(
    risk
  ) %>%
  summarize(
    perc = sum(anticipatory_yn == "yes") / n()
  ) %>%
  arrange(
    perc
  ) %>%
  mutate(
    risk = factor(risk, risk)
  ) %>%
  ggplot(
    aes(
      y = risk,
      x = perc
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  scale_y_discrete(
    labels = risk_labeller
  ) +
  scale_x_continuous(
    labels = scales::label_percent(1)
  ) +
  labs(
    y = "",
    x = "% responses reporting AA in place for risk",
    title = "Anticipatory action in place for risks"
  )

ggsave(
  file.path(
    plot_dir,
    "aa.png"
  ),
  p_aa,
  height = 6,
  width = 6,
  units = "in"
)

# impacts of anticipatory action

df_aa_impact <- df_risk %>%
  distinct(
    `_id`,
    risk,
    anticipatory_impact
  ) %>%
  filter(
    !is.na(anticipatory_impact)
  ) %>%
  group_by(
    risk,
    anticipatory_impact
  ) %>%
  summarize(
    n = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    perc = n / sum(n)
  )

aa_impact_lvls <- df_aa_impact %>%
  summarize(
    perc = sum(perc[str_detect(anticipatory_impact, "sign")]),
    .groups = "drop"
  ) %>%
  arrange(
    perc
  ) %>%
  pull(
    risk
  )

p_aa_impact <- df_aa_impact %>%
  ggplot(
    aes(
      y = factor(risk, aa_impact_lvls),
      x = anticipatory_impact
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    )
  ) +
  scale_y_discrete(
    labels = risk_labeller
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  labs(
    y = "",
    x = "Potential impact of AA on response",
    title = "Potential benefits of anticipatory action",
    fill = "% of\nresponses"
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("mint-hdx"),
    labels = scales::label_percent(1),
    breaks = scales::breaks_pretty(3)
  ) +
  geom_text_hdx(
    aes(
      label = scales::label_percent(1)(perc)
    ),
    color = "black"
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "aa_impact.png"
  ),
  p_aa_impact,
  height = 6,
  width = 7,
  units = "in"
)

# aa versus risk

p_risk_aa_norm <- df_risk %>%
  distinct(
    `_id`,
    risk,
    severity_current,
    anticipatory_impact
  ) %>%
  filter(
    !is.na(anticipatory_impact)
  ) %>%
  count(
    severity_current,
    anticipatory_impact
  ) %>%
  group_by(
    severity_current
  ) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = severity_current,
      y = anticipatory_impact
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    )
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    labels = scales::label_percent(1),
    breaks = scales::breaks_pretty(3)
  ) +
  geom_text_hdx(
    aes(
      label = scales::label_percent(1)(perc)
    ),
    color = "black"
  ) +
  scale_x_discrete(
    labels = likert_labeller
  ) +
  scale_y_discrete(
    labels = likert_labeller
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  labs(
    x = "Reported severity of risk",
    y = "Reported impact of AA",
    fill = "% of responses\nreporting that level of severity",
    title = "Relationship between risk severity and AA impact",
    subtitle = "Looking across reported levels of severity"
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "risk_aa_norm.png"
  ),
  p_risk_aa_norm,
  height = 6,
  width = 6,
  units = "in"
)


# checking the reported AA impact all together, how frequently respondents answered
# the same level of severity for all risks
df_aa_rep <- df_risk %>%
  filter(
    !is.na(severity_current)
  ) %>%
  distinct(
    `_id`,
    risk,
    anticipatory_impact
  ) %>%
  group_by(
    `_id`
  ) %>%
  summarize(
    aa = paste(likert_labeller(anticipatory_impact), collapse = ", "),
    aa_sd = sd(as.numeric(anticipatory_impact)),
    aa_sd = ifelse(is.na(aa_sd), 0, aa_sd),
    num_responses = n(),
    num_unique = length(unique(anticipatory_impact))
  )

p_aa_tend <- df_aa_rep %>%
  count(
    num_responses,
    num_unique
  ) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = num_responses,
      y = num_unique
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    ),
    color = "white"
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("sapphire-hdx"),
    labels = scales::label_percent(1),
    breaks = scales::breaks_pretty(3)
  ) +
  geom_text_hdx(
    aes(
      label = scales::label_percent(1)(perc)
    ),
    color = "black"
  ) +
  labs(
    x = "# of risks reported",
    y = "# of unique AA impact values reported",
    fill = "% of\nresponses",
    title = "Tendency to report multiple impacts of AA"
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "aa_tendency.png"
  ),
  p_aa_tend,
  height = 5,
  width = 5,
  units = "in"
)

#######################
#### VULNERABILITY ####
#######################

df_vuln <- df_risk %>%
  pivot_longer(
    none:disabled_people
  ) %>%
  distinct(
    `_id`,
    risk,
    name,
    value
  ) %>%
  group_by(
    risk,
    name,
  ) %>%
  summarize(
    perc = mean(value, na.rm = TRUE)
  ) %>%
  mutate(
    none_flag = name == "none"
  )

vuln_lvls <- df_vuln %>%
  filter(
    name == "none"
  ) %>%
  arrange(
    perc
  ) %>%
  pull(
    risk
  )

vuln_group_lvls <- df_vuln %>%
  group_by(
    name
  ) %>%
  summarize(
    perc = mean(perc)
  ) %>%
  arrange(
    desc(perc)
  ) %>%
  pull(
    name
  )

p_vuln <- df_vuln %>%
  ggplot(
    aes(
      x = factor(name, vuln_group_lvls),
      y = factor(risk, vuln_lvls)
    )
  ) +
  geom_tile(
    aes(
      alpha = perc,
      fill = none_flag
    ),
    color = "white"
  ) +
  scale_x_discrete(
    labels = function(x) str_replace(likert_labeller(x), " ", "\n")
  ) +
  scale_y_discrete(
    labels = risk_labeller
  ) +
  scale_fill_manual(
    values = unname(hdx_hex(c("tomato-hdx", "mint-hdx"))),
    guide = "none"
  ) +
  geom_text_hdx(
    aes(
      label = scales::label_percent(1)(perc)
    ),
    color = "black"
  ) +
  scale_alpha_continuous(
    guide = "none"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(vjust = 0.5)
  ) +
  labs(
    x = "",
    y = "",
    title = "Most vulnerable groups by risk"
  ) +
  theme(
    axis.line.x = element_blank()
  )

ggsave(
  file.path(
    plot_dir,
    "vuln.png"
  ),
  p_vuln,
  height = 5,
  width = 10,
  units = "in"
)

#####################
#### SEASONALITY ####
#####################

df_seas <- df_risk %>%
  pivot_longer(
    q1:q4
  ) %>%
  distinct(
    `_id`,
    risk,
    name,
    value
  ) %>%
  group_by(
    risk,
    name,
  ) %>%
  summarize(
    perc = mean(value, na.rm = TRUE)
  )

seas_lvls <- df_seas %>%
  summarize(
    perc = sd(perc)
  ) %>%
  arrange(
    perc
  ) %>%
  pull(risk)

p_seas <- df_seas %>%
  ggplot(
    aes(
      x = name,
      y = factor(risk, seas_lvls)
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    ),
    color = "white"
  ) +
  scale_x_discrete(
    labels = toupper
  ) +
  scale_y_discrete(
    labels = risk_labeller
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("sapphire-hdx"),
    labels = scales::label_percent(1)
  ) +
  geom_text_hdx(
    aes(
      label = scales::label_percent(1)(perc)
    ),
    color = "black"
  ) +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "",
    y = "",
    title = "Seasonality of risk",
    fill = "% of\nresponses"
  ) +
  theme(
    axis.line.x = element_blank()
  ) +
  expand_limits(
    fill = 0
  )

ggsave(
  file.path(
    plot_dir,
    "seas.png"
  ),
  p_seas,
  height = 5,
  width = 7,
  units = "in"
)

#####################
#### CORRELATION ####
#####################

p_corr_bin <- df_risk %>%
  distinct(
    `_id`,
    risk,
    severity_current
  ) %>%
  pivot_wider(
    names_from = risk,
    values_from = severity_current
  ) %>%
  select(
    -`_id`
  ) %>%
  mutate(
    across(
      .fns = ~ as.numeric(is.na(.x))
    )
  ) %>%
  cor() %>%
  ggcorrplot(
    method = "circle"
  ) +
  labs(
    title = "Correlation of binary (risk reported or not)"
  ) +
  theme(
    plot.background = element_rect(
      fill = "white"
    )
  )

ggsave(
  file.path(
    plot_dir,
    "corr_bin.png"
  ),
  p_corr_bin,
  height = 6,
  width = 6,
  units = "in"
)


p_corr_comp <- df_risk %>%
  distinct(
    `_id`,
    risk,
    severity_current
  ) %>%
  pivot_wider(
    names_from = risk,
    values_from = severity_current
  ) %>%
  select(
    -`_id`
  ) %>%
  mutate(
    across(
      .fns = as.numeric
    )
  ) %>%
  cor(
    use = "pairwise.complete.obs"
  ) %>%
  ggcorrplot(
    method = "circle"
  ) +
  labs(
    title = "Correlation of severity when both risks reported"
  ) +
  theme(
    plot.background = element_rect(
      fill = "white"
    )
  )

ggsave(
  file.path(
    plot_dir,
    "corr_comp.png"
  ),
  p_corr_comp,
  height = 6,
  width = 6,
  units = "in"
)
