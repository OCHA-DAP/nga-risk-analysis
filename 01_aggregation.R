library(tidyverse)
library(readxl)

######################
#### DATA LOADING ####
######################

nga_dir <- Sys.getenv("NGA_HNO_DIR")
data_dir <- file.path(nga_dir, "data")
tool_dir <- file.path(nga_dir, "tool")

# survey data
df <- read_excel(
  file.path(
    data_dir,
    "DRAFT_2023_Risk_Analysis_Dataset_1902022_Shared_Final.xlsx"
  ),
  guess_max = 10000
)

# tool choices (for matching up to admin names)
tool <- read_excel(
  file.path(
    nga_dir,
    "tool",
    "FORM_2022_Risk_Analysis_Data_Collection_Tool_V4.xlsx"
  ),
  sheet = "choices"
)

####################################
#### CLEANING DATA FOR ANALYSIS ####
####################################

# extracting column names from last slash in full data frame name
names(df) <- str_extract(
  names(df),
  "([^\\/]+$)"
)

# get long format data where each row is a risk and LGA

patterns <- paste(
  "severity_current",
  "severity_future",
  "anticipatory_yn",
  "anticipatory_impact",
  "seasonality",
  "capacity",
  "vulnerability",
  sep = "|"
)

df_long <- df %>%
  select(-risk) %>%
  pivot_longer(
    cols = c(
      matches("^severity|^capacity|^anticipatory|^seasonality|^vulnerability")
    ),
    names_to = c("variable", "risk"),
    names_pattern = paste0("(", patterns, ")_(.*)")
  ) %>%
  select(
    `_id`,
    state,
    lga,
    variable,
    risk,
    value,
    constraints,
    constraints_cause
  ) %>%
  pivot_wider(
    id_cols = c(`_id`, state, lga, risk, constraints, constraints_cause),
    names_from = "variable",
    values_from = "value"
  ) %>%
  separate(
    col = lga,
    into = paste0("lga", 1:10),
    sep = " "
  ) %>%
  pivot_longer(
    cols = lga1:lga10,
    names_to = NULL,
    values_to = "lga"
  ) %>%
  relocate(
    lga,
    .after = state
  ) %>%
  filter(
    !is.na(lga)
  )

# convert lga to names that are present in the adm2 data for joining
# and get unique state for each lga
df_lga <- tool %>%
  filter(
    list_name == "lga_c"
  ) %>%
  transmute(
    lga = name,
    lga_name = label,
    state = factor(
      tools::toTitleCase(state),
      levels = rev(c("Adamawa", "Borno", "Yobe"))
    )
  )

df_named <- df_long %>%
  select(
    -state
  ) %>%
  left_join(
    df_lga,
    by = "lga"
  ) %>%
  mutate(
    lga = lga_name
  ) %>%
  select(
    -lga_name
  ) %>%
  relocate(
    state,
    .before = lga
  )

# create factors from data and seasonal analysis
df_fctrs <- df_named %>%
  mutate(
    across(
      .cols = c(constraints, severity_current:capacity),
      .fns = ~factor(
        x = .x,
        levels = c("very_low", "low", "medium", "high", "very_high"),
        ordered = TRUE
      )
    ),
    anticipatory_impact = factor(
      anticipatory_impact,
      levels = c("none", "limited", "some", "significant", "very_significant"),
      ordered = TRUE
    )
  )

# get datasets with unique number of rows for risks and constraints
df_risk_draft <- df_fctrs %>%
  select(
    -starts_with("constraints")
  )

df_constraints_draft <- df_fctrs %>%
  distinct(`_id`, state, lga, constraints, constraints_cause) %>%
  select(-`_id`)

# create additional binary columns for seasonality and vulnerability

df_season <- map_dfc(
  .x = c(paste0("q", 1:4)),
  .f = ~ df_risk_draft %>%
    transmute(
      !!.x := str_detect(seasonality, .x)
    )
)

df_vulnerability <- map_dfc(
  .x = c("none", "refugees", "host_community", "men", "idps", "returnees",
         "women", "boys", "girls", "disabled_people"),
  .f = ~ df_risk_draft %>%
    transmute(
      !!.x := str_detect(vulnerability, paste0("\\b", .x, "\\b"))
    )
)

df_risk <- bind_cols(
  df_risk_draft,
  df_season,
  df_vulnerability
) %>%
  relocate(
    vulnerability,
    .after = q4
  )

# create binary columns for constraints

df_constraints_cause <- map_dfc(
  .x = c("movement_restriction", "access_restrictions", "cost_increases",
         "humanitarian_footprint_reduction", "loss_road_access", "forced_movement",
         "supplies_inaccessible", "other", "scale_down", "asset_destruction",
         "needs_denial"),
  .f = ~ df_constraints_draft %>%
    transmute(
      !!.x := str_detect(constraints_cause, .x)
    )
)

df_constraints <- bind_cols(
  df_constraints_draft,
  df_constraints_cause
)

#######################
#### SAVE OUT DATA ####
#######################

write_csv(
  df_constraints,
  file.path(
    data_dir,
    "constraints.csv"
  )
)

write_csv(
  df_risk,
  file.path(
    data_dir,
    "risks.csv"
  )
)
