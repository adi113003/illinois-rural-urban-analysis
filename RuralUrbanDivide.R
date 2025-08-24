#install.packages(c("tidycensus", "dplyr", "ggplot2", "sf", "tigris", "readr", "stringr", "plotly", "leaflet"))
library(tidycensus)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(readr)
library(stringr)
library(plotly)
library(leaflet)

census_api_key("e3f46bae4cc366b2b942db692283a95c58de2231", install = TRUE,overwrite=TRUE)


# 2020 population data
pop_2020 <- get_decennial(geography = "county", variables = "P1_001N", year = 2020, state = "IL", geometry = TRUE)

# 2000 population data
pop_2000 <- get_decennial(geography = "county", variables = "P001001", year = 2000, state = "IL", geometry = FALSE)


# Clean up 2020 data
pop_2020_clean <- pop_2020 %>%
  select(GEOID, NAME, pop_2020 = value)

# Clean up 2000 data
pop_2000_clean <- pop_2000 %>%
  select(GEOID, pop_2000 = value)

# Merge and calculate percent change
pop_change <- left_join(pop_2020_clean, pop_2000_clean, by = "GEOID") %>%
  mutate(
    pct_change = 100 * (pop_2020 - pop_2000) / pop_2000
  )


ggplot(pop_change) +
  geom_sf(aes(fill = pct_change), color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Percent Change in Population by County (2000–2020)",
       fill = "% Change") +
  theme_minimal()

library(readxl)
rucc <- read_excel("Ruralurbancontinuumcodes2023.xlsx") %>%
  filter(State == "IL") %>%
  mutate(GEOID = str_pad(as.character(FIPS), 5, pad = "0")) %>%
  select(GEOID, County_Name, RUCC_2023 = RUCC_2023)

# Join with pop_change
pop_rucc <- left_join(pop_change, rucc, by = "GEOID") %>%
  mutate(class = ifelse(RUCC_2023 <= 3, "Metro", "Nonmetro"))


ggplot(pop_rucc, aes(x = log(pop_2000), y = pct_change, color = class)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Population Change vs. County Size (2000–2020)",
       x = "Log(Population in 2000)", y = "% Change in Population",
       color = "County Type") +
  theme_minimal()


library(plotly)

plot_ly(pop_rucc,
        x = ~log(pop_2000),
        y = ~pct_change,
        type = 'scatter',
        mode = 'markers',
        color = ~class,
        colors = c("Metro" = "blue", "Nonmetro" = "red"),
        text = ~paste(NAME,
                      "<br>2000 Pop:", pop_2000,
                      "<br>2020 Pop:", pop_2020,
                      "<br>% Change:", round(pct_change, 1), "%"),
        hoverinfo = 'text') %>%
  layout(title = "Population Change vs. Size by County (Illinois)",
         xaxis = list(title = "Log(Population in 2000)"),
         yaxis = list(title = "% Change in Population (2000–2020)"))


library(leaflet)

pal <- colorNumeric(palette = "RdBu", domain = pop_rucc$pct_change)

leaflet(pop_rucc) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(pct_change),
              color = "white",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.8,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE),
              label = ~paste0(NAME, "<br>% Change: ", round(pct_change, 1), "%")) %>%
  addLegend(pal = pal, values = ~pct_change,
            title = "% Change (2000–2020)",
            position = "bottomright")

library(tidycensus)
library(dplyr)
library(ggplot2)
library(stringr)


# Load variable codes for ACS 5-year 2020
v2020 <- load_variables(2020, "acs5", cache = TRUE)

# Get age-sex population estimates (table B01001)
il_age_sex <- get_acs(
  geography = "county",
  state = "IL",
  table = "B01001",
  year = 2020,
  survey = "acs5"
)


# Get ACS age-sex data for Illinois
il_age_sex <- get_acs(
  geography = "county",
  state = "IL",
  table = "B01001",
  year = 2020,
  survey = "acs5"
)


# Create age-sex mapping
male_vars <- sprintf("B01001_%03d", 3:25)
female_vars <- sprintf("B01001_%03d", 27:49)

il_age_sex <- il_age_sex %>%
  mutate(
    sex = case_when(
      variable %in% male_vars ~ "Male",
      variable %in% female_vars ~ "Female",
      TRUE ~ NA_character_
    ),
    age_group = case_when(
      variable %in% c("B01001_003", "B01001_027") ~ "Under 5",
      variable %in% c("B01001_004", "B01001_028") ~ "5-9",
      variable %in% c("B01001_005", "B01001_029") ~ "10-14",
      variable %in% c("B01001_006", "B01001_030") ~ "15-17",
      variable %in% c("B01001_007", "B01001_031") ~ "18-19",
      variable %in% c("B01001_008", "B01001_032") ~ "20-24",
      variable %in% c("B01001_009", "B01001_033") ~ "25-29",
      variable %in% c("B01001_010", "B01001_034") ~ "30-34",
      variable %in% c("B01001_011", "B01001_035") ~ "35-39",
      variable %in% c("B01001_012", "B01001_036") ~ "40-44",
      variable %in% c("B01001_013", "B01001_037") ~ "45-49",
      variable %in% c("B01001_014", "B01001_038") ~ "50-54",
      variable %in% c("B01001_015", "B01001_039") ~ "55-59",
      variable %in% c("B01001_016", "B01001_040") ~ "60-64",
      variable %in% c("B01001_017", "B01001_041") ~ "65-66",
      variable %in% c("B01001_018", "B01001_042") ~ "67-69",
      variable %in% c("B01001_019", "B01001_043") ~ "70-74",
      variable %in% c("B01001_020", "B01001_044") ~ "75-79",
      variable %in% c("B01001_021", "B01001_045") ~ "80-84",
      variable %in% c("B01001_022", "B01001_046") ~ "85+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group) & !is.na(sex))

# Assume you already have RUCC loaded
il_age_sex <- left_join(il_age_sex, rucc, by = "GEOID") %>%
  mutate(class = ifelse(RUCC_2023 <= 3, "Metro", "Nonmetro"))


library(forcats)


metro_data <- il_age_sex %>%
  filter(class == "Metro") %>%
  group_by(age_group, sex) %>%
  summarise(estimate = sum(estimate)) %>%
  ungroup() %>%
  mutate(age_group = fct_rev(factor(age_group, levels = unique(age_group))),
         estimate = ifelse(sex == "Male", -estimate, estimate))

ggplot(metro_data, aes(x = estimate, y = age_group, fill = sex)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(labels = abs) +
  labs(title = "Population Pyramid: Metro Illinois Counties (2020)",
       x = "Population", y = "Age Group") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e"))


years <- c(2000, 2010, 2020)
var_codes <- c("2000" = "P001001", "2010" = "P001001", "2020" = "P1_001N")

library(purrr)

# Create a named vector mapping year to the correct variable code
var_codes <- c("2000" = "P001001", "2010" = "P001001", "2020" = "P1_001N")

# Loop correctly over years with variable access using [[ ]] instead of [ ]
pop_data <- map_df(c(2000, 2010, 2020), function(y) {
  get_decennial(
    geography = "county",
    state = "IL",
    variables = var_codes[[as.character(y)]],
    year = y,
    geometry = FALSE
  ) %>%
    mutate(year = y)
})


library(tidyr)

pop_wide <- pop_data %>%
  select(GEOID, NAME, year, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "pop_")



pop_ts <- pop_wide %>%
  mutate(
    change_2010 = 100 * (pop_2010 - pop_2000) / pop_2000,
    change_2020 = 100 * (pop_2020 - pop_2000) / pop_2000
  ) %>%
  select(GEOID, NAME, change_2010, change_2020)


pop_ts <- left_join(pop_ts, rucc, by = "GEOID") %>%
  mutate(class = ifelse(RUCC_2023 <= 3, "Metro", "Nonmetro"))

pop_long <- pop_ts %>%
  pivot_longer(cols = starts_with("change_"), names_to = "year", values_to = "pct_change") %>%
  mutate(year = str_remove(year, "change_") %>% as.numeric())


ggplot(pop_long, aes(x = year, y = pct_change, group = NAME, color = class)) +
  geom_line(alpha = 0.4) +
  labs(title = "Population % Change Since 2000 by County (IL)",
       x = "Year", y = "% Change Since 2000") +
  scale_color_manual(values = c("Metro" = "blue", "Nonmetro" = "red")) +
  theme_minimal()


# Median Household Income: B19013_001
income <- get_acs(
  geography = "county",
  state = "IL",
  variables = "B19013_001",
  year = 2020,
  survey = "acs5"
) %>%
  select(GEOID, NAME, income = estimate)

# Median Home Value: B25077_001
home_value <- get_acs(
  geography = "county",
  state = "IL",
  variables = "B25077_001",
  year = 2020,
  survey = "acs5"
) %>%
  select(GEOID, home_value = estimate)

afford <- left_join(income, home_value, by = "GEOID") %>%
  mutate(afford_ratio = home_value / income)


# Get geometry for map
afford_geo <- get_acs(
  geography = "county",
  state = "IL",
  variables = "B19013_001",
  year = 2020,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(GEOID, geometry) %>%
  left_join(afford, by = "GEOID")

ggplot(afford_geo) +
  geom_sf(aes(fill = afford_ratio), color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Housing Affordability by County in Illinois (2020)",
       fill = "Home Value / Income") +
  theme_minimal()

afford_plot_data <- left_join(pop_rucc, afford, by = "GEOID")

ggplot(afford_plot_data, aes(x = afford_ratio, y = pct_change, color = class)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Population Change vs. Housing Affordability",
    x = "Home Value / Income (Affordability Ratio)",
    y = "% Population Change (2000–2020)",
    color = "County Type"
  ) +
  theme_minimal()

library(leaflet)

pal <- colorNumeric("YlOrRd", afford_geo$afford_ratio, na.color = "gray")

leaflet(afford_geo) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(afford_ratio),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~paste0(NAME, "<br>Affordability Ratio: ", round(afford_ratio, 2)),
    highlightOptions = highlightOptions(color = "black", weight = 2)
  ) %>%
  addLegend(pal = pal, values = ~afford_ratio, title = "Affordability Ratio")

afford_bins <- c(0, 2.5, 3.0, 4.0, 5.0, Inf)
afford_labels <- c("Very Affordable", "Affordable", "Moderate", "Unaffordable", "Severely Unaffordable")

library(leaflet)

# Bin categories
afford_geo$afford_cat <- cut(
  afford_geo$afford_ratio,
  breaks = afford_bins,
  labels = afford_labels,
  include.lowest = TRUE
)

# Define color palette
pal_cat <- colorFactor(
  palette = c("green", "lightgreen", "gold", "orange", "red"),
  domain = afford_geo$afford_cat
)

# Plot
leaflet(afford_geo) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal_cat(afford_cat),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~paste0(NAME, "<br>", round(afford_ratio, 2), " (", afford_cat, ")"),
    highlightOptions = highlightOptions(color = "black", weight = 2)
  ) %>%
  addLegend(
    pal = pal_cat, values = ~afford_cat, title = "Affordability Category",
    position = "bottomright"
  )

library(ggplot2)
library(dplyr)

ggplot(pop_rucc, aes(x = factor(RUCC_2023), y = pct_change)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Population Change by RUCC Code (Illinois, 2000–2020)",
    x = "RUCC Code (1 = Metro, 9 = Most Rural)",
    y = "% Population Change"
  ) +
  theme_minimal(base_size = 14)
