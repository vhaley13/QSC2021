library(tidycensus)
# library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)

blskey <- c("edbf2bffc7a74515b665705b10b12a2e")
usdakey <- c("97C0CDC3-8A8C-3010-A750-6EF9BB912D7B")
hudkey <- c("eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6IjZhYmRkOWE5N2I0ZTBlYWY3ZTM1MDJhOWFjMjMxNWRiN2VjYWQyYTdjMjMxYTUzZTg4MmJlNDdjMDU5YTRjOTExYWZmYzBhOWY4NThmOTI3In0.eyJhdWQiOiI2IiwianRpIjoiNmFiZGQ5YTk3YjRlMGVhZjdlMzUwMmE5YWMyMzE1ZGI3ZWNhZDJhN2MyMzFhNTNlODgyYmU0N2MwNTlhNGM5MTFhZmZjMGE5Zjg1OGY5MjciLCJpYXQiOjE2MzUzNzcwOTYsIm5iZiI6MTYzNTM3NzA5NiwiZXhwIjoxOTUwOTA5ODk2LCJzdWIiOiIyNjEzMiIsInNjb3BlcyI6W119.FDne7ezzJXl-iS3lRgAuAzGDAgAFwb_WxABSXLwuA1sxq_60VW0Gpi6Phay5FuJisRewv3AYZ4OXp2_9MnloMQ
")

key <- c("ddd0c5f47ae1fa167dbcbc7804b46ef9e76d45e2")

census_api_key(key, install = TRUE, overwrite = TRUE) 

## variable search

v17 <- load_variables(2017, "acs5", cache = TRUE)

View(v17)

#@ acs

vt <- tidycensus::get_acs(geography = "county", 
                          variables = c(medincome = "B19013_001"), 
                          state = "VT", 
                          year = 2018)

vt

# simple plot

vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2014-2018 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

## spatial

# basic map

orange <- get_acs(state = "CA", county = "Orange", geography = "tract", 
                  variables = "B19013_001", geometry = TRUE)

head(orange)

orange %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") 

# facet map

racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")

harris <- get_decennial(geography = "tract", variables = racevars, 
                        state = "TX", county = "Harris County", geometry = TRUE,
                        summary_var = "P001001") 

head(harris)

harris %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = pct)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + # Appropriate CRS for Houston, but may not be for your data
  scale_fill_viridis_c()

# detailed map

ny <- get_acs(geography = "tract", 
              variables = "B19013_001", 
              state = "NY", 
              county = "New York", 
              geometry = TRUE)

ggplot(ny, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar)

library(sf)

ny2 <- get_acs(geography = "tract", 
               variables = "B19013_001", 
               state = "NY", 
               county = "New York", 
               geometry = TRUE, 
               cb = FALSE)

library(tigris)

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

ny_water <- area_water("NY", "New York", class = "sf") 

ny_erase <- st_erase(ny2, ny_water)

ggplot(ny_erase, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar)

library(sf)
st_write(orange, "orange.shp")


## margins of error

vars <- paste0("B01001_0", c(20:25, 44:49))

ramsey <- get_acs(geography = "tract", 
                  variables = vars, 
                  state = "MN", 
                  county = "Ramsey", 
                  year = 2016)

head(ramsey %>% select(-NAME))

ramsey65 <- ramsey %>%
  group_by(GEOID) %>%
  summarize(sumest = sum(estimate), 
            summoe = moe_sum(moe, estimate))

head(ramsey65)


## microdata

install.packages(c("survey", "srvyr"))

pums_vars_2018 <- pums_variables %>% 
  filter(year == 2018, survey == "acs1")

pums_vars_2018 %>% 
  distinct(var_code, var_label, data_type, level)

pums_vars_2018 %>% 
  distinct(var_code, var_label, data_type, level) %>% 
  filter(level == "person")

# pums data download

vt_pums <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "SCHL"),
  state = "VT",
  survey = "acs1",
  year = 2018
)

vt_pums

# recoded

vt_pums_recoded <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "SCHL"),
  state = "VT",
  survey = "acs1",
  year = 2018,
  recode = TRUE
)

vt_pums_recoded

# analyzing pums

sum(vt_pums_recoded$PWGTP)

vt_pums_recoded %>% 
  count(PUMA, SEX_label, wt = PWGTP)

vt_pums_recoded %>% 
  mutate(ba_above = SCHL %in% c("21", "22", "23", "24")) %>% 
  group_by(PUMA, SEX_label) %>% 
  summarize(
    total_pop = sum(PWGTP),
    mean_age = weighted.mean(AGEP, PWGTP),
    ba_above = sum(PWGTP[ba_above == TRUE & AGEP >= 25]),
    ba_above_pct = ba_above / sum(PWGTP[AGEP >= 25])
  )

vt_pums_rep_weights <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "SCHL"),
  state = "VT",
  survey = "acs1",
  year = 2018,
  recode = TRUE,
  rep_weights = "person"
)

vt_survey_design <- to_survey(vt_pums_rep_weights)

library(srvyr, warn.conflicts = FALSE)

vt_survey_design %>% 
  survey_count(PUMA, SEX_label)

survey::svyby(~SEX_label, ~PUMA, design = vt_survey_design, survey::svytotal)

vt_survey_design %>% 
  mutate(ba_above = SCHL %in% c("21", "22", "23", "24")) %>% 
  filter(AGEP >= 25) %>% 
  group_by(PUMA, SEX_label) %>% 
  summarize(
    age_25_up = survey_total(vartype = "ci"),
    ba_above_n = survey_total(ba_above, vartype = "ci"),
    ba_above_pct = survey_mean(ba_above, vartype = "ci")
  )

# modeling with PUMS

vt_pums_to_model <- get_pums(
  variables = c("PUMA", "WAGP", "JWMNP", "JWTR", "COW", "ESR"),
  state = "VT",
  survey = "acs5",
  year = 2018,
  rep_weights = "person"
)

vt_model_sd <- vt_pums_to_model %>% 
  filter(
    ESR == 1,   # civilian employed
    JWTR != 11, # does not work at home
    WAGP > 0,   # earned wages last year
    JWMNP > 0   # commute more than zero min
  ) %>%
  mutate(
    emp_type = case_when(
      COW %in% c("1", "2")      ~ "private",
      COW %in% c("3", "4", "5") ~ "public",
      TRUE                      ~ "self"
    )
  ) %>%
  to_survey()

vt_model_sd %>% 
  summarize(
    n              = survey_total(1),
    mean_wage      = survey_mean(WAGP),
    median_wage    = survey_median(WAGP),
    mean_commute   = survey_mean(JWMNP),
    median_commute = survey_median(JWMNP)
  )

vt_model_sd %>% 
  survey_count(emp_type)

model <- survey::svyglm(log(JWMNP) ~ log(WAGP) + emp_type + PUMA, design = vt_model_sd)
summary(model)

# mapping PUMS

ne_states <- c("VT", "NH", "ME", "MA", "CT", "RI")
ne_pumas <- map(ne_states, tigris::pumas, class = "sf", cb = TRUE) %>% 
  reduce(rbind)

ne_pums <- get_pums(
  variables = c("PUMA", "POVPIP"),
  state = ne_states,
  survey = "acs1",
  year = 2018
)

ne_pov <- ne_pums %>%
  group_by(ST, PUMA) %>%
  summarize(
    total_pop = sum(PWGTP),
    pct_in_pov = sum(PWGTP[POVPIP < 200]) / total_pop
  )

ne_pumas %>%
  left_join(ne_pov, by = c("STATEFP10" = "ST", "PUMACE10" = "PUMA")) %>%
  ggplot(aes(fill = pct_in_pov)) +
  geom_sf() +
  scale_fill_viridis_b(
    name = NULL,
    option = "magma",
    labels = scales::label_percent(1)
  ) +
  labs(title = "Percentage of population below 200% of the poverty line") +
  theme_void()


# verification of PUMS estimates

wy_relp <- get_pums(
  variables = "RELP",
  state = "Wyoming",
  survey = "acs1",
  year = 2018,
  rep_weights = "person"
)

ut_ten <- get_pums(
  variables = "TEN",
  state = "Utah",
  survey = "acs1",
  year = 2018,
  rep_weights = "housing"
)

hi_age <- get_pums(
  variables = "AGEP",
  state = "Hawaii",
  survey = "acs1",
  year = 2018,
  rep_weights = "person"
)

wy_relp %>% 
  to_survey() %>% 
  survey_count(RELP) %>% 
  filter(RELP == "16")

ut_ten %>% 
  distinct(SERIALNO, .keep_all = TRUE) %>%
  to_survey(type = "housing") %>% 
  survey_count(TEN) %>% 
  filter(TEN == 2)

hi_age %>% 
  filter(between(AGEP, 0, 4)) %>% 
  to_survey() %>% 
  summarize(age_0_4 = survey_total(1))



#### population change

us_components <- get_estimates(geography = "state", product = "components")

us_components

unique(us_components$variable)

net_migration <- get_estimates(geography = "county",
                               variables = "RNETMIG",
                               year = 2019,
                               geometry = TRUE,
                               resolution = "20m") %>%
  shift_geometry()

net_migration

order = c("-15 and below", "-15 to -5", "-5 to +5", "+5 to +15", "+15 and up")

net_migration <- net_migration %>%
  mutate(groups = case_when(
    value > 15 ~ "+15 and up",
    value > 5 ~ "+5 to +15",
    value > -5 ~ "-5 to +5",
    value > -15 ~ "-15 to -5",
    TRUE ~ "-15 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order))

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

ggplot() +
  geom_sf(data = net_migration, aes(fill = groups, color = groups), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE) +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto") +
  labs(title = "Net migration per 1000 residents by county",
       subtitle = "US Census Bureau 2019 Population Estimates",
       fill = "Rate",
       caption = "Data acquired with the R tidycensus package | @kyle_e_walker")

la_age_hisp <- get_estimates(geography = "county", 
                             product = "characteristics", 
                             breakdown = c("SEX", "AGEGROUP", "HISP"),  
                             breakdown_labels = TRUE, 
                             state = "CA", 
                             county = "Los Angeles")

la_age_hisp

compare <- filter(la_age_hisp, str_detect(AGEGROUP, "^Age"), 
                  HISP != "Both Hispanic Origins", 
                  SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

ggplot(compare, aes(x = AGEGROUP, y = value, fill = SEX)) + 
  geom_bar(stat = "identity", width = 1) + 
  theme_minimal(base_family = "Roboto") + 
  scale_y_continuous(labels = function(y) paste0(abs(y / 1000), "k")) + 
  scale_x_discrete(labels = function(x) gsub("Age | years", "", x)) + 
  scale_fill_manual(values = c("darkred", "navy")) + 
  coord_flip() + 
  facet_wrap(~HISP) + 
  labs(x = "", 
       y = "2019 Census Bureau population estimate", 
       title = "Population structure by Hispanic origin", 
       subtitle = "Los Angeles County, California", 
       fill = "", 
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")


# migration flows
wch_flows <- get_flows(
  geography = "county",
  state = "NY",
  county = "Westchester",
  year = 2018
)


wch_flows %>% 
  filter(variable == "MOVEDOUT") %>% 
  arrange(desc(estimate)) %>% 
  head()

wch_flows %>% 
  filter(variable == "MOVEDNET") %>% 
  arrange(estimate) %>% 
  head()

wch_flows %>% 
  filter(is.na(GEOID2)) %>% 
  head()

la_flows <- get_flows(
  geography = "metropolitan statistical area",
  breakdown = "RACE",
  breakdown_labels = TRUE,
  msa = 31080,   # los angeles msa fips code
  year = 2015
)

# net migration between la and san francisco
la_flows %>% 
  filter(str_detect(FULL2_NAME, "San Fran"), variable == "MOVEDNET")


phx_flows <- get_flows(
  geography = "metropolitan statistical area",
  msa = 38060,
  year = 2018,
  geometry = TRUE
)

phx_flows %>% 
  head()

library(mapdeck)

top_move_in <- phx_flows %>% 
  filter(!is.na(GEOID2), variable == "MOVEDIN") %>% 
  slice_max(n = 25, order_by = estimate) %>% 
  mutate(
    width = estimate / 500,
    tooltip = paste0(
      scales::comma(estimate * 5, 1),
      " people moved from ", str_remove(FULL2_NAME, "Metro Area"),
      " to ", str_remove(FULL1_NAME, "Metro Area"), " between 2014 and 2018"
    )
  )

top_move_in %>% 
  mapdeck(style = mapdeck_style("dark"), pitch = 45) %>% 
  add_arc(
    origin = "centroid1",
    destination = "centroid2",
    stroke_width = "width",
    auto_highlight = TRUE,
    highlight_colour = "#8c43facc",
    tooltip = "tooltip"
  )


