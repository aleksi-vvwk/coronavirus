library(coronavirus)
update_dataset()

covid19_df <- refresh_coronavirus_jhu()
head(covid19_df)

# Summary of the total confrimed cases by country (top 20):

library(dplyr)

summary_df <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(20)

# Summary of new cases during the past 24 hours by country and type (as of 2021-09-17):

library(tidyr)

coronavirus %>%
  filter(date == max(date)) %>%
  select(country, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)


# Plot the confirmed cases distribution by counrty with treemap plot:

conf_df <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()

plot_ly(
  data = conf_df,
  type = "treemap",
  values = ~ total_cases,
  labels = ~ country,
  parents =  ~ parents,
  domain = list(column = 0),
  name = "Confirmed",
  textinfo = "label+value+percent parent"
)


# Plot the top 20 vaccinated countries:

covid19_vaccine %>%
  filter(date == max(date),!is.na(population)) %>%
  mutate(fully_vaccinated_ratio = people_fully_vaccinated / population) %>%
  arrange(-fully_vaccinated_ratio) %>%
  slice_head(n = 20) %>%
  arrange(fully_vaccinated_ratio) %>%
  mutate(country = factor(country_region, levels = country_region)) %>%
  plot_ly(
    y = ~ country,
    x = ~ round(100 * fully_vaccinated_ratio, 2),
    text = ~ paste(round(100 * fully_vaccinated_ratio, 1), "%"),
    textposition = 'auto',
    orientation = "h",
    type = "bar"
  ) %>%
  layout(
    title = "Percentage of Fully Vaccineted Population - Top 20 Countries",
    yaxis = list(title = ""),
    xaxis = list(title = "Source: Johns Hopkins Centers for Civic Impact",
                 ticksuffix = "%")
  )
