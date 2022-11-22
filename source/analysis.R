library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Load data frame ----

df <- get_data()

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num = 6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ----
#----------------------------------------------------------------------------#
# Variables calculated from the data
#----------------------------------------------------------------------------#

# Average growth of the total jail population rate (1970-2018)
avg_rate_growth <- df %>%
  mutate(state_county = paste0(state, ", ", county_name)) %>%
  select(year, state_county, total_jail_pop_rate) %>%
  group_by(state_county) %>%
  filter(year == max(year) | year == min(year)) %>%
  spread(key = year, value = total_jail_pop_rate) %>%
  ungroup() %>%
  summarize(rate_diff = `2018` - `1970`) %>%
  summarize(avg_rate_growth = mean(rate_diff, na.rm = T)) %>%
  pull(avg_rate_growth)

# Average total jail population rate (most recent)
avg_rate <- df %>%
  filter(year == max(year)) %>%
  summarize(avg_rate = mean(total_jail_pop_rate, na.rm = T)) %>%
  pull(avg_rate)

# Rural region average total jail population rate (most recent)
avg_rate_rural <- df %>%
  filter(urbanicity == "rural", year == max(year)) %>%
  mutate(state_county = paste0(state, ", ", county_name)) %>%
  summarize(avg_rate_rural = mean(total_jail_pop_rate, na.rm = T)) %>%
  pull(avg_rate_rural)

# Average black people jail population rate (most recent)
avg_rate_black <- df %>%
  filter(year == max(year)) %>%
  summarize(avg_rate_black = mean(black_jail_pop_rate, na.rm = T)) %>%
  pull(avg_rate_black)


## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Including the necessary functions to plot the graph of yearly growth of total jail population
#----------------------------------------------------------------------------#

# This function cleans up the dataset to filter out only the total jail population in each year.
get_year_jail_pop <- function() {
  year_jail_pop <- df %>%
    group_by(year) %>%
    summarize(year_total_jail_pop = sum(total_jail_pop, na.rm = T))

  return(year_jail_pop)
}

# This function plots the chart of total jail population in the U.S. changing yearly.
plot_jail_pop_for_us <- function() {
  title <- "Increase of Jail Population in U.S. (1970-2018)"
  caption <- "Data from Incarceration Trends Dataset by Vera Institute of Justice"
  legend_x <- "Year"
  legend_y <- "Total Jail Population"

  plot <- get_year_jail_pop() %>%
    ggplot() +
    geom_col(mapping = aes(x = year, y = year_total_jail_pop)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = legend_x,
      y = legend_y,
      title = title,
      caption = caption
    )

  return(plot)
}

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# Including the necessary functions to plot the graph of yearly growth of total jail population in one or more states
#----------------------------------------------------------------------------#

# This function filter out the data for states given.
get_jail_pop_by_states <- function(states) {
  jail_pop_by_states <- df %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarize(state_total_jail_pop = sum(total_jail_pop, na.rm = T))

  return(jail_pop_by_states)
}

# This function plots the chart for yearly change in jail population in one or more states.
plot_jail_pop_by_states <- function(states) {
  title <- "Increase of Jail Population in U.S. States (1970-2018)"
  caption <- "Data from Incarceration Trends Dataset by Vera Institute of Justice"
  legend_x <- "Year"
  legend_y <- "Total Jail Population"

  plot <- get_jail_pop_by_states(states) %>%
    ggplot() +
    geom_line(mapping = aes(x = year, y = state_total_jail_pop, group = state, color = state)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = legend_x,
      y = legend_y,
      title = title,
      caption = caption
    )

  return(plot)
}

## Section 5  ----
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Proportion of male jail population versus county population
#----------------------------------------------------------------------------#

# This function calculates the proportion of male jail population, and filter out the total population (under 500000) (most recent)
get_prop_male_jail_pop_vs_county_pop <- function() {
  prop_male_jail_pop_vs_county_pop <- df %>%
    filter(year == max(year)) %>%
    mutate(prop_male_jail_pop = male_jail_pop / total_jail_pop) %>%
    filter(prop_male_jail_pop <= 1.0) %>%
    filter(total_pop < 500000) %>%
    select(total_pop, prop_male_jail_pop)

  return(prop_male_jail_pop_vs_county_pop)
}

# This function plots the chart of male jail population and total population (under 500000) correlation
plot_prop_male_jail_pop_vs_county_pop <- function() {
  title <- "Male Jail Population Versus County Population"
  caption <- "Data from Incarceration Trends Dataset by Vera Institute of Justice, populations over 500000 are omitted"
  legend_x <- "County Population"
  legend_y <- "Proportion of Male Jail Population"

  plot <- get_prop_male_jail_pop_vs_county_pop() %>%
    ggplot() +
    geom_point(mapping = aes(x = total_pop, y = prop_male_jail_pop)) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      x = legend_x,
      y = legend_y,
      title = title,
      caption = caption
    )

  return(plot)
}

## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Proportion of black people jail population across Washington state
#----------------------------------------------------------------------------#

# This function calculates the proportion of black people jail population in WA, and get the coordinates of the location
get_prop_black_jail_pop_wa <- function() {
  counties <- map_data("county") %>%
    mutate(polyname = paste0(region, ",", subregion)) %>%
    select(long, lat, polyname)
  
  prop_black_jail_pop_wa <- df %>%
    filter(year == max(year), state == "WA") %>%
    mutate(prop_black_jail_pop_wa = black_jail_pop / total_jail_pop) %>%
    filter(prop_black_jail_pop_wa <= 1.0) %>%
    left_join(county.fips, by = "fips") %>%
    left_join(counties, by = "polyname") %>%
    select(county_name, long, lat, prop_black_jail_pop_wa)

  return(prop_black_jail_pop_wa)
}

# This function plots the proportion of black people jail population in WA counties
plot_map_prop_black_jail_pop_wa <- function() {
  title <- "Proportion of Black People Jail Population in Washington State (2018)"
  caption <- "Data from Incarceration Trends Dataset by Vera Institute of Justice"
  legend_x <- "Longitude"
  legend_y <- "Latitude"


  plot <- get_prop_black_jail_pop_wa() %>%
    ggplot() +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = county_name, fill = prop_black_jail_pop_wa),
      color = "white",
      linewidth = .2
    ) +
    coord_map() + # use a map-based coordinate system
    scale_fill_continuous(low = "white", high = "Red") +
    labs(
      fill = "Proportion of Black People Jail Population",
      title = title,
      caption = caption,
      x = legend_x,
      y = legend_y,
    ) +
    theme_minimal()

  return(plot)
}
