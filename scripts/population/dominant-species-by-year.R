###project: Dissertation - bioenergetics repository
###author(s): Mack White
###goal(s): Examine which species make up bulk of biomass annually (by hydrologic year)
###date(s): July 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, vegan, readxl, codyn, purrr)

dt <- read_csv("data/population/map_thru_042024_zerofilled.csv") |> 
      filter(site %in% c("RB9", "RB10", "RB11")) |> 
      select(hydrologic_year, month, site, bout, 
             common_name, latin_name, 
             total_length, weight_g) |> 
      na.omit()
glimpse(dt)

na_count_per_column <- sapply(dt, function(x) sum(is.na(x)))
print(na_count_per_column)

hydro_yr_players <- dt |> 
      group_by(hydrologic_year, common_name) |> 
      summarize(total_biomass = sum(weight_g, na.rm = TRUE)) |> 
      ungroup() |> 
      group_by(hydrologic_year)  |> 
      mutate(total_annual_biomass = sum(total_biomass))  |> 
      mutate(proportion = total_biomass / total_annual_biomass) |> 
      arrange(hydrologic_year, desc(total_biomass))  |> 
      slice_head(n = 5) |> 
      mutate(annual_all = sum(proportion)) |> 
      ungroup()

yearly_players <- hydro_yr_players |> 
      select(common_name) |> 
      distinct()

