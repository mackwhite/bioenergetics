###project: Dissertation - bioenergetics repository
###author(s): Mack White
###goal(s): using shark river prey stoichiometry data to generate parameter estimates
###date(s): July 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load standard libraries
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, readr, fishflux)

### read in data
dat <- read_csv("data/population/map_thru_042024_zerofilled.csv") |> 
      filter(common_name == "Snook")

glimpse(dat)