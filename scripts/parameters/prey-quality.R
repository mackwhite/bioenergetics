###project: Dissertation - bioenergetics repository
###author(s): Mack White
###goal(s): using shark river prey stoichiometry data to generate parameter estimates
###date(s): July 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load standard libraries
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, readr, taxize, readxl)

dat <- readxl::read_excel("data/parameters/diet-nutrients/shark-river-prey-quality-master.xlsx")

dt <- dat |> 
      group_by(common_name, kingdom, phylum, class, order, family, genus, scientific_name) |> 
      summarize(Dc_m = mean(Dc_m, na.rm = TRUE),
                Dc_sd = mean(Dc_sd, na.rm = TRUE),
                Dn_m = mean(Dn_m, na.rm = TRUE),
                Dn_sd = mean(Dn_sd, na.rm = TRUE),
                Dp_m = mean(Dp_m, na.rm = TRUE),
                Dp_sd = mean(Dp_sd, na.rm = TRUE),
                Jg_m = mean(Jg_m, na.rm = TRUE),
                Jg_se = mean(Jg_se, na.rm = TRUE))

dt1 <- dt |> mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))
glimpse(dt1)

# NA-fill data to closest taxonomic unit ----------------------------------



