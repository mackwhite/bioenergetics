###author(s): Mack White, Li Kui, Angel Chen
###goal(s): calculate summary stats needed for analysis/figures
###date(s): January - March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, writexl)

old <- read_csv("data/map_years1thru17.csv") |> 
      janitor::clean_names() |> 
      select(hydroyear, date, year, speciescode, pittag, tl, sl, weight, drainage, site, temp_c,
             salinity, stomachcontent) |> 
      unite(site, drainage, site, sep = "-") |> 
      filter(speciescode %in% c(20, 50)) |> 
      filter(!is.na(stomachcontent)) |> 
      filter(site %in% c("RB-8", "RB-9", "RB-10", "RB-11", "RB-12", "RB-13")) #81 samples outside of HW sites


year19 <- read_csv("data/map_year19.csv") |> 
      janitor::clean_names() |> 
      select(hydroyear, date, year, speciescode, pittag, tl, sl, weight, drainage, site, temp_c,
             salinity, stomachcontent) |> 
      unite(site, drainage, site, sep = "-") |> 
      filter(speciescode %in% c(20, 50)) |> 
      filter(!is.na(stomachcontent)) |> 
      filter(site %in% c("RB-8", "RB-9", "RB-10", "RB-11", "RB-12", "RB-13"))


year20 <- read_csv("data/map_year20.csv") |> 
      janitor::clean_names() |> 
      select(hydroyear, date, year, speciescode, pittag, tl, sl, weight, drainage, site, temp_c,
             salinity, stomachcontent) |> 
      unite(site, drainage, site, sep = "-") |> 
      filter(speciescode %in% c(20, 50)) |> 
      filter(!is.na(stomachcontent)) |> 
      filter(site %in% c("RB-8", "RB-9", "RB-10", "RB-11", "RB-12", "RB-13"))

all <- rbind(old, year19, year20)
# writexl::write_xlsx(all, "data/all_diets_thruMarch2024.xlxs")

all_bass <- all |> 
      filter(speciescode == 20)

all_snook <- all |> 
      filter(speciescode == 50)

annual_sample_size <- all |> 
      group_by(hydroyear, speciescode) |> 
      summarize(n = n()) |> 
      arrange(speciescode, hydroyear)

