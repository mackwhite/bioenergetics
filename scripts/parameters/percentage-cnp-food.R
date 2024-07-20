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
dat <- read_csv("data/parameters/diet-nutrients/shark-river-prey-nutrients.csv") |> 
      filter(!is.na(CommonName)) |> 
      ### filtering weird outliers
      filter(C_percent >= 35) |> 
      filter(N_percent >= 8) |> 
      filter(P_percent >= 0.5)

glimpse(dat)

# visualize differences amongst taxa --------------------------------------

### Carbon Percent
dat |> 
      ggplot(aes(CommonName, C_percent)) +
      geom_boxplot()

### Nitrogen Percent
dat |> 
      ggplot(aes(CommonName, N_percent)) +
      geom_boxplot()

### Phosphorus Percent
dat |> 
      ggplot(aes(CommonName, P_percent)) +
      geom_boxplot()

summary_dat <- dat |> 
      summarize(Dc_m = mean(C_percent),
                Dc_sd = sd(C_percent),
                Dn_m = mean(N_percent),
                Dn_sd = sd(N_percent),
                Dp_m = mean(P_percent),
                Dp_sd = sd(P_percent))

bg_test <- dat |> 
      filter(CommonName == "Bluegill") |> 
      mutate(cn = C_mol/N_mol,
             np = N_mol/P_mol,
             cp = C_mol/P_mol) |> 
      summarize(meancn = mean(cn),
                meannp = mean(np),
                meancp = mean(cp))
