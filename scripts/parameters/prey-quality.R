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
                Jg_se = mean(Jg_se, na.rm = TRUE)) |> 
      ungroup()|> 
      mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) |> 
      mutate(class = if_else(class == "Actinopterygii", 
                             "Teleostei", 
                             class))

Dx_mixed_summary <- dt |> 
      summarize(Dc_m = mean(Dc_m, na.rm = TRUE),
                Dc_sd = mean(Dc_sd, na.rm = TRUE),
                Dn_m = mean(Dn_m, na.rm = TRUE),
                Dn_sd = mean(Dn_sd, na.rm = TRUE),
                Dp_m = mean(Dp_m, na.rm = TRUE),
                Dp_sd = mean(Dp_sd, na.rm = TRUE),
                Jg_m = mean(Jg_m, na.rm = TRUE),
                Jg_se = mean(Jg_se, na.rm = TRUE)) |> 
      ungroup() |> 
      mutate(Diet = "Mixed")

Dx_invert_summary <- dt |> 
      filter(class!= "Teleostei") |> 
      summarize(Dc_m = mean(Dc_m, na.rm = TRUE),
                Dc_sd = mean(Dc_sd, na.rm = TRUE),
                Dn_m = mean(Dn_m, na.rm = TRUE),
                Dn_sd = mean(Dn_sd, na.rm = TRUE),
                Dp_m = mean(Dp_m, na.rm = TRUE),
                Dp_sd = mean(Dp_sd, na.rm = TRUE),
                Jg_m = mean(Jg_m, na.rm = TRUE),
                Jg_se = mean(Jg_se, na.rm = TRUE)) |> 
      ungroup() |> 
      mutate(Diet = "Crustaceans")

Dx_lepomis_summary <- dt |> 
      filter(genus == "Lepomis") |> 
      summarize(Dc_m = mean(Dc_m, na.rm = TRUE),
                Dc_sd = mean(Dc_sd, na.rm = TRUE),
                Dn_m = mean(Dn_m, na.rm = TRUE),
                Dn_sd = mean(Dn_sd, na.rm = TRUE),
                Dp_m = mean(Dp_m, na.rm = TRUE),
                Dp_sd = mean(Dp_sd, na.rm = TRUE),
                Jg_m = mean(Jg_m, na.rm = TRUE),
                Jg_se = mean(Jg_se, na.rm = TRUE)) |> 
      ungroup() |> 
      mutate(Diet = "Lepomis spp.")

Dx_fish_summary <- dt |> 
      filter(class == "Teleostei") |> 
      summarize(Dc_m = mean(Dc_m, na.rm = TRUE),
                Dc_sd = mean(Dc_sd, na.rm = TRUE),
                Dn_m = mean(Dn_m, na.rm = TRUE),
                Dn_sd = mean(Dn_sd, na.rm = TRUE),
                Dp_m = mean(Dp_m, na.rm = TRUE),
                Dp_sd = mean(Dp_sd, na.rm = TRUE),
                Jg_m = mean(Jg_m, na.rm = TRUE),
                Jg_se = mean(Jg_se, na.rm = TRUE)) |> 
      ungroup() |> 
      mutate(Diet = "Fish")

Dx_diet_treatments <- rbind(Dx_mixed_summary, Dx_lepomis_summary, 
                            Dx_invert_summary, Dx_fish_summary) |> 
      select(Diet, everything())
write_csv(Dx_diet_treatments, "data/parameters/diet-nutrients/diet-treament-quality.csv")

Qx_summary <- dt |> 
      filter(order == "Perciformes") |> 
      summarize(Qc_m = mean(Dc_m, na.rm = TRUE),
                Qc_sd = mean(Dc_sd, na.rm = TRUE),
                Qn_m = mean(Dn_m, na.rm = TRUE),
                Qn_sd = mean(Dn_sd, na.rm = TRUE),
                Qp_m = mean(Dp_m, na.rm = TRUE),
                Qp_sd = mean(Dp_sd, na.rm = TRUE),
                Jg_m = mean(Jg_m, na.rm = TRUE),
                Jg_se = mean(Jg_se, na.rm = TRUE)) |> 
      ungroup()

write_csv(Qx_summary, "data/population/snook-quality-taxon-fill.csv")

# NA-fill data to closest taxonomic unit ----------------------------------

df <- dt

# Calculate averages at each taxonomic level
averages_genus <- df %>%
      group_by(genus) %>%
      summarize(across(c(Dc_m:Jg_se), ~mean(., na.rm = TRUE), .names = "genus_{.col}"))

averages_family <- df %>%
      group_by(family) %>%
      summarize(across(c(Dc_m:Jg_se), ~mean(., na.rm = TRUE), .names = "family_{.col}"))

averages_order <- df %>%
      group_by(order) %>%
      summarize(across(c(Dc_m:Jg_se), ~mean(., na.rm = TRUE), .names = "order_{.col}"))

averages_class <- df %>%
      group_by(class) %>%
      summarize(across(c(Dc_m:Jg_se), ~mean(., na.rm = TRUE), .names = "class_{.col}"))

averages_phylum <- df %>%
      group_by(phylum) %>%
      summarize(across(c(Dc_m:Jg_se), ~mean(., na.rm = TRUE), .names = "phylum_{.col}"))

# Left join these averages back to the main dataset
df <- df %>%
      left_join(averages_genus, by = "genus") %>%
      left_join(averages_family, by = "family") %>%
      left_join(averages_order, by = "order") %>%
      left_join(averages_class, by = "class") %>%
      left_join(averages_phylum, by = "phylum")

# Fill NAs using the coalesce function in order of specificity
df <- df %>%
      mutate(across(c(Dc_m:Jg_se), ~coalesce(.,
                                             get(paste0("genus_", cur_column())),
                                             get(paste0("family_", cur_column())),
                                             get(paste0("order_", cur_column())),
                                             get(paste0("class_", cur_column())),
                                             get(paste0("phylum_", cur_column()))),
                    .names = "{.col}_filled"))

df <- df %>%
      select(-matches("genus_|family_|order_|class_|phylum_"))

df1 <- df |> 
      select(-Dc_m, -Dc_sd, -Dn_m, -Dn_sd, -Dp_m, -Dp_sd, 
             -Jg_m, -Jg_se) |> 
      rename(Dc_m = Dc_m_filled,
             Dc_sd = Dc_sd_filled,
             Dn_m = Dn_m_filled,
             Dn_sd = Dn_sd_filled,
             Dp_m = Dp_m_filled,
             Dp_sd = Dp_sd_filled,
             Jg_m = Jg_m_filled,
             Jg_se = Jg_se_filled)

write_csv(df1, "data/parameters/diet-nutrients/prey-quality-taxon-fill-final.csv")

