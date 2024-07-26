###project: Dissertation - bioenergetics repository
###author(s): Mack White
###goal(s): using fishflux to seek out parameter inputs for snook
###date(s): July 2024
###note(s): 
### package vignette - https://nschiett.github.io/fishflux/index.html

# Housekeeping ------------------------------------------------------------

### load standard libraries
# install.packages("librarian")
# install.packages("devtools")
# devtools::install_github("nschiett/fishflux", dependencies=TRUE)
# library(fishflux)
librarian::shelf(tidyverse, readxl, readr, fishflux)
data("param_zebsco")

# identify metabolic parameters for perciformes ---------------------------

perciformes_metabolic <- metabolic_parameters |> 
      filter(family %in% c("Apogonidae", "Centrarchidae", "Cichlidae", "Gobiidae",
                           "Pomacentridae", "Sciaenidae")) |> 
      mutate(order = "Perciformes") |> 
      group_by(order) |> 
      summarize(a_m_avg = mean(a_m),
                a_sd_avg = mean(a_sd),
                b0_m_avg = mean(b0_m),
                b0_sd_avg = mean(b0_sd))

snook_metabolic_params <- data.frame(
      ### very close to what is reported by Glenncross & Felsing (2006) - barramundi
      species = "Common Snook",
      alpha_m = 0.6993033,
      alpha_sd = 0.1095661,
      b0_m = 0.0007550052,
      b0_sd = 0.0003645753
)


# pull metabolic normalization constant at varied TP ----------------------

metabolism("Centropomidae", temp = 24.1825, troph_m = 4.0)
metabolism("Centropomidae", temp = 24.1825, troph_m = 3.5)
metabolism("Centropomidae", temp = 24.1825, troph_m = 3.0)

snook_f0_normalization_constant <- data.frame(
      diet = c("Lepomis", "Mixed", "Crustacean"),
      h_m = c(4, 3.5, 3.0),
      f0_m = c(0.002130072, 0.001911705, 0.001687307),
      f0_sd = c(4.275895e-10, 3.898963e-10, 3.460819e-10)
)

# modeling where consuming mix of everything ------------------------------

mod_all <- cnp_model_mcmc(TL = 25:75, param = list(
      Qc_m = 43.54966, Qc_sd = 2.540621, Qn_m = 10.75658, Qn_sd = 0.747464, Qp_m = 2.680854, Qp_sd = 0.4940187,
      Dc_m = 43.50965,	Dc_sd = 2.817301, Dn_m = 10.50397, Dn_sd	= 0.8348159, Dp_m = 2.477653, Dp_sd = 0.4584972,
      ac_m = 0.8, an_m = 0.8, ap_m = 0.7,
      linf_m = 101.1079, linf_sd = 4.998803, k_m = 0.175, k_sd = 0.015500, t0_m = -1.352, t0_sd = 0.1714,
      lwa_m = 0.009504, lwb_m = 3.078241,
      mdw_m = 0.258806,
      f0_m = 0.001911705, f0_sd = 3.898963e-10,
      alpha_m = 0.6993033, alpha_sd = 0.1095661, theta_m = 2.5, theta_sd = 0.5,
      r_m = 1.906930, h_m = 3.5, v_m = 24.1825, v_sd = 3.449313 
      ))

df_all <- data.frame(mod_all$summary) |> 
      mutate(Diet = "Mixed") |> 
      filter(variable %in% c("Fc", "Fn", "Fp",
                             "Ic", "In", "Ip"))

# modeling where consuming lepomis prey ------------------------------

mod_lepomis <- cnp_model_mcmc(TL = 25:75, param = list(
      Qc_m = 43.54966, Qc_sd = 2.540621, Qn_m = 10.75658, Qn_sd = 0.747464, Qp_m = 2.680854, Qp_sd = 0.4940187,
      Dc_m = 43.99959,	Dc_sd = 3.053239, Dn_m = 11.01415, Dn_sd	= 0.9857259, Dp_m = 2.68101, Dp_sd = 0.6020884,
      ac_m = 0.8, an_m = 0.8, ap_m = 0.7,
      linf_m = 101.1079, linf_sd = 4.998803, k_m = 0.175, k_sd = 0.015500, t0_m = -1.352, t0_sd = 0.1714,
      lwa_m = 0.009504, lwb_m = 3.078241,
      mdw_m = 0.258806,
      f0_m = 0.002130072, f0_sd = 4.275895e-10,
      alpha_m = 0.6993033, alpha_sd = 0.1095661, theta_m = 2.5, theta_sd = 0.5,
      r_m = 1.906930, h_m = 4.0, v_m = 24.1825, v_sd = 3.449313 
))

df_lepomis <- data.frame(mod_lepomis$summary) |> 
      mutate(Diet = "Lepomis spp.") |> 
      filter(variable %in% c("Fc", "Fn", "Fp",
                             "Ic", "In", "Ip"))

# modeling where consuming invertebrate prey ------------------------------

mod_invert <- cnp_model_mcmc(TL = 25:75, param = list(
      Qc_m = 43.54966, Qc_sd = 2.540621, Qn_m = 10.75658, Qn_sd = 0.747464, Qp_m = 2.680854, Qp_sd = 0.4940187,
      Dc_m = 39.02479,	Dc_sd = 3.065907, Dn_m = 9.193625, Dn_sd	= 0.8810677, Dp_m = 1.2015, Dp_sd = 0.2838872,
      ac_m = 0.8, an_m = 0.8, ap_m = 0.7,
      linf_m = 101.1079, linf_sd = 4.998803, k_m = 0.175, k_sd = 0.015500, t0_m = -1.352, t0_sd = 0.1714,
      lwa_m = 0.009504, lwb_m = 3.078241,
      mdw_m = 0.258806,
      f0_m = 0.001687307, f0_sd = 3.460819e-10,
      alpha_m = 0.6993033, alpha_sd = 0.1095661, theta_m = 2.5, theta_sd = 0.5,
      r_m = 1.906930, h_m = 3.0, v_m = 24.1825, v_sd = 3.449313 
))

df_invert <- data.frame(mod_invert$summary) |> 
      mutate(Diet = "Crustaceans") |> 
      filter(variable %in% c("Fc", "Fn", "Fp",
                             "Ic", "In", "Ip"))

# visualizing effects of dietary differences ------------------------------

### carbon ingested and excreted daily - assumes meeting metabolic requirements
plot_cnp(mod = mod_all, y = c("Fc", "Ic"),
         x = "tl", probs = c(0.5))

plot_cnp(mod = mod_lepomis, y = c("Fc", "Ic"),
         x = "tl", probs = c(0.5))

plot_cnp(mod = mod_invert, y = c("Fc", "Ic"),
         x = "tl", probs = c(0.5))

### nitrogen ingested and excreted daily - assumes meeting metabolic requirements
plot_cnp(mod = mod_all, y = c("Fn", "In"),
         x = "tl", probs = c(0.5))

plot_cnp(mod = mod_lepomis, y = c("Fn", "In"),
         x = "tl", probs = c(0.5))

plot_cnp(mod = mod_invert, y = c("Fn", "In"),
         x = "tl", probs = c(0.5))

### phosphorus ingested and excreted daily - assumes meeting metabolic requirements
plot_cnp(mod = mod_all, y = c("Fp", "Ip"),
         x = "tl", probs = c(0.5))

plot_cnp(mod = mod_lepomis, y = c("Fp", "Ip"),
         x = "tl", probs = c(0.5))

plot_cnp(mod = mod_invert, y = c("Fp", "Ip"),
         x = "tl", probs = c(0.5))

# join all-together and plot side-by-side ---------------------------------

diet_effects_cs <- rbind(df_all, df_lepomis, df_invert)
glimpse(diet_effects_cs)

Ic <- diet_effects_cs |> 
      filter(variable == "Ic") |> 
      ggplot(aes(x = TL, y = mean, color = Diet, group = Diet)) +
      # geom_line() +
      geom_ribbon(aes(ymin = Q_25, ymax = Q_75, fill = Diet), alpha = 0.2) +
      labs(x = "Total Length (cm)",
           y = "Carbon Consumption CI (g/day)") +
      scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.title = element_blank(),
            axis.line = element_line("black"),
            axis.text = element_blank(),
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.position = "none")

In <- diet_effects_cs |> 
      filter(variable == "In") |> 
      ggplot(aes(x = TL, y = mean, color = Diet, group = Diet)) +
      # geom_line() +
      geom_ribbon(aes(ymin = Q_25, ymax = Q_75, fill = Diet), alpha = 0.2) +
      labs(x = "Total Length (cm)",
           y = "Nitrogen Consumption CI (g/day)") +
      scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.title = element_blank(),
            axis.line = element_line("black"),
            axis.text = element_blank(),
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.position = "none")

Ip <- diet_effects_cs |> 
      filter(variable == "Ip") |> 
      ggplot(aes(x = TL, y = mean, color = Diet, group = Diet)) +
      # geom_line() +
      geom_ribbon(aes(ymin = Q_25, ymax = Q_75, fill = Diet), alpha = 0.2) +
      labs(x = "Total Length (cm)",
           y = "Phosphorus Consumption CI (g/day)") +
      scale_y_continuous(breaks = seq(0, 0.7, by = 0.1), limits = c(0, 0.7)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.title = element_blank(),
            axis.line = element_line("black"),
            axis.text = element_blank(),
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.position = "none")

Fc <- diet_effects_cs |> 
      filter(variable == "Fc") |> 
      ggplot(aes(x = TL, y = mean, color = Diet, group = Diet)) +
      # geom_line() +
      geom_ribbon(aes(ymin = Q_25, ymax = Q_75, fill = Diet), alpha = 0.2) +
      labs(x = "Total Length (cm)",
           y = "Carbon Supply CI (g/day)") +
      scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.title = element_blank(),
            axis.line = element_line("black"),
            axis.text = element_blank(),
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.position = "none")

Fn <- diet_effects_cs |> 
      filter(variable == "Fn") |> 
      ggplot(aes(x = TL, y = mean, color = Diet, group = Diet)) +
      # geom_line() +
      geom_ribbon(aes(ymin = Q_25, ymax = Q_75, fill = Diet), alpha = 0.2) +
      labs(x = "Total Length (cm)",
           y = "Nitrogen Supply CI (g/day)") +
      scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.title = element_blank(),
            axis.line = element_line("black"),
            axis.text = element_blank(),
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.position = "none")

Fp <- diet_effects_cs |> 
      filter(variable == "Fp") |> 
      ggplot(aes(x = TL, y = mean, color = Diet, group = Diet)) +
      # geom_line() +
      geom_ribbon(aes(ymin = Q_25, ymax = Q_75, fill = Diet), alpha = 0.2) +
      labs(x = "Total Length (cm)",
           y = "Phosphorus Supply CI (g/day)") +
      scale_y_continuous(breaks = seq(0, 0.7, by = 0.1), limits = c(0, 0.7)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.title = element_blank(),
            axis.line = element_line("black"),
            axis.text = element_blank(),
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.position = "none")

ggpubr::ggarrange(Ic, Fc, 
                  In, Fn, 
                  Ip, Fp,
                  labels = c('a)','b)','c)','d)','e)','f)'),
                  font.label = list(size = 20, face = "bold"),
                  ncol = 2, nrow = 3, align = "v")

# ggsave("plots/diet-comparison.tiff", units = "in", width = 12,
#        height = 12, dpi =  600, compression = "lzw")

diet_effects_cs |> 
      filter(variable %in% c("Ic", "Fc")) |> 
      ggplot(aes(x = TL, y = mean, color = Diet, group = Diet)) +
      # geom_line() +
      geom_ribbon(aes(ymin = Q_25, ymax = Q_75, fill = Diet), alpha = 0.2) +
      facet_wrap(~variable) +
      labs(x = "Total Length (cm)",
           y = "Carbon Consumption CI (g/day)") +
      scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.title = element_text(face = "bold", color = "black", size = 18),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold", color = "black", size = 18),
            legend.title = element_text(face = "bold", color = "black", size = 18),
            legend.text = element_text(face = "bold", color = "black", size = 18),
            legend.position = "bottom")

# ggsave("plots/diet-comparison-axis-and-legend.tiff", units = "in", width = 12,
#        height = 12, dpi =  600, compression = "lzw")


# examine limitation across diet treatments -------------------------------

### mixed diet
mixed_limitation <- limitation(mod = mod_all, plot = FALSE) |> 
      mutate(Diet = "Mixed")

limitation(mod = mod_all, plot = TRUE) |> 
      mutate(Diet = "Mixed")

### sunfish diet
lepomis_limitation <- limitation(mod = mod_lepomis, plot = FALSE) |> 
      mutate(Diet = "Lepomis spp.")

limitation(mod = mod_lepomis, plot = TRUE) |> 
      mutate(Diet = "Lepomis spp.")

### crustaceans
crustacean_limitation <- limitation(mod = mod_invert, plot = FALSE) |> 
      mutate(Diet = "Crustaceans")

limitation(mod = mod_invert, plot = TRUE) |> 
      mutate(Diet = "Crustaceans")

diet_effects_lim <- rbind(mixed_limitation, lepomis_limitation, crustacean_limitation)

# examine sensitivity to different diet treatments ------------------------

### mixed diet
mixed_sensitivity <- sensitivity()

### sunfish diet
lepomis_sensitivity <- sensitivity()

### crustaceans
crustacean_sensitivity <- sensitivity()
      