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

# modeling where consuming mix of everything ------------------------------

mod_all <- cnp_model_mcmc(TL = 25:75, param = list(
      Qc_m = 43.54966, Qc_sd = 2.540621, Qn_m = 10.75658, Qn_sd = 0.747464, Qp_m = 2.680854, Qp_sd = 0.4940187,
      Dc_m = 43.50965,	Dc_sd = 2.817301, Dn_m = 10.50397, Dn_sd	= 0.8348159, Dp_m = 2.477653, Dp_sd = 0.4584972,
      ac_m = 0.8, an_m = 0.8, ap_m = 0.7,
      linf_m = 101.1079, linf_sd = 4.998803, k_m = 0.175, k_sd = 0.015500, t0_m = -1.352, t0_sd = 0.1714,
      lwa_m = 0.009504, lwb_m = 3.078241,
      mdw_m = 0.258806,
      f0_m = 0.002531, f0_sd = 0.000000000515726,
      alpha_m = 0.6993033, alpha_sd = 0.1095661, theta_m = 2.5, theta_sd = 0.5,
      r_m = 1.906930, h_m = 4.125, h_sd = 0.5, v_m = 24.1825, v_sd = 3.449313 
      ))

plot_cnp(mod = mod, y = c("Fp", "Ip", "Fn", "In", "Fc", "Ic"),
         x = "tl", probs = c(0.5))

plot_cnp(mod = mod, y = c("Fp", "Ip", "Fn", "In"),
         x = "tl", probs = c(0.8))

plot_cnp(mod = mod, y = c("Fp", "Ip"),
         x = "tl", probs = c(0.5, 0.8))

plot_cnp(mod = mod, y = c("Fp", "Ip"),
         x = "tl", probs = c(0.5))

all <- data.frame(mod$summary)