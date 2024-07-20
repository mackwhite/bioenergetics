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

data(param_zebsco)
# aspect ratio ------------------------------------------------------------
### source = FishBase.org
asp_rat <- aspect_ratio("Centropomus undecimalis")
r = 1.90693

# trophic level -----------------------------------------------------------

troph_lev <- trophic_level("Centropomus undecimalis")
h = 4.125

# length-weight relationship ----------------------------------------------
### source = Froese, R., J. Thorson and R.B. Reyes Jr., 2013. A Bayesian approach for estimating length-weight relationships in fishes. J. Appl. Ichthyol. (2013):1-7
lwa_lwb <- find_lw("Centropomus undecimalis")
# species                     lwa_m    lwa_sd    lwb_m  lwb_sd
# 1 Centropomus undecimalis 0.00741 0.000994898  3.02 0.0255102

lwa_m = 0.00741
lwa_sd = 0.000994898
lwb_m = 3.02
lwb_sd = 0.0255102

### use shark river values, but may need to be redone since fishflux used g/cm

# growth parameters - vbg -------------------------------------------------
### source = FishBase.org
### use Taylor et al 2000 estimates but need to determine if those need to be recalculated bc 
### fisflux uses tl(cm) and 
linf_k_t0 <- growth_params("Centropomus undecimalis", otolith = TRUE)
linf <- 77.7
k <- 0.49

# metabolic parameters ----------------------------------------------------

data(metabolic_parameters)

perciformes_metabolic <- metabolic_parameters |> 
      filter(family %in% c("Apogonidae", "Centrarchidae", "Cichlidae", "Gobiidae",
                           "Pomacentridae", "Sciaenidae")) |> 
      mutate(order = "Perciformes") |> 
      group_by(order) |> 
      summarize(a_m_avg = mean(a_m),
                a_sd_avg = mean(a_sd),
                b0_m_avg = mean(b0_m),
                b0_sd_avg = mean(b0_sd))


# metabolism --------------------------------------------------------------
### source = Barneche & Allen 2018 Ecology Letters doi: 10.1111/ele.12947. These parameters are for the best model (Model 2 in the paper online supplementary material) of fish resting metabolic rates reported in the paper, which also includes trophic level as a covariate.
metabolism("Centropomidae", temp = 24.1825, troph_m = 4.125)
f0_m = 0.002530924
f0_sd = 5.077058e-10 
alpha_m = 0.77
alpha_sd = 0.05286288
b0_m = 0.00048083
b0_sd = 1e-10

# metabolic rate ----------------------------------------------------------
### these values need re-evaluated
metabolic_rate(temp = 24.1825, troph = 4.125, asp = 1.90693, B0 = 0.00048083,
               m_max = 7500, m = 4500, a = 0.77, growth_g_day = 0.018, 
               f = 2)

mod <- cnp_model_mcmc(TL = 25:75, param = list(
      Qc_m = 36.41 , Qc_sd = 2.71, Qn_m = 11.04, Qn_sd = 0.75, Qp_m = 4.27, Qp_sd = 0.13,
      Dc_m = 45.31226,	Dc_sd = 4.528513, Dn_m = 10.52142, Dn_sd	= 0.994411, Dp_m = 1.879727, Dp_sd = 0.662342,
      ac_m = 0.8, an_m = 0.8, ap_m = 0.7,
      linf_m = 101.1079, linf_sd = 4.998803, k_m = 0.175, k_sd = 0.015500, t0_m = -1.352, t0_sd = 0.1714,
      lwa_m = 0.009504, lwb_m = 3.078241,
      mdw_m = 0.258806,
      f0_m = 0.002531, f0_sd = 0.000000000515726,
      alpha_m = 0.77, alpha_sd = 0.052863, theta_m = 2.5, theta_sd = 0.5,
      r_m = 1.906930, h_m = 4.125, h_sd = 0.5, v_m = 24.1825, v_sd = 3.449313 
      ))

plot_cnp(mod = mod, y = c("Fp", "Ip"),
         x = "tl", probs = c(0.5, 0.8))



      