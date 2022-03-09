#Parameter sensitivity analysis

#Package installation
install.packages("dplyr")
install.packages("fast")
install.packages("forcats")
install.packages("ggplot2")
install.packages("hydroGOF")
install.packages("sensitivity")
install.packages("tidyr")
install.packages("purrr")

#Loading R packages

library(dplyr)
library(fast)
library(forcats)
library(ggplot2)
library(hydroGOF)
library(lubridate)
library(purrr)
library(SWATplusR)
library(sensitivity)
library(tidyr)

#Loading demo data

# The path where the SWAT project will be written
path_plus <- "C:/swatplus_tp3/04_sar/TxtInOut/"

q_obs <- read.csv(paste0(path_plus,"obs_daily_200305_chanel91.csv"))

q_obs <- filter(q_obs, Date >= ymd("2003-01-01"),
                Date <= "2005-12-31")

#Model Parameters

#This uses 7 parameters frequently used for model calibration with respect to simulated discharge
#First define parameter names

par_names <- c("cn2.hru | change = abschg",
               "lat_ttime.hru | change = absval",
               "lat_len.hru | change = absval",
               "k.sol | change = pctchg",
               "z.sol | change = pctchg",
               "esco.hru | change = absval",
               "epco.hru | change = absval")

#Sensitivity analysis with fast
#Requires few model evaluations
#Could not get this part to work

#Sensitivity analysis with the method of Sobol
#Function has to be defined that returns the scalar variable for which the sensitivity is assessed

swat_sobol <- function(par, obs) {
  q_sim <- run_swatplus(project_path = path_plus,
                        output =list(q_sim = define_output(file = "channel",
                                                           variable = "flo_out",
                                                           unit = 1)),
                        parameter = par,
                        start_date = "2001-01-01",
                        end_date = "2005-12-31",
                        years_skip = 2, n_thread = 4,
                        add_date = FALSE)
  nse_q <- map_dbl(q_sim$simulation$q_sim/8.64, ~ NSE(.x, obs))
  return(nse_q)
}

#Two random sets of samples with same sample size for the parameters that should be analyzed are required

par_bound <- tibble("cn2.hru | change = abschg" = c(-15, 10),
                    "lat_ttime.hru | change = absval" = c(0.5, 50),
                    "lat_len.hru | change = absval" = c(10, 100),
                    "k.sol | change = pctchg" = c(-50, 50),
                    "z.sol | change = pctchg" = c(-50, 50),
                    "esco.hru | change = absval" = c(0, 1),
                    "epco.hru | change = absval" = c(0, 1))
n_par  <- 7
n_samp <- 500

x1 <- data.frame(matrix(runif(n_par * n_samp), nrow = n_samp)) %>%
  set_names(., names(par_bound)) %>%
  map2_dfc(., par_bound, ~ (.x * (.y[2] - .y[1]) + .y[1]))
x2 <- data.frame(matrix(runif(n_par * n_samp), nrow = n_samp)) %>%
  set_names(., names(par_bound)) %>%
  map2_dfc(., par_bound, ~ (.x * (.y[2] - .y[1]) + .y[1]))

#Command to use method of Sobol

sens_sobol <- sobol(model = swat_sobol, X1 = x1, X2 = x2, 
                    obs = q_obs$discharge, nboot = 100)
sens_sobol

#Visualization of Sobol analysis
#Parameters are ranked by their sensitivity
#Visualize parameter ranking in a barplot

plot_sobol <- sens_sobol$S %>%
  mutate(parameter = rownames(.)) %>%
  mutate(parameter = factor(parameter) %>% fct_reorder(., original))
ggplot(data = plot_sobol) +
  geom_pointrange(aes(x = parameter, y = original ,
                      ymin = `min. c.i.`, ymax = `max. c.i.`)) +
  coord_flip() +
  xlab("Parameter") +
  ylab("Sensitivity") +
  theme_bw()
