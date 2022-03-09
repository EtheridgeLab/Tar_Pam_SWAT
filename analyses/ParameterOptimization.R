#Package Installation

install.packages("hydroGOF")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("ggplot2")
install.packages(c("zoo", "latticeExtra", "polynom", "car", "Hmisc","reshape"))
install.packages("hydromad", repos="http://hydromad.catchment.org")
install.packages("DEoptim")
install.packages("dream", repos="http://hydromad.catchment.org")

#Loading R Packages

library(SWATplusR)
library(hydroGOF)
library(hydromad)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

#Loading Demo Data

# The path where the SWAT demo project will be written
demo_path <- "C:/Users/julie/Documents/R/"


# Loading the SWAT+ demo project on your hard drive
path_plus <- load_demo(dataset = "project",
                       version = "plus",
                       path = demo_path)


#Use observed data to evaluate model in each optimization step

q_obs <- load_demo(dataset = "observation")

#Limit the data to a time period

q_obs <- filter(q_obs, date >= ymd("2003-01-01"),
                       date <= ymd("2012-12-31"))

#Definition of the function to optimize
#Search for optimum parameter set that minimizes return value 
#Defined a function with one input argument par
#Par is passed to run_swatplus()
#Simulation returns discharge at main outlet for time period

swat_model <- function(par) {
  run_swatplus(project_path = path_plus,
               output = list(q_sim = define_output(file = "channel",
                                                   variable = "flo_out",
                                                   unit = 1)),
               parameter = par,
               start_date = "2000-01-01",
               end_date = "2012-12-31",
               years_skip = 3,
               quiet = TRUE)
}

#To convert from ha*m*day^-1 to (m^3)*(s^-1), divide by 8.64

swat_optim <- function(par, obs) {
  q_sim <- swat_model(par)
  nse_q <- NSE(q_sim$simulation$q_sim/8.64, obs)
  return(nse_q)
}


#Parameter Optimization with optim
#Uses observed discharge values from q_obs$discharge
#Evaluates simulated discharges using function NSE() from package hydroGOF that is then returned as a single value

#Seven parameters frequently used for model calibration with respect to simulated discharge

par_names <- c("cn2.hru | change = abschg",
               "lat_ttime.hru | change = absval",
               "lat_len.hru | change = absval",
               "k.sol | change = pctchg",
               "z.sol | change = pctchg",
               "epco.hru | change = absval",
               "esco.hru | change = absval")

#Optim function requires starting values (par_init)

par_init <- c(0, 3, 50, 0 , 0, 0.5, 0.5)

#Define lower boundary (par_lwr)

par_lwr  <- c(-15, 0.5,  10, -50, -50, 0, 0)

#Define upper boundary (par_upr)

par_upr  <- c( 10,  50, 100,  50,  50, 1, 1)

#Name the parameter sets using syntax for SWAT model parameters required for run_swat*() functions

names(par_init) <- par_names
names(par_lwr) <- par_names
names(par_upr) <- par_names

#The method='L-BFGS-B' defines parameter boundaries
#Omit feedback
#If want to get feedback, set control parameter trace=6 in list of control parameters

opt_bfgs <- optim(par = par_init, fn = swat_optim, method = "L-BFGS-B",
                  lower = par_lwr, upper = par_upr, obs = q_obs$discharge,
                  control = list(maxit = 100))

#Result of optimization is saved in a list object

#List entries opt_bfgs$par provides final optimum parameter set
opt_bfgs$par

#opt_bfgs$par shows obtimized objective criterion (NSE in this case)
opt_bfgs$value

#Rerun swat_model() defined above with optimum parameter set
q_bfgs <- swat_model(opt_bfgs$par)

#Visualize discharge time series simulation 
q_plot <- q_bfgs$simulation %>%
  mutate(q_sim = q_sim/8.64) %>%
  left_join(., q_obs, by = "date") %>%
  rename(q_obs = discharge) %>%
  gather(., key = "variable", value = "discharge", -date)

#Plot
ggplot(data = q_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  theme_bw()

#Parameter optimization with SCEoptim()

#Similar to optim() but add a few control arguments to define some settings in optimization

#Define argument reltol=0.001 to stop the optimization when the improvements in NSE are below the threshold
#Define argument tolsteps=3 to stop after third time improvement in NSE was below threshold
#Define argument trace=1 to get feedback from each optimization cycle

opt_sce  <- SCEoptim(swat_optim, par_init, lower = par_lwr, upper = par_upr, obs = q_obs$discharge,
                     control = list(reltol = 10^(-3), tolsteps = 3, trace = 1))
          
#SCEoptim() provides more detailed information compared to optim()
#SCEoptim() provides best parameter set of each iteration step
#Opt_sce$BESTMEM.ALL saves the best parameter set for each iteration step 

#Write in tibble to use in simulations

par_best <- as_tibble(opt_sce$BESTMEM.ALL)

q_sce <- run_swatplus(project_path = path_plus,
                      output = list(q_sim = define_output(file = "channel",
                                                          variable = "flo_out",
                                                          unit = 1)),
                      parameter = par_best,
                      start_date = "2000-01-01",
                      end_date = "2012-12-31",
                      years_skip = 3,
                      n_thread = 4)
  
opt_sce$POP.FIT.ALL <- opt_sce$POP.FIT.ALL[1:10,]

#Dotty plots

par_plot <- q_sce$parameter$values %>%
  mutate(nse = apply(opt_sce$POP.FIT.ALL, 1, min),
         run = paste0("run_", sprintf("%02d", 1:10))) %>%  
  gather(key = "par", value = "val", -nse, -run)

ggplot(par_plot) +
  geom_point(aes(x = val, y = abs(nse), col = run)) +
  facet_wrap(.~par, scales = "free") +
  theme_bw()              

#Plot all ten simulations by modifying data and plot command

sce_plot <- q_sce$simulation$q_sim %>%
  mutate_if(is.numeric, list(~./8.64)) %>%
  gather(., key = "variable", value = "discharge", -date)

obs_plot <- rename(q_obs, q_obs = discharge)

ggplot() +
  geom_line(data = obs_plot, aes(x = date, y = q_obs), col = "black") +
  geom_line(data = sce_plot,
            aes(x = date, y = discharge, col = variable), alpha = 0.75) +
  scale_color_brewer(palette = "Paired") +
  ylab("discharge") +
  theme_bw()

