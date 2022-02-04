#Parameter sampling and model calibration
#test

#package installation

install.packages("lhs")
install.packages("hydroGOF")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tibble")
install.packages("tidyr")
install.packages("purrr")
install.packages("ggplot2")

#load R packages

library(SWATplusR)
library(lhs)
library(hydroGOF)
library(dplyr)
library(lubridate)
library(tibble)
library(tidyr)
library(purrr)
library(ggplot2)

#Loading demo data

# The path where the SWAT demo project will be written
demo_path <- "C:/Users/julie/Documents/R/"

# Loading the SWAT+ demo project on your hard drive
path_plus <- load_demo(dataset = "project",
                       version = "plus",
                       path = demo_path,)

q_obs <- load_demo(dataset = "observation")

#define calibration period for data set
q_obs_cal <- filter(q_obs, date >= ymd("2003-01-01"),
                    date <= ymd("2007-12-31"))

#define validation period for data set
q_obs_val <- filter(q_obs, date >= ymd("2008-01-01"),
                    date <= ymd("2012-12-31"))

#Parameter sampling
par_bound <- tibble("cn2.hru | change = abschg" = c(-15, 10),
                    "lat_ttime.hru | change = absval" = c(0.5, 50),
                    "lat_len.hru | change = absval" = c(10, 100),
                    "k.sol | change = pctchg" = c(-50, 50),
                    "z.sol | change = pctchg" = c(-50, 50),
                    "esco.hru | change = absval" = c(0, 1),
                    "epco.hru | change = absval" = c(0, 1))

#Random sampling with run(if)
#Creates uniformly distributed vector

n_sample <- 250
par_runif <- map_df(par_bound, ~ runif(n_sample, .x[1], .x[2]))
par_runif

#Random sampling with lhs

n_sample <- 250
n_par <- ncol(par_bound)
par_iter1 <- randomLHS(n = n_sample, k = n_par) %>%
  as_tibble(., .name_repair = "unique") %>%
  map2_df(., par_bound, ~ (.x * (.y[2] - .y[1]) + .y[1])) %>%
set_names(names(par_bound))

par_iter1

#Model Calibration

#Simulation Runs
#Implement all LHS sampled parameter combinations and simulate daily discharges

q_cal1 <- run_swatplus(project_path = path_plus,
                       output = list(q_sim = define_output(file = "channel",
                                                           variable = "flo_out",
                                                           unit = 1)),
                       parameter = par_iter1,
                       start_date = "2000-01-01",
                       end_date = "2007-12-31",
                       years_skip = 3,
                       n_thread = 4)

#Model Evaluation

nse_cal1 <- q_cal1$simulation$q_sim %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x/8.64, q_obs_cal$discharge))

#Identify relevant parameter and/or constrain parameter boundaries
#Dotty plot 

dotty_cal1 <- q_cal1$parameter$values %>%
  mutate(nse = nse_cal1) %>%
  filter(nse > -5) %>%
  gather(key = "par", value = "parameter_range", -nse)

ggplot(data = dotty_cal1) +
  geom_point(aes(x = parameter_range, y = nse)) +
  facet_wrap(.~par, ncol = 3, scales = "free_x") +
  theme_bw()

#Parameter update and re-evaluation of the model
#Modify boundaries of one parameter

par_bound$`lat_ttime.hru | change = absval` <- c(0, 5)

par_iter2 <- randomLHS(n = n_sample, k = n_par) %>%
  as_tibble(.) %>%
  map2_df(., par_bound, ~ (.x * (.y[2] - .y[1]) + .y[1])) %>%
  set_names(names(par_bound))

q_cal2 <- run_swatplus(project_path = path_plus,
                       output = list(q_sim = define_output(file = "channel",
                                     variable = "flo_out",
                                     unit = 1)),
                       parameter = par_iter2,
                       start_date = "2000-01-01",
                       end_date = "2007-12-31",
                       years_skip = 3,
                       n_thread = 4)

#Evaluate updated simulations

nse_cal2 <- q_cal2$simulation$q_sim %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x/8.64, q_obs_cal$discharge))

#Overview of NSE values for best simulation runs
sort(nse_cal2, decreasing = T) %>% enframe()

#Set all simulations that result in an NSE>0.7 as acceptable

run_sel <- which(nse_cal2 >= 0.7)

#Model Validation
#Parameter sampling and simulation
#Use indices of acceptable simulations and their parameter combinations to perform simulations

q_val <- run_swatplus(project_path = path_plus,
                      output = list(q_sim = define_output(file = "channel",
                                                          variable = "flo_out",
                                                          unit = 1)),
                      parameter = q_cal2$parameter,
                     run_index = run_sel,
                      start_date = "2005-01-01",
                      end_date = "2012-12-31",
                      years_skip = 3,
                      n_thread = 4, keep_folder = T)

#Evaluation of the calibration period
#Eval of validation period identical to eval of iterations of calibration period

nse_val <- q_val$simulation$q_sim %>%
  select(-date) %>%
  map_dbl(., ~NSE(.x/8.64, q_obs_val$discharge))


nse_comp <- tibble(run = names(nse_cal2[run_sel]),
                   calibration = nse_cal2[run_sel],
                   validation = nse_val)

nse_comp %>% 
  gather(key = "period", value = "nse", - run) %>% 
  ggplot(data = .) +
  geom_boxplot(aes(x = period, y = nse), fill = "grey") +
  theme_bw()

nse_comp %>% 
  arrange(desc(calibration))

#Visualization of discharge simulation
#Plot the ensemble of simulations as a band of min and max values for each time step
#Compare it to observations

run_best <- nse_comp$run[1]

get_run_stat <- function(run_table, best_run) {
  run_table %>% 
    select(-date) %>% 
    mutate(q_max = pmap_dbl(., max),
           q_min = pmap_dbl(., min)) %>%
    select(matches(best_run), q_min, q_max) %>%
    set_names(c("q_sim", "q_min", "q_max")) %>%
    add_column(date = run_table$date, .before = 1)
}

cal_stat <- q_cal2$simulation$q_sim %>% 
  select(!!names(q_val$simulation$q_sim)) %>% 
  get_run_stat(., run_best) %>% 
  mutate_if(., is.numeric, ~ (./8.64)) %>% 
  mutate(period = "calibration")

val_stat <- q_val$simulation$q_sim %>% 
  get_run_stat(., run_best) %>% 
  mutate_if(., is.numeric, ~ (./8.64)) %>% 
  mutate(period = "validation")

sim_stat <- bind_rows(cal_stat, val_stat) %>%
  left_join(., q_obs, by = "date") %>%
  rename( q_obs = discharge) %>%
  gather(key = "variable", value = "q", -date, -q_min, - q_max, -period)

ggplot(data = sim_stat) +
  geom_ribbon(aes(x = date, ymin = q_min, ymax = q_max), fill = "grey50") +
  geom_line(aes(x = date, y = q, col = variable), lwd = 0.25) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs( x = "Date (yyyy)", 
        y = expression (Discharge~(m^3~s^{-1})), 
        col = "Discharge") + 
  facet_wrap(period~., scales = "free_x", ncol = 1) + 
  theme_bw() + 
  theme(panel.spacing = unit(0.2, "lines"),
        legend.position = "bottom")

