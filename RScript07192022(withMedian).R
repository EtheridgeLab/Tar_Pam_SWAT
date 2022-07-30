# restart: only 

# rm (rm: clears list earlier; fresh start) function: creates empty list for incorporating data (no limits)
rm(list=ls())

#remote package used to solve multipurpose_weights, covariance
install.packages('remotes')
library(remotes)

#to solve the issue of zero data at exit (creates a tablew with empty dataset rather than error)
# got an error (data.table 1.14.2 using 4 threads (see ?getDTthreads).  Latest news: r-datatable.com)
install.packages("data.table")
library(data.table)

#using github we are installing SWATplusR package
remotes::install_github('chrisschuerz/SWATplusR')
library(SWATplusR)



#if we don't set WD we need to give entire location

setwd("C:/Users/TAPASM21/Desktop/SWATforOutletpoints/T2OP/T2OP/Scenarios/Trial3071720222003to2009/")
library(hydroGOF)
library(SWATplusR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tibble)

#demo_path <- 'Define:/your/path'
#set up the path to have no blanks
  path = 'C:/Users/TAPASM21/Desktop/SWATforOutletpoints/T2OP/T2OP/Scenarios/Trial3071720222003to2009/TxtInOut/'

#q_obs <- load_demo(dataset = 'observation')
q_obs=read.csv("Z:/SWAT/TP_Observed Fow Data/Trial editing observed flow data Greenville NC/TP_observedflow_Median(T6)_2003_2009.csv")

#Add extra stuff to change q_obs to 2 columns (date, discharge) date = yyyy-mm-dd
# I think date will need to be coerced into a date object???
#X and Y 2 column $X didnt get any error, so its in proper format
q_obs$X=NULL
q_obs
q_obs = q_obs %>% separate(Date, c("month", "day","year"))
q_obs$date2 = paste(q_obs$year, q_obs$month, q_obs$day, sep = "-")
q_obs$date3<-as.Date(q_obs$date2,format='%Y-%m-%d')
q_obs$month = NULL
q_obs$day= NULL
q_obs$year= NULL
q_obs$date2= NULL
q_obs$date= q_obs$date3
q_obs$date3= NULL
q_obs$d2=q_obs$discharge
q_obs$discharge=NULL
q_obs$discharge=q_obs$d2
q_obs$d2 = NULL


# ran our TxtInOut folder here with 2 simulation convert to 1000
n <- 16
par_set <- tibble(
  'cn2.hru | change = abschg' = runif(n,-44.78,-44.78),
  #                   'lat_ttime.hru | change = absval' = runif(n,4,15),
  'lat_len.hru | change = abschg' = runif(n,40,60),
  'epco.hru | change = absval' = runif(n,0.1,.7),
  'esco.hru | change = absval' = runif(n,0.4,.8),
  'perco.hru | change = absval' = runif(n,0.4,1),
  'k.sol | change = pctchg' = runif(n,-10,10),
  #                   'awc.sol | change = pctchg' = runif(n,-20,20),
  'ovn.hru | change = abschg' = runif(n,.5,6),
  'flo_min.aqu | change = abschg' = runif(n,0,500),
  'alpha.aqu | change = absval' = runif(n,0.15,0.3)
)
par_set
#Running multiple simulations
q_simn <- run_swatplus(project_path = path,
                       output = define_output(file = "channel_sd",
                                              variable = "flo_out",
                                              unit = 229),
                       parameter = par_set,
                       n_thread = 8)

#look at output (single op 2 diff datasets parameter and simulated value)
q_simn$parameter
q_simn$simulation



#create simulation output data frame
f = q_simn$simulation$flo_out


#check simulated data:
write.csv(f,'simulateTP.csv')

#f=f[c(),]
###Calculate NSE Scores
#prep NSE obs and sim matricies
run_sel_sub = f[, c(2:ncol(f))]
qmatrix=run_sel_sub
qmatrix[, c(1:ncol(qmatrix))] = q_obs$discharge

#checking outputs individually of qmatrix
write.csv(qmatrix,'qmatrixCheck.csv')

#Get NSE scores, filter out threshold data
run_sel = data.frame(rNSE(sim =run_sel_sub, obs = qmatrix))
run_sel$simID=row.names(run_sel)

#checking run_sel
write.csv(run_sel,'runselcheck.csv')


temp=colnames(run_sel)
run_sel = rename(run_sel,  NSE = temp[1] )
run_sel
min(run_sel$NSE)
max(run_sel$NSE)
###SET NSE THRESHOLD
threshold=min(run_sel$NSE)
run_sel = run_sel[run_sel$NSE => threshold,]
run_sel
#select simulations based on run_sel$simID
q_plus1 =f %>% select(run_sel$simID)
q_plus1= cbind(f[, 1], q_plus1)
par_set
temp2 = run_sel %>% separate(simID, c("run", "num"))
temp2$num = as.numeric(temp2$num)
run_sel = cbind(run_sel, temp2$num)
run_sel = rename(run_sel,  par_setID = "temp2$num" )
par_set_vis=par_set
par_set_vis = par_set_vis[c(run_sel$par_setID), ]
par_set_vis$NSE = run_sel$NSE
par_set_vis
write.csv(par_set_vis, "par_set_vis.csv")


####
q_plot <- q_obs %>%
  rename(q_obs = discharge) %>% # Rename the discharge columnt to q_obs
  filter(year(date) %in% 2003:2009) %>% # Filter for years between 2003 and 2012
  left_join(., q_plus1, by = 'date') %>% # Join with the q_plus table by date
  #left_join(., q_2012, by = 'date') %>% # Join with the q_plus table by date
  pivot_longer(., cols = -date, names_to = 'variable', values_to = 'discharge') # Make a long table for plotting



#Create jpeg name and size
jpeg(file="TP_Trial2.jpeg", width = 1600, height = 800)



#set line type to solid
lineset =colnames(q_plus1)
lineset = replace(lineset, 1:length(lineset), "solid")
plot_colors = lineset
plot_colors = replace(plot_colors, 2:length(plot_colors), "red")
plot_colors = replace(plot_colors, 1, "black")


ggplot(data = q_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = plot_colors) +
  scale_linetype_manual(values = lineset) +
  theme_bw()
dev.off()