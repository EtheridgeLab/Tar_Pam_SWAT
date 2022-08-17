#Parent Iteration (i0)


#Start fresh clear everything

rm(list=ls())


#Install these packages only once comment out afterwords (Copy RSWAT excecuatable file in the TxtInOut folder)
#install.packages('remotes')
#install.packages("data.table")
#remotes::install_github('chrisschuerz/SWATplusR')

#install libraries
library(remotes)
library(data.table)
library(SWATplusR)
library(hydroGOF)
library(SWATplusR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tibble)


#Set working directory
setwd("C:/Users/TAPASM21/Desktop/SWATforOutletpoints/T2OP/T2OP/Scenarios/Trial3071720222003to2009/")


#Set path for the TxtInOut folder (other than working directory: difficult to copy)
path = 'C:/Users/TAPASM21/Desktop/SWATforOutletpoints/T2OP/T2OP/Scenarios/Trial3071720222003to2009/TxtInOut/'


#Observed flow data: (USGS daily average flow converted data)
q_obs=read.csv("Z:/SWAT/TP_Observed Fow Data/Trial editing observed flow data Greenville NC/T11.csv")


#editing observed flow data
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


#generating random values for the parameters (adjust number of simulations depending on the computation power)
n <- 10


#Input parameters and correspoding range based on Cal_parms file in the TxtInOut folder (case specific: depends on watershed)
par_set <- tibble(
  'bf_max.aqu | change = absval' = runif(n,1.4744,2.087),
  'cn3_swf.hru | change = absval' = runif(n,0.301,0.3509),
  'dis_stream.hru | change = absval' = runif(n,48209,64707),
  'msk_co1.bsn | change = absval' = runif(n,5.567,6.351),
  'msk_co2.bsn | change = absval' = runif(n,5.785,6.351),
  'surlag.bsn | change = absval' = runif(n,12.677,15.751),
  'chw.rte | change = absval' = runif(n,558,1044),
  'chd.rte | change = absval' = runif(n,16,19),
  'chs.rte | change = absval' = runif(n,8.68,10.146),
  'chl.rte | change = absval' = runif(n,271,354),
  'chn.rte | change = absval' = runif(n,0.144,0.154),
  'chk.rte | change = absval' = runif(n,242,287),
  'revap_co.aqu | change = absval' = runif(n,0.103,0.122),
  'revap_min.aqu | change = absval' = runif(n,427,540),
  'cn2.hru | change = absval' = runif(n,45,48),
  'lat_ttime.hru | change = absval' = runif(n,29.7,79.4),
  'lat_len.hru | change = absval' = runif(n,53.8,93.15),
  'epco.hru | change = absval' = runif(n,0.19,0.455),
  'esco.hru | change = absval' = runif(n,0.053,0.068),
  'perco.hru | change = absval' = runif(n,0.920,0.928),
  'k.sol | change = absval' = runif(n,588,801),
  'awc.sol | change = absval' = runif(n,0.141,0.152),
  'ovn.hru | change = absval' = runif(n,25.2,29),
  'flo_min.aqu | change = absval' = runif(n,23,27.5),
  'alpha.aqu | change = absval' = runif(n,0.742,0.892)
)
par_set


#Generating simulated flow
q_simn <- run_swatplus(project_path = path,
                       output = define_output(file = "channel_sd",
                                              variable = "flo_out",

                                              
#check channel number carefully: there might be multiple channel files: use (channel reaches (rivs1))                                    
                                              unit = 116),
                       parameter = par_set,


#for parallel processing: change number of threads depending on computational power (keep some threads for other activities) 
                       n_thread = 8)


#Format simulated data as per observed data (no need to interpolate any missing values in the observed data): 
f<-dplyr::filter(q_simn$simulation$flo_out,date %in% q_obs$date)


#Any specific location for saving outputs ( I call it as RD)
RD <- 'Z:/SWAT/TP_FIles/RSWAT_Simulations_ExcelSheet/FlowOutput'


#writing simulated flow data (may be useful later: we can recall here and continue from this step)
write.csv(f,file.path(RD,"10_08122022.4Auto.csv"))


#Calculating Nash Sutcliiffe Efficiency
run_sel_sub = f[, c(2:ncol(f))]
qmatrix=run_sel_sub
qmatrix[, c(1:ncol(qmatrix))] = q_obs$discharge
run_sel = data.frame(NSE(sim =run_sel_sub, obs = qmatrix))
run_sel$simID=row.names(run_sel)
temp=colnames(run_sel)
run_sel = rename(run_sel,  NSE = temp[1] )
run_sel
min(run_sel$NSE)
max(run_sel$NSE)


#Top NSE values and corresponding parameters table (advisable to take top 10%)
run_sel = top_n(run_sel, 10, NSE)
run_sel
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


#plot (QSimulated & Qobserved)
q_plot <- q_obs %>%
  rename(q_obs = discharge) %>% # Rename the discharge columnt to q_obs
  filter(year(date) %in% 2003:2009) %>% # Filter for years between 2003 and 2012
  left_join(., q_plus1, by = 'date') %>% # Join with the q_plus table by date
  pivot_longer(., cols = -date, names_to = 'variable', values_to = 'discharge') # Make a long table for plotting


#Create jpeg name and size
jpeg(file= paste0(RD,"S1.jpeg"), width = 1600, height = 800)
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


#Automatated part starts here:- (for 500 runs) 
for (i in 1:500) {
  
#made it as a percentage change to tackle the issue of limiting variables
  a<-1.1*max(par_set_vis$`bf_max.aqu | change = absval`)
  aa<-0.9* min(par_set_vis$`bf_max.aqu | change = absval`)
  b<- 1.2*max(par_set_vis$`cn3_swf.hru | change = absval`)
  bb<-0.8* min(par_set_vis$`cn3_swf.hru | change = absval`)
  c<- 1.05*max(par_set_vis$`dis_stream.hru | change = absval`)
  cc<- 0.95*min(par_set_vis$`dis_stream.hru | change = absval`)
  d<-1.1*max(par_set_vis$`msk_co1.bsn | change = absval`)
  dd<- 0.9*min(par_set_vis$`msk_co1.bsn | change = absval`)
  e<- 1.1*max(par_set_vis$`msk_co2.bsn | change = absval`)
  ee<- 0.9*min(par_set_vis$`msk_co2.bsn | change = absval`)
  eee<- 1.1*max(par_set_vis$`surlag.bsn | change = absval`)
  eeee<- 0.9*min(par_set_vis$`surlag.bsn | change = absval`)
  g<-1.05*max(par_set_vis$`chw.rte | change = absval`)
  gg<- 0.95*min(par_set_vis$`chw.rte | change = absval`)
  h<- 1.1*max(par_set_vis$`chd.rte | change = absval`)
  hh<- 0.9*min(par_set_vis$`chd.rte | change = absval`)
  hhh<-1.1*max(par_set_vis$`chs.rte | change = absval`)
  hhhh<- 0.9*min(par_set_vis$`chs.rte | change = absval`)
  j<- 1.05*max(par_set_vis$`chl.rte | change = absval`)
  jj<- 0.95*min(par_set_vis$`chl.rte | change = absval`)
  k<- 1.2*max(par_set_vis$`chn.rte | change = absval`)
  kk<- 0.8*min(par_set_vis$`chn.rte | change = absval`)
  l<- 1.05*max(par_set_vis$`chk.rte | change = absval`)
  ll<- 0.95*min(par_set_vis$`chk.rte | change = absval`)
  m<- 1.05*max(par_set_vis$`revap_co.aqu | change = absval`)
  mm<- 0.95*min(par_set_vis$`revap_co.aqu | change = absval`)
  z<-1.05*max(par_set_vis$`revap_min.aqu | change = absval`)
  zz<- 0.95*min(par_set_vis$`revap_min.aqu | change = absval`)
  o<- 1.1*max(par_set_vis$`cn2.hru | change = absval`)
  oo<- 0.9*min(par_set_vis$`cn2.hru | change = absval`)
  p<- 1.1*max(par_set_vis$`lat_ttime.hru | change = absval`)
  pp<- 0.9*min(par_set_vis$`lat_ttime.hru | change = absval`)
  q<- 1.1*max(par_set_vis$`lat_len.hru | change = absval`)
  qq<- 0.9*min(par_set_vis$`lat_len.hru | change = absval`)
  r<- 1.2*max(par_set_vis$`epco.hru | change = absval`)
  rr<-0.8* min(par_set_vis$`epco.hru | change = absval`)
  s<- 1.2*max(par_set_vis$`esco.hru | change = absval`)
  ss<- 0.8*min(par_set_vis$`esco.hru | change = absval`)
  t<- 1.2*max(par_set_vis$`perco.hru | change = absval`)
  tt<- 0.8*min(par_set_vis$`perco.hru | change = absval`)
  u<- 1.05*max(par_set_vis$`k.sol | change = absval`)
  uu<- 0.95*min(par_set_vis$`k.sol | change = absval`)
  v<- 1.2*max(par_set_vis$`awc.sol | change = absval`)
  vv<- 0.8*min(par_set_vis$`awc.sol | change = absval`)
  w<- 1.1*max(par_set_vis$`ovn.hru | change = absval`)
  ww<- 0.9*min(par_set_vis$`ovn.hru | change = absval`)
  x<- 1.1*max(par_set_vis$`flo_min.aqu | change = absval`)
  xx<- 0.9*min(par_set_vis$`flo_min.aqu | change = absval`)
  y<- 1.2*max(par_set_vis$`alpha.aqu | change = absval`)
  yy<- 0.8*min(par_set_vis$`alpha.aqu | change = absval`)
  

#keep simulation number low and increase the number of iterations: produce results faster with same computational power    
  n <- 21


#par_set1 generates radom varaibles by taking parameter range of i-1th iteration's top NSE values    
    par_set1 <- tibble(
    'bf_max.aqu | change = absval' = runif(n,aa,a),
    'cn3_swf.hru | change = absval' = runif(n,bb,b),
    'dis_stream.hru | change = absval' = runif(n,cc,c),
    'msk_co1.bsn | change = absval' = runif(n,dd,d),
    'msk_co2.bsn | change = absval' = runif(n,ee,e),
    'surlag.bsn | change = absval' = runif(n,eeee,eee),
    'chw.rte | change = absval' = runif(n,gg,g),
    'chd.rte | change = absval' = runif(n,hh,h),
    'chs.rte | change = absval' = runif(n,hhhh,hhh),
    'chl.rte | change = absval' = runif(n,jj,j),
    'chn.rte | change = absval' = runif(n,kk,k),
    'chk.rte | change = absval' = runif(n,ll,l),
    'revap_co.aqu | change = absval' = runif(n,mm,m),
    'revap_min.aqu | change = absval' = runif(n,zz,z),
    'cn2.hru | change = absval' = runif(n,oo,o),
    'lat_ttime.hru | change = absval' = runif(n,pp,p),
    'lat_len.hru | change = absval' = runif(n,qq,q),
    'epco.hru | change = absval' = runif(n,rr,r),
    'esco.hru | change = absval' = runif(n,ss,s),
    'perco.hru | change = absval' = runif(n,tt,t),
    'k.sol | change = absval' = runif(n,uu,u),
    'awc.sol | change = absval' = runif(n,vv,v),
    'ovn.hru | change = absval' = runif(n,ww,w),
    'flo_min.aqu | change = absval' = runif(n,xx,x),
    'alpha.aqu | change = absval' = runif(n,yy,y)
  )
  par_set1
  
  
 #Adding top NSE variable values from i-1 th iteration:- NSE will always go up 
  Ram<-par_set_vis[which.max(par_set_vis$NSE),-26]
    par_set<-rbind(Ram,par_set1)
  
    
#Running multiple simulations (reduced threads here so I can keep it running in the background and still able to do other tasks on the desktop)
  q_simn <- run_swatplus(project_path = path,
                         output = define_output(file = "channel_sd",
                                                variable = "flo_out",
                                                unit = 116),
                         parameter = par_set,
                         n_thread = 4)
  

#create simulation output data frame
  f = q_simn$simulation$flo_out
  f<-dplyr::filter(f,date %in% q_obs$date)


#writing multiple csv files for flow  
RD <- 'Z:/SWAT/TP_FIles/RSWAT_Simulations_ExcelSheet/FlowOutput'
write.csv(f,file.path(RD, paste(i,"sim.csv",row.names(FALSE))))
  
  
#NSE calculations
  run_sel_sub = f[, c(2:ncol(f))]
  qmatrix=run_sel_sub
  qmatrix[, c(1:ncol(qmatrix))] = q_obs$discharge
  run_sel = data.frame(NSE(sim =run_sel_sub, obs = qmatrix))
  run_sel$simID=row.names(run_sel)
  temp=colnames(run_sel)
  run_sel = rename(run_sel,  NSE = temp[1] )
  run_sel
  min(run_sel$NSE)
  max(run_sel$NSE)
  
  
#SET NSE THRESHOLD (Select top 5% top NSE rows here)
  run_sel = top_n(run_sel, 2, NSE)
  run_sel
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

  
#writing multiple NSE and correspoding parameters table    
  write.csv(par_set_vis,file.path(RD, paste(i,"Par.csv",row.names(FALSE))))
}























