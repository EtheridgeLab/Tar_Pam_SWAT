rm(list=ls())
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


#WorkingDirectory:-
setwd("C:/Users/TAPASM21/Desktop/RSWAT/NitrateSWATR/Directory/")

#copy excecutable in the TxtInOut
path = 'C:/Users/TAPASM21/Desktop/SWATforOutletpoints/T2OP/T2OP/Scenarios/ExcessNitrogen08262022Run02to09/TxtInOut/'

q_obs<-read.csv("C:/Users/TAPASM21/Desktop/RSWAT/NitrateSWATR/ObservedData/Nitrate2003to2009.csv")

q_obs$X=NULL
q_obs
#look for letter case carefully here_if that doesn't work change type in excel from general to number
q_obs = q_obs %>% separate(Date, c("month", "day","year"))
q_obs$date2 = paste(q_obs$year, q_obs$month, q_obs$day, sep = "-")
q_obs$date3<-as.Date(q_obs$date2,format='%Y-%m-%d')
q_obs$month = NULL
q_obs$day= NULL
q_obs$year= NULL
q_obs$date2= NULL
q_obs$date= q_obs$date3
q_obs$date3= NULL
q_obs$d2=q_obs$Nitrate
q_obs$Nitrate=NULL
q_obs$Nitrate=q_obs$d2
q_obs$d2 = NULL

z<-q_obs
z<-group_by(z,Date, .add = FALSE, .drop = group_by_drop_default(z)) %>% summarize(Nitrate =mean(Nitrate))
q_obs<- z

# its fun to see changes in the observed data

#run the TxtInOut folder here with 2 simulation (to identify parameters)
#convert to 1000
# For cn2.hru
n <- 200
par_set <- tibble(
  'bf_max.aqu | change = absval' = runif(n,2.3295,2.4),
  'cn3_swf.hru | change = absval' = runif(n,0.84029,0.841),
  'dis_stream.hru | change = absval' = runif(n,96482,96483),
  'msk_co1.bsn | change = absval' = runif(n,13.8547,13.855),
  'msk_co2.bsn | change = absval' = runif(n,10.1306,10.131),
  'surlag.bsn | change = absval' = runif(n,19.74004,19.75),
  'chw.rte | change = absval' = runif(n,1048.3,1049),
  'chd.rte | change = absval' = runif(n,10.021,10.03),
  'chs.rte | change = absval' = runif(n,10.4752,10.48),
  'chl.rte | change = absval' = runif(n,297.5,298),
  'chn.rte | change = absval' = runif(n,0.1275,0.128),
  'chk.rte | change = absval' = runif(n,297.5,298),
  'revap_co.aqu | change = absval' = runif(n,0.2367,0.237),
  'revap_min.aqu | change = absval' = runif(n,594.73,594.8),
  'cn2.hru | change = absval' = runif(n,49.79,49.8),
  'lat_ttime.hru | change = absval' = runif(n,89.569,89.6),
  'lat_len.hru | change = absval' = runif(n,55.039,55.04),
  'epco.hru | change = absval' = runif(n,0.8133,0.814),
  'esco.hru | change = absval' = runif(n,0.0031,0.0032),
  'perco.hru | change = absval' = runif(n,0.9657,0.966),
  'k.sol | change = absval' = runif(n,1181.225,1181.3),
  'awc.sol | change = absval' = runif(n,0.1151,0.1152),
  'ovn.hru | change = absval' = runif(n,72.5275,72.5276),
  'flo_min.aqu | change = absval' = runif(n,38.1395,38.14),
  'alpha.aqu | change = absval' = runif(n,0.51088,0.511),
  'erorgn.hru | change = absval' = runif(n,0,5),
  'biomix.hru | change = absval' = runif(n,0,1),
  'lat_orgn.hru | change = absval' = runif(n,0,200),
  'cmn.bsn | change = absval' = runif(n,0.001,0.003),
  'n_updis.bsn | change = absval' = runif(n,0,100),
  'nperco.bsn | change = absval' = runif(n,0,1),
  'rsdco.bsn | change = absval' = runif(n,0.02,0.1),
  'cdn.bsn | change = absval' = runif(n,0,3),
  'sdnco.bsn | change = absval' = runif(n,0,1),
  'rs2.swq | change = absval' = runif(n,0.001,0.1),
  'bc1.swq | change = absval' = runif(n,0.1,1),
  'bc3.swq | change = absval' = runif(n,0.2,0.4)
)
par_set

#Running multiple simulations
q_simn <- run_swatplus(project_path = path,
                       output = list(q_cha = define_output(file = 'channel_sd',
                                                           variable = 'flo_out',
                                                           unit = 112),
                                     q_cha = define_output(file = 'channel_sd',
                                                           variable = "no3_out",
                                                           unit = 112)),
                       parameter = par_set,
                       n_thread = 7)

#look at output (single op 2 diff datasets parameter and simulated value)

a<- q_simn$simulation$q_cha...2[,1:2]

c<- inner_join(x =q_simn$simulation$q_cha...2[,1:2], y = q_simn$simulation$q_cha...3, by = "date")

c<-c %>% filter(data.frame(c[,2]>0))

d<- c[,-1]
e<- data.frame(c)

g<- e[,3:202]*0.011574/e[,2]

#for adding date column to left
i <-cbind(e$date,g)
#for changing name
names(i)[names(i) == 'e$date'] <- 'date'

f<-i

f<-dplyr::filter(f,date %in% q_obs$date)

q_obs <- dplyr::filter(q_obs, date %in% f$date)


d<- q_simn$simulation$

write.csv(q_simn$simulation$q_cha...2, "flow.csv")
write.csv(q_simn$simulation$q_cha...3,"nitrate.csv")
q_simn$simulation


#create simulation output data frame

fq<-q_simn$simulation$q_cha
fn<q_simn$simulation$q_sur
plot(fq,fn)
# removing zero element rows from simulated data
##to change number of colums as of simulations####
fq<-fq %>% filter(data.frame(fq[,2]>0))


#for meging double dates in observed data # to do only once so removed from here
c<-group_by(Obs,date, .add = FALSE, .drop = group_by_drop_default(Obs)) %>%

#  summarize_all(mean)
z<-inner_join(x = fq[,1:2], y = fn, by = "date")

#write.csv(z,file.path("NSE1.csv"))

##remove number of simulations + 2nd column from here: i.e. for 2 simulations remove column 4
f = subset(z, select = -c(4) )

#prep NSE obs and sim matricies: NSE
run_sel_sub = f[, c(2:ncol(f))]
qmatrix=run_sel_sub
qmatrix[, c(1:ncol(qmatrix))] = q_obs$Nitrate
run_sel = data.frame(NSE(sim =run_sel_sub, obs = qmatrix))
run_sel$simID=row.names(run_sel)
temp=colnames(run_sel)
run_sel = rename(run_sel,  NSE = temp[1] )
run_sel
min(run_sel$NSE)
max(run_sel$NSE)

###SET NSE THRESHOLD
run_sel = top_n(run_sel, 10, NSE)
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

setnames(q_plus1,"f[, 1]" , "date")

write.csv(q_plus1,"qplus1.csv")

write.csv(par_set_vis, "par_set_vis.csv")
write.csv(par_set_vis,file.path(RD,"PS_1_Auto.csv"))



q_plot <- q_obs %>%
  rename(q_obs = Nitrate) %>% # Rename the discharge columnt to q_obs
  filter(year(date) %in% 2003:2009) %>% # Filter for years between 2003 and 2012
  left_join(., q_plus1, by = 'date') %>% # Join with the q_plus table by date
  #left_join(., q_2012, by = 'date') %>% # Join with the q_plus table by date
  pivot_longer(., cols = -date, names_to = 'variable', values_to = 'Nitrate') # Make a long table for plotting

#Create jpeg name and size
jpeg(file="TP_Trial2.jpeg", width = 1600, height = 800)

#set line type to solid
lineset =colnames(q_plus1)
lineset = replace(lineset, 1:length(lineset), "solid")
plot_colors = lineset
plot_colors = replace(plot_colors, 2:length(plot_colors), "red")
plot_colors = replace(plot_colors, 1, "black")

ggplot(data = q_plot) +
  geom_line(aes(x = date, y = Nitrate, col = variable, lty = variable)) +
  scale_color_manual(values = plot_colors) +
  scale_linetype_manual(values = lineset) +
  theme_bw()
dev.off()



ggplot(data = q_obs) +
  geom_line(aes(x = date, y = Nitrate)) +
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
                         output = list(q_cha = define_output(file = 'channel_sd',
                                                             variable = 'flo_out',
                                                             unit = 112),
                                       q_cha = define_output(file = 'channel_sd',
                                                             variable = "no3_out",
                                                             unit = 112)),
                         parameter = par_set,
                         n_thread = 7)
  

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






