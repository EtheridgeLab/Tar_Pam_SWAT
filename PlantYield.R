rm(list=ls())
setwd("C:/RSWAT03172023/")





library(hydroGOF)
library(SWATplusR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tibble)
library(lhs)

#set up the path to have no blanks
path = 'C:/Users/TAPASM21/Desktop/SWATPlusEntireProjects/25SubsNoParms/Scenarios/Nitr20ParmsExptNNSE044/TxtInOut'


n <- 1200
par_set <- tibble(
  'cn2.hru | change = pctchg' = runif(n, -30, 30),
  'perco.hru | change = abschg' = runif(n, -0.8, 0.3),
  'biomix.hru | change = abschg' = runif(n, -0.088, -0.088),
  'n_updis.bsn | change = abschg' = runif(n, 21.82, 21.82),
  'nperco.bsn | change = absval' = runif(n, 0.088, 0.088),
  'alpha.aqu | change = pctchg' = runif(n, 6.4, 6.4),
  'awc.sol | change = abschg' = runif(n, -0.036, -0.036),
  'chk.rte | change = pctchg' = runif(n, 18.3, 18.3),
  'cn3_swf.hru | change = abschg' = runif(n, -0.6, 0.6),
  'epco.hru | change = abschg' = runif(n, 19.8, 19.8),
  'flo_min.aqu | change = pctchg' = runif(n, 35.6, 35.6),
  'ovn.hru | change = abschg' = runif(n, 2.832, 2.832),
  'revap_co.aqu | change = abschg' = runif(n, -6.7, -6.7),
  'surlag.bsn | change = absval' = runif(n, 2.268, 2.268),
  'evlai.bsn | change = absval' = runif(n, 606.2, 606.2),
  'lat_len.hru | change = pctchg' = runif(n, 16.0, 16.0),
  'erorgp.hru | change = pctchg' = runif(n, -33.3, -33.3),
  'petco.hru | change = pctchg' = runif(n, -10.8, -10.8),
  'lat_orgn.hru | change = pctchg' = runif(n, 27.0, 27.0),
  'field_len.hru | change = pctchg' = runif(n, 13.0, 13.0),
  'crk.sol | change = pctchg' = runif(n, 8.2, 8.2)
)
par_set


q_simn <- run_swatplus(project_path = path, 
                       output = list(Agrr = define_output(file = "hru_pw",
                                                          variable = "yield",
                                                          unit = 142),
                       
                                    Corn = define_output(file = "hru_pw",
                                                              variable = "yield",
                                                              unit = 171),
                                    Cots = define_output(file = "hru_pw",
                                                              variable = "yield",
                                                              unit = 178),
                                    Soyb = define_output(file = "hru_pw",
                                                              variable = "yield",
                                                              unit = 185)),
                       
                       output_interval = "a",
                       start_date = 20030101,
                       end_date = 20111231,
                       parameter = par_set,
                       n_thread = 8)


a<- q_simn$simulation$Agrr
b<-q_simn$simulation$Corn
c<-q_simn$simulation$Cots

saverage<- data.frame(matrix(NA, nrow= n, ncol= 8))




a <- a[, -1]

average[,1] <- as.data.frame(colMeans(a)*365)



b <- b[, -1]

average[,2] <- as.data.frame(colMeans(b)*365)


c <- c[, -1]

average[,3] <- as.data.frame(colMeans(c)*365)

average$X4 <- average$X1 + average$X2 + average$X3

average$`SurfNitr/LatNitr`<- average$SurfNit/average$GWNitr

average$`GwNitr/LatNitr` <- average$LatNite / average$GWNitr

average$X7 <- q_simn$parameter$values$cn2
average$X8 <- q_simn$parameter$values$perco


colnames(average) <- c("SurfNit", "GWNitr", "LatNite","SumOf3","SurfNitr/LatNitr", "GwNitr/LatNitr", "cn2", "perco")




####to delete

x<- q_simn$simulation$LatNitra

x = x %>% separate(date, c("year", "month","day"))


x$year<- as.numeric(x$year)
x$month= NULL 
x$day= NULL 

sapply(x, mode)


annual_avg <- aggregate(. ~ year, x, FUN = sum)


