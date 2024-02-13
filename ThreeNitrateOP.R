#################REMOTE SERVER SCRIPT##############################

setwd("E:/Kate_REU_Server/7_5_2023/r_script")

install.packages("lhs")
library(hydroGOF)
library(SWATrunR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tibble)
library(lhs)


#set up the path to have no blanks
path = 'E:/Kate_REU_Server/7_5_2023/text_in_out/Default/Default/'


n <- 1000
par_set <- tibble(
  'cn2.hru | change = pctchg' = runif(n,5,40),
  'perco.hru | change = abschg' = runif(n,-0.20,0.40),
  'cn3_swf.hru | change = abschg' = runif(n,-0.8,0.5),
  'surlag.bsn | change = absval' = runif(n,2,20),
  'nperco.bsn | change = absval' = runif(n,-0.05,1),
  'k.sol | change = pctchg' = runif(n,-20,20),
  
)
par_set

######simulation######
q_simn <- run_swatplus(project_path = path, 
                       output = list(SurfNitr = define_output(file = "hru_ls",
                                                              variable = "surqno3",
                                                              unit = 4255),
                                     GWNitr = define_output(file = "hru_pw",
                                                            variable = "percn",
                                                            unit = 4255),
                                     LatNitra = define_output(file = "hru_ls",
                                                              variable = "lat3no3",
                                                              unit = 4255),
                                     SurfNitr = define_output(file = "hru_ls",
                                                              variable = "surqno3",
                                                              unit = 4259),
                                     GWNitr = define_output(file = "hru_pw",
                                                            variable = "percn",
                                                            unit = 4259),
                                     LatNitra = define_output(file = "hru_ls",
                                                              variable = "lat3no3",
                                                              unit = 4259),
                                     SurfNitr = define_output(file = "hru_ls",
                                                              variable = "surqno3",
                                                              unit = 4263),
                                     GWNitr = define_output(file = "hru_pw",
                                                            variable = "percn",
                                                            unit = 4263),
                                     LatNitra = define_output(file = "hru_ls",
                                                              variable = "lat3no3",
                                                              unit = 4263),
                                     SurfNitr = define_output(file = "hru_ls",
                                                              variable = "surqno3",
                                                              unit = 4251),
                                     GWNitr = define_output(file = "hru_pw",
                                                            variable = "percn",
                                                            unit = 4251),
                                     LatNitra = define_output(file = "hru_ls",
                                                              variable = "lat3no3",
                                                              unit = 4251)),
                       output_interval = "a",
                       start_date = 20030101,
                       end_date = 20191231,
                       
                       parameter = par_set,
                       n_thread = 10)

####new code as of 7/13/2023 REMOTE SERVER######

#####4255#########

#surface nitrate
average1<- data.frame(matrix(NA, nrow= n, ncol= 12)) 
a1<- q_simn$simulation$SurfNitr...2
a1 <- a1[, -1]                        #taking the results from just this column? and putting in a1
average1[, 1] <- as.data.frame(colMeans(a1))     #getting average from that column

#ground water nitrate 4255
b1<- q_simn$simulation$GWNitr...10
b1 <-b1 [, -1]
average1[, 2] <- as.data.frame(colMeans(b1))


#lateral 4255
c1<-q_simn$simulation$LatNitra...3
c1 <-c1 [, -1]
average1[, 3] <- as.data.frame(colMeans(c1))

####4259####

##hru 4259 is a2

#surface nitrate
average2<- data.frame(matrix(NA, nrow= n, ncol= 12)) 
a2<- q_simn$simulation$SurfNitr...4
a2 <- a2 [, -1]
average2[, 1] <- as.data.frame(colMeans(a2))   ##error here: replacement element 1 has 198, need 200

#ground water 4259
b2<- q_simn$simulation$GWNitr...11
b2 <-b2 [, -1]
average2[, 2] <- as.data.frame(colMeans(b2))

#lateral 4259
c2<-q_simn$simulation$LatNitra...5
c2 <-c2 [, -1]
average2[, 3] <- as.data.frame(colMeans(c2))

#####4263#####
##HRU 4263 is a3

#surface nitrate 4263
a3<- q_simn$simulation$SurfNitr...6
a3 <- a3[, -1]
average[,1] <- as.data.frame(colMeans(a3))
colMeans(a3)

#groundwater 4263
b3<- q_simn$simulation$GWNitr...11
b3 <-b3 [, -1]
average[, 1] <- as.data.frame(colMeans(b3))

#lateral 4263
c3<-q_simn$simulation$LatNitra...7
c3 <-c3 [, -1]
average[, 1] <- as.data.frame(colMeans(c3))

#####4251#####
##hru 4251

#surface nitrate
a4<- q_simn$simulation$SurfNitr...8
a4 <- a4[, -1]
average[,1] <- as.data.frame(colMeans(a4))

#groundwater 4251
b4<- q_simn$simulation$GWNitr...12
b4 <-b4 [, -1]
average[, 1] <- as.data.frame(colMeans(b4))

#lateral 4251
c4<-q_simn$simulation$LatNitra...9
c4 <-c4 [, -1]
average[, 1] <- as.data.frame(colMeans(c4))

####matrix code####

#matrix for 4255
average1[, 3] <- as.data.frame(colMeans(c1))
average1$X4 <- average1$X1 + average1$X2 + average1$X3
average1$X7 <- q_simn$parameter$values$cn2
average1$X8 <- q_simn$parameter$values$perco
average1$X9 <- q_simn$parameter$values$cn3_swf
average1$X10 <- q_simn$parameter$values$surlag
average1$X11 <- q_simn$parameter$values$nperco
average1$X12 <- q_simn$parameter$values$k
colnames(average1) <- c("SurfNit", "GWNitr", "LatNite","SumOf3","SurfNitr/LatNitr", "GwNitr/LatNitr", "cn2", "perco", "cn3_swf", "surlag", "nperco", "k")
average1$`SurfNitr/LatNitr`<- average1$SurfNit/average1$GWNitr
average1$`GwNitr/LatNitr` <- average1$LatNite / average1$GWNitr

write.csv(average1)
write.csv(average1,"average1.csv")

#matrix for 4259
average2$X4 <- average2$X1 + average2$X2 + average2$X3
average2$X7 <- q_simn$parameter$values$cn2
average2$X8 <- q_simn$parameter$values$perco
average2$X9 <- q_simn$parameter$values$cn3_swf   #new code as of 7/10
average2$X10 <- q_simn$parameter$values$surlag
average2$X11 <- q_simn$parameter$values$nperco
average2$X12 <- q_simn$parameter$values$k        #new code end
colnames(average2) <- c("SurfNit", "GWNitr", "LatNite","SumOf3","SurfNitr/LatNitr", "GwNitr/LatNitr", "cn2", "perco")
average2$`SurfNitr/LatNitr`<- average2$SurfNit/average2$GWNitr
average2$`GwNitr/LatNitr` <- average2$LatNite / average2$GWNitr
average2$X12 <- q_simn$parameter$values$k        #new code end
colnames(average2) <- c("SurfNit", "GWNitr", "LatNite","SumOf3","SurfNitr/LatNitr", "GwNitr/LatNitr", "cn2", "perco", "cn3_swf", "surlag", "nperco", "k")
average2$`SurfNitr/LatNitr`<- average2$SurfNit/average2$GWNitr
average2$`GwNitr/LatNitr` <- average2$LatNite / average2$GWNitr

#matrix for 4263
average3$X4 <- average3$X1 + average3$X2 + average3$X3
average3$X7 <- q_simn$parameter$values$cn2
average3$X8 <- q_simn$parameter$values$perco
average3$X9 <- q_simn$parameter$values$cn3_swf   #new code as of 7/10
average3$X10 <- q_simn$parameter$values$surlag
average3$X11 <- q_simn$parameter$values$nperco
average3$X12 <- q_simn$parameter$values$k        #new code end
average3$X12 <- q_simn$parameter$values$k        #new code end
colnames(average3) <- c("SurfNit", "GWNitr", "LatNite","SumOf3","SurfNitr/LatNitr", "GwNitr/LatNitr", "cn2", "perco",  "cn3_swf", "surlag", "nperco", "k")
average3$`SurfNitr/LatNitr`<- average3$SurfNit/average3$GWNitr
average3$`GwNitr/LatNitr` <- average3$LatNite / average3$GWNitr
View(average3)
write.csv(average3, "average3.csv")


#matrix for 4251
average4[, 3] <- as.data.frame(colMeans(c4))
average4$X4 <- average4$X1 + average4$X2 + average4$X3
average4$X7 <- q_simn$parameter$values$cn2
average4$X8 <- q_simn$parameter$values$perco
average4$X9 <- q_simn$parameter$values$cn3_swf   #new code as of 7/10
average4$X10 <- q_simn$parameter$values$surlag
average4$X11 <- q_simn$parameter$values$nperco
average4$X12 <- q_simn$parameter$values$k        #new code end
colnames(average4) <- c("SurfNit", "GWNitr", "LatNite","SumOf3","SurfNitr/LatNitr", "GwNitr/LatNitr", "cn2", "perco", "cn3_swf", "surlag", "nperco", "k")
average4$`SurfNitr/LatNitr`<- average4$SurfNit/average4$GWNitr
average4$`GwNitr/LatNitr` <- average4$LatNite / average4$GWNitr
View(average4)
write.csv(average4, "average4.csv")


###graphs for visualization




####to delete

x<- q_simn$simulation$LatNitra

x = x %>% separate(date, c("year", "month","day"))


x$year<- as.numeric(x$year)
x$month= NULL 
x$day= NULL 

sapply(x, mode)


annual_avg <- aggregate(. ~ year, x, FUN = sum)