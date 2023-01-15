# code to quickly check O/P from the printed csv file
#open the channel_sd_day file from TxtInOut folder and delete the first line
t<- read.csv("C:/Users/TAPASM21/Desktop/SWATforOutletpoints/T2OP/T2OP/Scenarios/Default/TxtInOut/channel_sd_day.csv")
U<-select(t,mon, day,yr,name,no3_out,flo_out)
#U<-data.table(U)

#keep this space in channel name as it is in the csv file to 
v<- filter(U, name == "cha112          ") 

sapply(v, mode)

v$no3_out<- as.numeric(v$no3_out)
v$flo_out<- as.numeric(v$flo_out)

sapply(v, mode)

v$'Nitrate_simulated'<- v[,5]*0.011574/v[,6]

#changing date formate of v
v$date2 = paste(v$yr, v$mon, v$day, sep = "-")
v$date3<-as.Date(v$date2,format='%Y-%m-%d')
#deleting extra colums
v$mon= NULL
v$day= NULL
v$yr= NULL
v$date2= NULL
v$date=v$date3
v$date3= NULL


#observed nitrate data
q_obs<-read.csv("Nitrate2003to2009.csv")
#q_obs<-read.csv("C:/Users/TAPASM21/Desktop/RSWAT/NitrateSWATR/ObservedData/Nitrate2003to2009.csv")

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
#this line of code merges 2 readings of nitrate on same day with mean of values
z<-group_by(z,date, .add = FALSE, .drop = group_by_drop_default(z)) %>% summarize(Nitrate =mean(Nitrate))
q_obs<- z


df1<- left_join(x=q_obs,y=v,by="date",all.x=TRUE)

NSE(df1$Nitrate_simulated,df1$Nitrate)



#plots
ggplot()+
  geom_line(data=df1,aes(y=Nitrate,x= date, color="black"))+
  geom_line(data=df1,aes(y=Nitrate_simulated,x= date, color="blue"))
                                                   