#loading necessary libraries
library(ggplot2)
library(dplyr)
library(forecast)
library(tseries)
library(TTR)
library(xts)

#loading dataset
consumption <- read.csv(file.choose(),dec = ".")

#take a look
head(consumption)
str(consumption)

#Data cleansing
#There is missing value, consists of 1.25% of total dataset
#According to UCI desctiption, consecutive semicolons mean missing values.
#we will replace semicolon value by using previous value
#replace global_active_power
ms_total <- consumption %>%
  select(Date,Time,Global_active_power)

NAs <- ms_total == "?"
is.na(ms_total)[NAs] <- TRUE
ms_total$Global_active_power <-
  na.locf(ms_total$Global_active_power, fromLast = FALSE)

consumption$Global_active_power <- ms_total$Global_active_power

consumption %>% 
  select(Global_active_power) %>%
  filter(Global_active_power == "?")
# na.loc is command to fill missing value. 
#replace sub_meter_1
ms_total <- consumption %>%
  select(Date,Time,Sub_metering_1)

NAs <- ms_total == "?"
is.na(ms_total)[NAs] <- TRUE
ms_total$Sub_metering_1 <-
  na.locf(ms_total$Sub_metering_1, fromLast = FALSE)

consumption$Sub_metering_1 <- ms_total$Sub_metering_1

consumption %>% 
  select(Sub_metering_1) %>%
  filter(Sub_metering_1 == "?")

#replace sub_meter_2
ms_total <- consumption %>%
  select(Date,Time,Sub_metering_2)

NAs <- ms_total == "?"
is.na(ms_total)[NAs] <- TRUE
ms_total$Sub_metering_2 <-
  na.locf(ms_total$Sub_metering_2, fromLast = FALSE)

consumption$Sub_metering_2 <- ms_total$Sub_metering_2

consumption %>% 
  select(Sub_metering_2) %>%
  filter(Sub_metering_2 == "?")
#replace sum_meter_3
ms_total <- consumption %>%
  select(Date,Time,Sub_metering_3)

NAs <- ms_total == "?"
is.na(ms_total)[NAs] <- TRUE
ms_total$Sub_metering_3 <-
  na.locf(ms_total$Sub_metering_3, fromLast = FALSE)

consumption$Sub_metering_3 <- ms_total$Sub_metering_3

consumption %>% 
  select(Sub_metering_3) %>%
  filter(Sub_metering_3 == "?")


#test semicolon value
consumption %>%
  filter(Date == "28/4/2007")#We have deleted missing value


#Adding new columns and edit column names for clearance
#We will pay more attention on active energy consumption,
#Which means we will deleting Voltage,reactive power,intensity
#And we will add other appliance column for reference
consumption$Global_active_power <- as.numeric(levels(consumption$Global_active_power))[consumption$Global_active_power]
consumption$Sub_metering_1 <- as.numeric(levels(consumption$Sub_metering_1))[consumption$Sub_metering_1]
consumption$Sub_metering_2 <- as.numeric(levels(consumption$Sub_metering_2))[consumption$Sub_metering_2]
is.na(consumption)
head(consumption)
consumption$Date <- as.Date(consumption$Date,format = "%d/%m/%Y")
consumption$Time <- as.POSIXct(consumption$Time,format = "%H:%M:%S")

consumption <- consumption %>%
  select(-Global_reactive_power,-Global_intensity,-Voltage) %>%
  mutate(total_power = Global_active_power * 1000/60)

consumption <- consumption %>%
  mutate(other = total_power -Sub_metering_1 -Sub_metering_2 - Sub_metering_3)

colnames(consumption)
consumption <- select(consumption, -Global_active_power)
colnames(consumption)[colnames(consumption) == "Sub_metering_1"] <- "Kitchen"
colnames(consumption)[colnames(consumption) == "Sub_metering_2"] <- "Laundry"
colnames(consumption)[colnames(consumption) == "Sub_metering_3"] <- "Living"
consumption <- select(consumption,Date,Time,total_power,everything())
colnames(consumption)

#Draw 2006-12-16 all day electricity consumption
firstday <- consumption %>%
  filter(Date == "2006-12-16")

str(firstday$Time)
ggplot(firstday,aes(x=Time,y=total_power)) + geom_line()

#get dataset grouped by date.
allyear <- consumption %>%
  group_by(Date) %>%
  summarize_at(.vars = vars(total_power,Kitchen,Laundry,Living,other),
               .funs = c(sum="sum"))
allyear$Living_sum <- as.numeric(allyear$Living_sum)

summary(allyear$total_power_sum)
sd(allyear$total_power_sum)
ggplot(allyear,aes(x=total_power_sum)) + 
  geom_histogram(bins = 1000,colour = "pink") +
  labs(xlab="Total Consumption",ylab = "Frequency",title = "Total electricity Distribution")

#draw plot based on allyear dataset
ggplot(allyear,aes(x=Date,y=total_power_sum)) + geom_line() + stat_smooth(color="red",fill="#FC4E07",method="loess")


#view of allyear
head(allyear)
tail(allyear)
allyear <- allyear[,c(1:6)]
str(allyear)
#convert into ts
#daily ts

#add three days to create complete two years period
allyear_daily <- data.frame(date=seq(max(allyear$Date)+1, max(allyear$Date)+3,'days'), x = new_3days )
new_3days <- matrix(c(29322,2186,227,9271,17638,
                      29322,2186,227,9271,17638,
                      29322,2186,227,9271,17638),ncol = 5,byrow = TRUE)
tail(allyear_daily)
colnames(allyear_daily) <- c('Date','total_power_sum','Kitchen_sum','Laundry_sum','Living_sum','other_sum')
allyear_daily <- rbind(allyear,allyear_daily)
str(allyear_daily)

daily_ts <- ts(allyear_daily$total_power_sum, frequency = 365.25, start=c(2006, 12,16))
plot(daily_ts)
daily_ts_decompose <- decompose(daily_ts)
plot(daily_ts_decompose)

adf.test(daily_ts, alternative = 'stationary')
Pacf(daily_ts,main = "Pacf chart",lag.max = 20)
acf(daily_ts,main = "acf chart",lag.max = 50)
ddd <- unclass(daily_ts)
#select model (3,0,1)
fit <- Arima(daily_ts, order = c(3,0,1))
fit
checkresiduals(fit)


#monthly ts
agg_Month <-aggregate(allyear,by=list(as.yearmon(allyear$Date,"%Y-%m-%D")), FUN=mean, na.rm=TRUE) 
head(agg_Month)
str(agg_Month)

aaa <- ts(agg_Month$total_power_sum, frequency = 12, start=c(2006, 12))
plot(aaa)
bbb <- decompose(aaa)
plot(bbb)
adf.test(aaa,alternative = "stationary")

aaa_diff2 <-diff(aaa,differences=2)  ??????
adf.test(aaa_diff2,alternative = "stationary")

Pacf(aaa_diff2,main = "Pacf chart",lag.max = 20)
acf(aaa_diff2,main = "acf chart",lag.max = 20)

auto.arima(aaa_diff2,seasonal = TRUE)
#selece model (2,2,1)
fit2 <- Arima(aaa_diff2, order = c(2,2,1))
fit2
checkresiduals(fit2)

#use monthly model to predict next one year
forecast_fit2 <- forecast(fit2, h=12, level=c(99.5))
plot(forecast_fit2)