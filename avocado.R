library(ggplot2)
library(cowplot)
library(dplyr)
library(tibbletime)
library(data.table)
library(tidyr)
library(lubridate)
library(tidyverse)
library(gridExtra) # multiple plots in 1
library(magick) # attach dope image for visual
library(scales) # show the colors
library(ggrepel) # for graph repel (labels
library(naniar) # to check for missing data
library(plyr) #rename 
library(astsa) #time series 
library(forecast) #auto.arima 
library(readxl)

colorsReBu <- c("#922B21", "#EE865D", "#DDCD5E", "#59BEC4", "#048B9F", "#114676")
colorsPuYe <- c("#5C374C", "#985277", "#CE6A85", "#FFCF6A", "#FFB742", "#E9692C")
colorsEarth <- c("#DA4511", "#FFBD00", "#6A953F", "#9A6233", "#D3AE7C", "#307CA1")
colorsRainbow <- c("#FF8EC8", "#FFDF51", "#46DBDF", "#FF8F46", "#42BAB7", "#DB0000")
colorsPastels <- c("#FA6E4F", "#F2CF59", "#FB8E7E", "#C5D7C0", "#8EC9BB", "#F8CA9D")

my_theme <- theme(
  text = element_text(color = "grey35"),
  plot.title = element_text(hjust=0.5),
  axis.title = element_text(size = 15),
  axis.text = element_text(size = 11),
  axis.line = element_line(size = 1.2, color = "grey35"),
  legend.box.background = element_rect(color = "grey75", size = 1),
  legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
  legend.title = element_text(face = "bold"))
  
#==============================Begin Data Analysis =================================================
setwd("~/Desktop/MS EXAM ")
data<- read.csv("~/Desktop/MS EXAM /avocado.csv")
#Delect unwanted column 
data<- data[,-1]
#convert variable'Data' to data formate 
data$Date <- as.POSIXct(strptime(data$Date, format = "%Y-%m-%d"))
#reorder the dataset by Date
df<-data[order(data$Date , decreasing = FALSE ),]


#============================Create conventional and organic dataset =====================
#===Plot to see the differences between two avocado 
levels(df$type)
# Create a Facet Wrap for each product
ggplot(data = df, aes(x = Date, y = AveragePrice, col=type)) +
  geom_line()+
  facet_wrap(~ type) + my_theme + theme(legend.position="bottom")
# Filter by type
organic1 <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "organic")
conventional1 <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "conventional")

##create conventional weekly dataset 
avePrice_con <- aggregate(conventional1[ , "AveragePrice"], by = list(date = conventional1$Date), FUN = mean)
totalVol_con <- aggregate(conventional1[ , "Total.Volume"], by = list(date=conventional1$Date), FUN = sum, na.rm = TRUE)
conventional <- merge(avePrice_con,totalVol_con,by="date")
conventional_weekly<-rename(conventional, c("x.x"="Avg_Price", "x.y"="Total_Volume"))
##create conventional monthly PRICE & Total volume dataset 
con_monthly <- read.csv("~/Desktop/MS EXAM /con_monthly.csv")
conventional_monthly<-con_monthly[-c(40:169),1:3]
conventional_monthly$date <- as.POSIXct(strptime(as.character(conventional_monthly$date), format = "%m/%d/%Y"))

##create organic weekly dataset 
avePrice_org <- aggregate(organic1[ , "AveragePrice"], by = list(date = organic1$Date), FUN = mean)
totalVol_org <- aggregate(organic1[ , "Total.Volume"], by = list(date=organic1$Date), FUN = sum, na.rm = TRUE)
organic <- merge(avePrice_org,totalVol_org,by="date")
organic_weekly<-rename(organic, c("x.x"="Avg_Price", "x.y"="Total_Volume"))
write.csv(conventional_weekly, file = "org_weekly.csv")

##create organic monthly dataset
organic_mon_use1 <- read.csv("~/Desktop/MS EXAM /organic_monthly.csv")
organic_mon_use<-organic_mon_use1[2:3]


##combined dataset
org_plot<-organic_mon_use1[,c(2,3,7)]
org_plot$date <- as.POSIXct(strptime(as.character(org_plot$date), format = "%m/%d/%Y"))
org_plot$type<-c("organic")
names(org_plot)[3] <- "Total_Volume"
con_plot<-conventional_monthly
con_plot$type<-c("conventional")
plot_type <- rbind(org_plot,con_plot)

#===Plot to see the differences between two avocado 
levels(plot_type$type)
# Create a Facet Wrap for each product
ggplot(data = plot_type, aes(x = date, y = Avg_Price, col=type)) +
  geom_line()+
  facet_wrap(~ type) + my_theme + theme(legend.position="bottom")


#==Lag plot 


#=====================Exploratory Data Analysis=====================
#See if there is an inverse relationship between supply and prices. 
options(repr.plot.width=8, repr.plot.height=6)

##Conventional 
#monthly average price
conventional_monthly <- conventional %>%
  ggplot(aes(x=Date, y=AveragePrice)) + 
  geom_line(color=colorsPastels[5]) + 
  theme_minimal()  + 
  my_theme + 
  labs(title="Conventional Avocados") + 
  geom_hline(yintercept=max(conventional$AveragePrice), linetype="dotted", color = "red") + 
  geom_hline(yintercept=min(conventional$AveragePrice), linetype="dotted", color = "red")
#monthly volume 
conventional_volume <- conventional %>%
  ggplot(aes(x=Date, y=Total.Volume)) + 
  geom_bar(stat='identity', fill=colorsPastels[5], color="black") + 
  theme_minimal() + 
  my_theme + 
  geom_smooth(method="loess", color="red")
##Organic 
#monthly average price 
organic_monthly <- organic %>% 
  ggplot(aes(x=Date, y=AveragePrice)) + 
  geom_line(color=colorsPastels[2]) +
  theme_minimal() + 
  my_theme+
  labs(title="Organic Avocados") + 
  geom_hline(yintercept=max(organic$AveragePrice), linetype="dotted", color = "red") + 
  geom_hline(yintercept=min(organic$AveragePrice), linetype="dotted", color = "red")
#monthly volume 
organic_volume <- organic %>%
  ggplot(aes(x=Date, y=Total.Volume)) + 
  geom_bar(stat='identity', fill=colorsPastels[2],color="black") + 
  theme_minimal()  + 
  my_theme + 
  geom_smooth(method="loess", color="red")
plot_grid(conventional_monthly, organic_monthly,conventional_volume, organic_volume, nrow=2, ncol=2)

#==============================Seasonal Pattern? ==========================
seasonal_df <- df
seasonal_df$month_year <- format(as.Date(df$Date), "%Y-%m")
seasonal_df$month <- format(as.Date(df$Date), "%m")
seasonal_df$year <- format(as.Date(df$Date), "%Y")
seasonal_df$monthabb <- sapply(seasonal_df$month, function(x) month.abb[as.numeric(x)])
seasonal_df$monthabb = factor(seasonal_df$monthabb, levels = month.abb)
#=====Monthly Average Price 





#============================Region Analysis =============================











#========================Time Series - Price - conventional ===================================
con_mon<-conventional_monthly[-c(37:39),2]
con_mon<-ts(con_mon)
lag.plot(diff(con_mon), lags=12, diag=T, main="Lag Plot for Conventional Avocados")
plot.ts(con_mon, main="Time Series Plot")

plot.ts(diff(con_mon), main="Time Series Plot for 
Conventional Avocados 
after Linear Differencing")

plot.ts(diff(diff(con_mon),2), main="Conventional Avocados")


#acf2(diff(con_mon),20, main="ACF/PACF with both regular and seasonal differencing")
acf2(diff(diff(con_mon),2),20, main="ACF/PACF with both regular and seasonal differencing")
#acf2(diff(diff(con_mon),12),24, main="ACF/PACF with both regular and seasonal differencing")

#fit1<-sarima(con_mon,2,1,2,0,1,1,4) 
#fit2<-sarima(con_mon,2,1,0,0,1,1,4) 
#fit3<-sarima(con_mon,0,1,2,0,1,1,4) 
fit4<-sarima(con_mon,0,1,0,0,1,1,4) 
fit5<-sarima(con_mon,1,0,1)
fit6<-sarima(con_mon,0,1,0,0,1,1,2) 

fit4

fit6
#===predict - fit4
pred_fit4<-sarima.for(con_mon,3,0,1,0,0,1,1,4)
pred_fit4
all_con_mon<-conventional_monthly$Avg_Price
##overlay actual values on the plot with predictions
lines(37:39, all_con_mon[37:39], type="b", col="blue")
##calculate lower and upper bounds of prediction intervals
lower_con_mon<-pred_fit4$pred - 1.96*pred_fit4$se
upper_con_mon<-pred_fit4$pred + 1.96*pred_fit4$se

##create a table with lower bound, actual value, and upper bound
comp<-cbind(lower_con_mon, all_con_mon[37:39], upper_con_mon)
comp

#===predict - fit6
pred_fit6<-sarima.for(con_mon,3,0,1,0,0,1,1,2)
pred_fit6
all_con_mon<-conventional_monthly$Avg_Price
##overlay actual values on the plot with predictions
lines(37:39, all_con_mon[37:39], type="b", col="blue")
##calculate lower and upper bounds of prediction intervals
lower_con_mon<-pred_fit6$pred - 1.96*pred_fit6$se
upper_con_mon<-pred_fit6$pred + 1.96*pred_fit6$se

##create a table with lower bound, actual value, and upper bound
comp<-cbind(lower_con_mon, all_con_mon[37:39], upper_con_mon)
comp






#========================Time Series - Price - organic ===================================
org<-organic_mon_use[-c(37:39),2]
org_ts<-ts(org)
lorg<-ts(log(org))
lag.plot(diff(org_ts), lags=12, diag=T, main="Lag Plot for Conventional Avocados")
plot.ts(org_ts, main="Time Series Plot")
plot.ts(diff(org_ts), main="Time Series Plot for 
Organic Avocados
after Linear DIfferencing")

plot.ts(diff(org_ts,12), main="Time Series Plot for Seasonal Differenced of Logarithm")
plot.ts(diff(diff(org_ts),4))
plot.ts(diff(diff(org_ts),2), main="Organic Avocados")


acf2(diff(diff(org_ts),2),20, main="ACF/PACF with both regular and seasonal-4 differencing")

#Fit1<-sarima(org_ts,1,1,1,1,1,0,2) 
Fit2<-sarima(org_ts,0,1,1,1,1,0,2)
#Fit3<-sarima(org_ts,1,1,1,1,1,1,4) 
#Fit4<-sarima(org_ts,1,1,1,1,1,1,2) 
Fit5<-sarima(org_ts,0,1,0,2,1,0,4) #***
Fit2
Fit5


#===predict - Fit5
pred_Fit5<-sarima.for(org_ts,3,0,1,0,2,1,0,4)
pred_Fit5
all_org_mon<-organic_mon_use$Avg_Price
##overlay actual values on the plot with predictions
lines(37:39, all_org_mon[37:39], type="b", col="blue")
##calculate lower and upper bounds of prediction intervals
lower_org_mon<-pred_Fit5$pred - 1.96*pred_Fit5$se
upper_org_mon<-pred_Fit5$pred + 1.96*pred_Fit5$se

##create a table with lower bound, actual value, and upper bound
comp_org_Fit5<-cbind(lower_org_mon, all_org_mon[37:39], upper_org_mon)
comp_org_Fit5


#===predict - Fit2
pred_Fit2<-sarima.for(org_ts,3,0,1,1,1,1,0,2)
pred_Fit2
all_org_mon<-organic_mon_use$Avg_Price
##overlay actual values on the plot with predictions
lines(37:39, all_org_mon[37:39], type="b", col="blue")
##calculate lower and upper bounds of prediction intervals
lower_org_mon<-pred_Fit2$pred - 1.96*pred_Fit2$se
upper_org_mon<-pred_Fit2$pred + 1.96*pred_Fit2$se

##create a table with lower bound, actual value, and upper bound
comp_org_Fit2<-cbind(lower_org_mon,all_org_mon[37:39], upper_org_mon)
comp_org_Fit2


