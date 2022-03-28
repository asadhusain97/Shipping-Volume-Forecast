#load library
library(tidyr) #datawrangling
library(ggplot2) #ploting
library(dplyr) #datawrangling
library(zoo) #forecast
library(lubridate) #datetimethings
library(forecast) #forecast
library(imputeTS) #impute


#Custom Functions
data_cleaning = function(filename){
  #read in data
  rh.data <- paste0(filename, ".csv")
  df <- read.csv(rh.data,fileEncoding="latin1")
  colsneeded <- c("BL_AWB_PRO",
                  "container_num",
                  "shipment_id",
                  "origin_city",
                  "pol_city",
                  "pod_city",
                  "bl_city",
                  "Service.Type",
                  "Carrier.Name",
                  "Actual.Cargo.Receipt.Date.Date",
                  "Departed.Date",
                  "Arrived.at.Delivery.Location.Date",
                  "Container.Type",
                  "Actual.Volume..CBM.")
  small_df <- df[colsneeded]
  # Renaming the cols for easy access
  names(small_df) <- c("BL_num",
                       "Container_num",
                       "Shipment_id",
                       "Origin_city",
                       "POL_city",
                       "POD_city",
                       "BL_city",
                       "Service_type",
                       "Carrier",
                       "Cargo_receipt_date",
                       "Departed_date",
                       "Delivery_date",
                       "Container_type",
                       "Volume")
  ## create new lane
  small_df$Lane = paste(small_df$Origin_city,"_",small_df$POL_city,"_",small_df$POD_city,"_",small_df$BL_city)
  
  #combine Vung Tau _ Vung Tau _ Oakland _ Oakland with Vung Tau _ Vung Tau _ Oakland _ Patterson
  small_df$Lane[which(small_df$Lane=="Vung Tau _ Vung Tau _ Oakland _ Oakland")] <- "Vung Tau _ Vung Tau _ Oakland _ Patterson"
  #combine Ningbo _ Ningbo _ Oakland _ Oakland with Ningbo _ Ningbo _ Oakland _ Patterson
  small_df$Lane[which(small_df$Lane=="Ningbo _ Ningbo _ Oakland _ Oakland")] <- "Ningbo _ Ningbo _ Oakland _ Patterson"
  #combine Yantian _ Yantian _ Oakland _ Oakland with Ningbo _ Ningbo _ Oakland _ Patterson
  small_df$Lane[which(small_df$Lane=="Yantian _ Yantian _ Oakland _ Oakland")] <- "Yantian _ Yantian _ Oakland _ Patterson"
  
  ## coerce the categorical column to factor format
  colnames_df <- as.vector(colnames(small_df))
  categorical_vars<-c("BL_num",
                      "Container_num",
                      "Shipment_id",
                      "Origin_city",
                      "POL_city",
                      "POD_city",
                      "BL_city",
                      "Service_type",
                      "Carrier",
                      # "Cargo_receipt_date",
                      # "Departed_date",
                      # "Delivery_date",
                      "Container_type",
                      "Lane")
  small_df[categorical_vars] <- lapply(small_df[categorical_vars], as.factor)
  
  # coerce the Date column to Date format
  date_vars <- c("Cargo_receipt_date",
                 "Departed_date",
                 "Delivery_date")
  small_df[date_vars] <- lapply(small_df[date_vars], function(x) as.Date(x,"%m/%d/%Y"))
  
  # Filtering out rows with 0 volume
  small_df <- filter(small_df, Volume > 50)
  print(small_df)
  str(small_df)
  # Getting month wise grouping
  small_df$Dep_month <- floor_date(small_df$Departed_date, "month")
  
  #group by shipping lanes
  volume_by_lanes <- small_df %>%
    select("Lane","Volume","Dep_month") %>%
    group_by(Lane) %>%
    summarize(total = sum(Volume),
              minyear=min(year(Dep_month)),
              maxyear=max(year(Dep_month)))%>%
    arrange(desc(total))
  #drop lanes that has no shipment at/after 2019
  volume_by_lanes <- volume_by_lanes %>% filter(maxyear >= 2019)

  ##### generate dataframe for each lane
  lane_result = list()
  i=1
  for(laneName in volume_by_lanes$Lane[1:40]){
    laneDf <- small_df%>%
      filter(Lane==laneName)%>%
      select("Lane","Volume","Departed_date","Dep_month")%>%
      arrange(Departed_date)%>%
      group_by(Dep_month)%>%
      summarize(monthly_volume=sum(Volume))
    # generate a sequence of complete months between the start date and end date in the data
    complete_month <- data.frame(seq(laneDf$Dep_month[1], laneDf$Dep_month[nrow(laneDf)], "month"))
    names(complete_month) <- "Dep_month"
    # combine the datasets and reveal the missing values
    complete_laneDf <- complete_month %>% left_join(laneDf, by = "Dep_month")
    # write.csv(complete_laneDf,paste("/Users/yuxuanli/Desktop/备份/spring 2022/Industry Practium/top20lanes_0319/",i,laneName,".csv"))
    lane_result[[laneName]] = complete_laneDf
    plot(complete_laneDf,type="o",main=laneName)
    # print(laneName)
    i=i+1
    
  }
  
  return(lane_result)
}


foralllane = function(name, alllanesfinal){
  
  #read data
  lanedt <- lanedata[[name]]
  
  #getting the start and end years and months of the time series
  mindt <- min(lanedt$Dep_month)
  maxdt <- max(lanedt$Dep_month)
  startts <- c(year(mindt), month(mindt))
  endts <- c(year(maxdt), month(maxdt))
  
  #Impute missing values if any using the mean of the whole time series
  fullts_mean <- ts(na_mean(lanedt$monthly_volume), start = startts,
                    end = endts, freq = 12)
  # without imputation
  fullts_no <- ts(lanedt$monthly_volume, start = startts,
                  end = endts, freq = 12)
  
  #Forecasting with the best model and plotting
  nums_mean <- forecastnplot(fullts_mean, timehorizon = 12, name, mindt, maxdt)
  nums_no <- forecastnplot(fullts_no, timehorizon = 12, name, mindt, maxdt)
  
  if (round(nums_mean[1],2) < round(nums_no[1],2)){
    alllanesfinal[nrow(alllanesfinal)+1,] <- c(name, round(nums_mean[1],2),
                                               round(nums_mean[2],2),
                                               round(nums_mean[3],2),
                                               round(nums_mean[4],2),
                                               round(nums_mean[5],2),
                                               round(nums_mean[6],2),
                                               round(nums_mean[7],2))
  } else{
    alllanesfinal[nrow(alllanesfinal)+1,] <- c(name, round(nums_no[1],2),
                                               round(nums_no[2],2),
                                               round(nums_no[3],2),
                                               round(nums_no[4],2),
                                               round(nums_no[5],2),
                                               round(nums_no[6],2),
                                               round(nums_no[7],2))
  }
  return(alllanesfinal)
}


forecastnplot = function(fullts, timehorizon, nameofplot, mindt, maxdt) {
  
  #defining ets and autoarima functions for tsCV use
  fets <- function(x, h) {
    forecast(ets(x, lambda = 0), h = h)
  }
  farima <- function(x, h) {
    forecast(auto.arima(x, lambda = 0), h = h)
  }
  
  # Compute CV errors for ETS as e1
  e1 <- tsCV(fullts, fets, h = timehorizon)
  # Compute CV errors for ARIMA as e2
  e2 <- tsCV(fullts, farima, h = timehorizon)
  
  # Find MAPE of each model class
  mape1 <- sum(abs(e1)/fullts, na.rm = TRUE)*100/length(e1)
  mape2 <- sum(abs(e2)/fullts, na.rm = TRUE)*100/length(e2)
  
  #For Business Impact Calculation
  startts <- c(year(mindt), month(mindt))
  endts <- c(year(maxdt), month(maxdt))
  last12 <- maxdt %m-% months(12)
  last24 <- maxdt %m-% months(24)
  last24ts <- c(year(last24), month(last24))
  last12ts <- c(year(last12), month(last12))
  
  if (mape1 <= mape2){
    #Model and forecast
    mod <- fullts %>% ets(lambda = 0) 
    fct <- mod %>% forecast(h = 12, level = 0)
    #give options to specify CI
    
    #business impact calc
    current <- window(fullts, start = last24ts, 
                      end = last12ts)
    actual <- window(fullts, start = last12ts, 
                     end = endts)
    new <- window(mod$fitted, start = last12ts, 
                  end = endts)
    currentval <- sum(current, na.rm = TRUE)
    actualval <- sum(actual, na.rm = TRUE)
    newval <- sum(new)
    
    #Getting the contract value and error
    contractval <- sum(fct$mean)
    mincontractval <- sum(fct$lower)
    maxcontractval <- sum(fct$upper)
    mapeval <- mape1
    
    #plotting
    plot(fct, main = nameofplot, xlab = "Years",
         ylab = "Volume (CBM)")
    lines(fct$fitted, col=3)
    lines(fullts, col = "Black")
    
  } else {
    #Model and forecast
    mod <- fullts %>% auto.arima(lambda = 0) 
    fct <- mod %>% forecast(h = 12, level = 0)
    #give options to specify CI
    
    #business impact calc
    current <- window(fullts, start = last24ts, 
                      end = last12ts)
    actual <- window(fullts, start = last12ts, 
                     end = endts)
    new <- window(mod$fitted, start = last12ts, 
                  end = endts)
    currentval <- sum(current, na.rm = TRUE)
    actualval <- sum(actual, na.rm = TRUE)
    newval <- sum(new)
    
    #Getting the contract value
    contractval <- sum(fct$mean)
    mincontractval <- sum(fct$lower)
    maxcontractval <- sum(fct$upper)
    mapeval <- mape2
    
    #plotting
    plot(fct, main = nameofplot, xlab = "Years",
         ylab = "Volume (CBM)")
    lines(fct$fitted, col=3)
    lines(fullts, col = "Black")
  }
  return(c(mapeval, mincontractval, contractval, maxcontractval, currentval, newval, actualval))
}



#run the data_cleaning function
lanedata <- data_cleaning("rh.data0226")

#get the name of all lanes
lnames <- names(lanedata)
##lapply(lanedata, function(x) sum(x[,2], na.rm = TRUE))
##Reduce("+", lsum)


alllanesfinal <- data.frame(Name = character(), MAPE = double(),
                            MinContractVal = double(), ContractVal = double(),
                            MaxContractVal = double(), CurrentPred = double(),
                            NewPred = double(), Actualval = double())


for (x in 7:length(lnames)){
  alllanesfinal <- foralllane(lnames[x], alllanesfinal)
  print(x)
}

View(alllanesfinal)

## Write to csv
write.csv(alllanesfinal,"alllanesfinal_2imputation.csv")




#########ROUGH WORK#########


#read data
lanedt <- lanedata[[lnames[6]]]

# lanedt <- read.csv(" 9 Ningbo _ Ningbo _ Baltimore _ North East .csv")

#getting the start and end years and months of the time series
mindt <- min(lanedt$Dep_month)
maxdt <- max(lanedt$Dep_month)
last12 <- maxdt %m-% months(12)
last24 <- last12 %m-% months(12)
startts <- c(year(mindt), month(mindt))
last24ts <- c(year(last24), month(last24))
last12ts <- c(year(last12), month(last12))
endts <- c(year(maxdt), month(maxdt))

#Impute missing values if any using the mean of the whole time series
fullts <- ts(lanedt$monthly_volume, start = startts,
             end = endts, freq = 12)

#Forecasting with the best model and plotting
fets <- function(full, h) {
  forecast(ets(x, lambda = 0), h = h)
}

farima <- function(x, h) {
  forecast(auto.arima(x, lambda = 0), h=h)
}


# Compute CV errors for ETS as e1
e1 <- tsCV(fullts, fets, h = 12)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(fullts, farima, h = 12)

# Find MAPE of each model class
(mape1 <- sum(abs(e1)/fullts, na.rm = TRUE)*100/length(e1))
(mape2 <- sum(abs(e2)/fullts, na.rm = TRUE)*100/length(e2))

if (mape1 <= mape2){
  #Model and forecast
  mod <- fullts %>% auto.arima(lambda = 0)
  fct <- mod %>% forecast(h = 12, level = 0)
  #give options to specify CI

  #business impact calc
  current <- window(fullts, start = last24ts,
                       end = last12ts)
  actual <- window(fullts, start = last12ts,
                    end = endts)
  new <- window(mod$fitted, start = last12ts,
                 end = endts)
  currentval <- sum(current)
  actualval <- sum(actual)
  newval <- sum(new)


  #Getting the contract value and error
  contractval <- sum(fct$mean)
  mincontractval <- sum(fct$lower)
  maxcontractval <- sum(fct$upper)
  mapeval <- mape1


  #plotting
  plot(fct, main = "asad", xlab = "Years",
       ylab = "Volume (CBM)")
  lines(fct$fitted, col=3)
  lines(fullts, col = "Black")

} else {
  #Model and forecast
  mod <- fullts %>% auto.arima(lambda = 0)
  fct <- mod %>% forecast(h = 12, level = 0)

  #business impact calc
  current <- window(fullts, start = last24ts,
                    end = last12ts)
  actual <- window(fullts, start = last12ts,
                   end = endts)
  new <- window(mod$fitted, start = last12ts,
                end = endts)
  currentval <- sum(current)
  actualval <- sum(actual)
  newval <- sum(new)
  

  #plotting
  plot(fct, main = "asda", xlab = "Years",
       ylab = "Volume (CBM)")
  lines(fct$fitted, col=3)
  lines(fullts, col = "Black")
}


fullts_mean <- ts(na_mean(lanedt$monthly_volume), start = startts,
                  end = endts, freq = 12)
# without imputation
fullts_no <- ts(lanedt$monthly_volume, start = startts,
                end = endts, freq = 12)

name <- lnames[4]
nums_mean <- forecastnplot(fullts_mean, timehorizon = 12, name)
nums_no <- forecastnplot(fullts_no, timehorizon = 12, name)

if (round(nums_mean[1],2) < round(nums_no[1],2)){
  alllanesfinal[nrow(alllanesfinal)+1,] <- c(name, round(nums_mean[1],2),
                                             round(nums_mean[2],2),
                                             round(nums_mean[3],2),
                                             round(nums_mean[4],2),
                                             round(nums_mean[5],2),
                                             round(nums_mean[6],2),
                                             round(nums_mean[7],2))
} else{
  alllanesfinal[nrow(alllanesfinal)+1,] <- c(name, round(nums_no[1],2),
                                             round(nums_no[2],2),
                                             round(nums_no[3],2),
                                             round(nums_no[4],2),
                                             round(nums_no[5],2),
                                             round(nums_no[6],2),
                                             round(nums_no[7],2))
}
