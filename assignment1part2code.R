dir = "C:/cygwin64/home/vedan/Code/sta141b/"
source(paste(dir, "/assignment1part2functions.R", sep = ""))
library(tidyverse)

# Make the datatables for the yosemite data

files <- list.files(path="C:/cygwin64/home/vedan/Code/sta141b/Solar1/unzip", pattern="stat")
df_list = lapply(files, getDataframes)
getDataframes(files[1])
yosemite_humid = df_list[[1]][[1]]
yosemite_solar_rad = df_list[[1]][[2]]
yosemite_sky_cov = df_list[[1]][[3]]
yosemite_psych = df_list[[1]][[4]]

# Humidity Data Validation -----------------------------------------------------
humid_class <- class(yosemite_humid)
dim_humid <- dim(yosemite_humid)
humid_classes <- sapply(yosemite_humid, function(x) class(x[c(1:dim(yosemite_humid)[2])]))

# GRAPHS
davis_humid = df_list[[5]][[1]]

# make a long form of the data to see differences between the max and mins across the months
# yes this does negate some of the data manipulation I did in my functions
long_humidity_yosemite <- yosemite_humid %>% pivot_longer(!c("Month", "location"), names_to = "measurement", values_to = "value")

long_humidity_yosemite_no_hour <- long_humidity_yosemite %>% filter(measurement != 'Max_Day') %>% filter(measurement != 'Min_Day') %>% filter(measurement != 'Min_Hour') %>% filter(measurement != 'Max_Hour')

long_humidity_yosemite_no_hour$Month <- as.character(long_humidity_yosemite_no_hour$Month)
long_humidity_yosemite_no_hour$Month  <- factor(long_humidity_yosemite_no_hour$Month , levels=unique(long_humidity_yosemite_no_hour$Month))
long_humidity_yosemite_no_hour$Month <- factor(long_humidity_yosemite_no_hour$Month , levels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))


yosemite_humid_graph_humidity_readings = ggplot(long_humidity_yosemite_no_hour, aes(x = Month,
                                                                                    y = value,
                                                                                    color = measurement, 
                                                                                    group = measurement)) +
  geom_point() +
  geom_line()+
  labs(title = "Yosemite - Monthly Statistics for Relative Humidity",
       x = "Month",
       y = "Percent Humidity", 
       fill = 'Measurement') + 
  theme_minimal() 

yosemite_humid_only_hour <- yosemite_humid[c('Month', 'Max_Hour', 'Min_Hour')]

long_humidity_yosemite_only_hour <- yosemite_humid_only_hour %>% pivot_longer(!c("Month"), names_to = "measurement", values_to = "value")

# Force the x-axis to go from Jan - Dec

long_humidity_yosemite_only_hour$Month <- as.character(long_humidity_yosemite_only_hour$Month)
long_humidity_yosemite_only_hour$Month  <- factor(long_humidity_yosemite_only_hour$Month , levels=unique(long_humidity_yosemite_only_hour$Month))
long_humidity_yosemite_only_hour$Month <- factor(long_humidity_yosemite_only_hour$Month , levels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

yosemite_humid_graph_hour_readings = ggplot(long_humidity_yosemite_only_hour, aes(x = Month,
                                                                                  y = value,
                                                                                  color = measurement, 
                                                                                  group = measurement)) +
  geom_point() +
  geom_line()+
  labs(title = "Yosemite - Average Hour Where the Daily Max and Min Humidity was Observed",
       x = "Month",
       y = "Hour", 
       fill = 'Corresponding Hour') + 
  theme_minimal()

# Solar Data Validation -------------------------------------------------------
class_solar <- class(yosemite_solar_rad)
dim_solar <- dim(yosemite_solar_rad)
solar_classes <- sapply(yosemite_solar_rad, function(x) class(x[c(1:dim(yosemite_solar_rad)[2])]))

long_solar_rad_yosemite <- yosemite_solar_rad %>% pivot_longer(!c("Month", "location"), names_to = "measurement", values_to = "value") %>% filter( measurement != "Direct_Max_Day" )

df = long_solar_rad_yosemite
df$Month <- as.character(df$Month)
df$Month  <- factor(df$Month , levels=unique(df$Month))
df$Month <- factor(df$Month , levels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))


long_solar_rad_yosemite = df

yosemite_solar_rad_graph = ggplot(long_solar_rad_yosemite, aes(x = Month,
                                                               y = value,
                                                               color = measurement, 
                                                               group = measurement)) +
  geom_point() +
  geom_line()+
  labs(title = "Yosemite - Monthly Statistics for Solar Radiation",
       x = "Month",
       y = "Wh/m²", 
       fill = 'Measurement') + 
  theme_minimal() 

# Sky Data Validation ----------------------------------------------------------
class_sky <- class(yosemite_sky_cov)
dim_sky <- dim(yosemite_sky_cov)
sky_classes <- sapply(yosemite_sky_cov, function(x) class(x[c(1:dim(yosemite_sky_cov)[2])]))

davis_sky_cov = df_list[[5]][[3]]

yosemite_davis_sky_cov <- bind_rows(yosemite_sky_cov, davis_sky_cov)

yosemite_davis_sky_cov_graph = ggplot(yosemite_davis_sky_cov, aes(x = month, 
                                                                  y = percent_covered, 
                                                                  color = location)) +
  geom_point() +
  labs(title = "Yosemite: Average Hourly Statistics for Opaque Sky Cover",
       x = "Month",
       y = "Opaque Sky Cover Percentage") + 
  theme_minimal() +
  geom_smooth()

# Psych Data Validation --------------------------------------------------------
class_psych <- class(yosemite_psych)
dim_psych <- dim(yosemite_psych)
psych_classes <- sapply(yosemite_psych, function(x) class(x[c(1:dim(yosemite_psych)[2])]))

long_psych <- yosemite_psych %>% pivot_longer(!c("percent", "location"), names_to = "measurement", values_to = "value")

yosemite_psych_graph = ggplot(yosemite_psych, aes(x = dewpoint,
                                                  y = dry_bulb, 
                                                  color = percent)) +
  geom_point() +
  labs(title = "Yosemite - Monthly Statistics for Solar Radiation",
       x = "Dewpoint",
       y = "Dry Bulb") + 
  theme_minimal() 
