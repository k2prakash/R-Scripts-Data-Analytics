#Load the data from the file
library(ggplot2)
library(dplyr)
library(tidyverse)

crime <- read.csv(
  "c:/Users/k2pra/Documents/CIS-541/Week2/crimeRatesByState2005.csv",
  sep=",", header=TRUE)
# crime
crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]

# Plot all the relationships 
pairs(crime2[,2:9], panel = panel.smooth, upper.panel=NULL)

# Scatter plot of Burglaries vs Murders
df <- select(crime2, c('burglary', 'murder'))
head(df)
g <- ggplot(df, aes(x=murder, y=burglary)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title = "Burglaries vs Murders", 
       y = "Murders per 100,000 population",
       x = "Burglaries per 100,000 population")
plot(g)

# Scatter plot of Motor Vehicle Thefts vs Murders
df <- select(crime2, c('motor_vehicle_theft', 'murder'))
head(df)
g <- ggplot(df, aes(x=murder, y=motor_vehicle_theft)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title = "Motor Vehicle Thefts vs Murders", 
       y = "Murders per 100,000 population",
       x = "Motor Vehicle Thefts per 100,000 population")
plot(g)

# Box Plot of Murders, Burglaries and Motor-Vehicle thefts
df <- select(crime2, c('state', 'murder', 'burglary', 'motor_vehicle_theft'))
head(df)
par(mfrow=c(1, 3))  # divide graph area in 3 columns
boxplot(df$murder, data=df, main="Murders across the Nation",
        ylab= "Murders per 100,000 population",
        col = "light blue")
        #,sub=paste("Outlier rows: ",
        #         boxplot.stats(df$murder)$out))
boxplot(df$burglary, data=df, main="Burglaries across the Nation",
        ylab= "Burglaries per 100,000 population",
        col = "orange")
        #,sub=paste("Outlier rows: ", boxplot.stats(df$burglary)$out))
boxplot(df$motor_vehicle_theft, data=df,
        main="Motor Vehicle Thefts across the Nation",
        ylab= "Motor vehicle Thefts per 100,000 population",
        col = "violet")
        #,sub=paste("Outlier rows: ", 
        #            boxplot.stats(df$motor_vehicle_theft)$out))

barplot(df$murder, 
        names.arg = df$state, col = 'blue',border = NA, 
        xlab = "States", ylab = "Murders per 100,000",
        main = "Plot of Murders across the nation")
