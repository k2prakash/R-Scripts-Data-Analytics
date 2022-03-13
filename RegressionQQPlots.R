worldIndicatorsFile <- file.choose()

worldIndicatorsData <- read.csv(worldIndicatorsFile)

myRegression <- lm(CO2.Emissions ~ Energy.Usage, data = worldIndicatorsData)

summary(myRegression)

plot(myRegression)

myRegression$residuals

myRegression$fitted.values

ResidualPlotData <- cbind(myRegression$fitted.values, myRegression$residuals)

write.csv(ResidualPlotData,
          "c:/Users/k2pra/Documents/CIS-541/Week2/Residuals.csv")

QQData <- qqnorm(myRegression$residuals)
write.csv(QQData,
          "c:/Users/k2pra/Documents/CIS-541/Week2/qqdata.csv")