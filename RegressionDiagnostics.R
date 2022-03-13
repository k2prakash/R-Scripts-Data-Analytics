WorldIndicatorsFile=file.choose()

WorldIndicatorsData=read.csv(WorldIndicatorsFile)

myRegression=lm(CO2.Emissions ~ Energy.Usage, data=WorldIndicatorsData)

ResidualPlotData=cbind(myRegression$fitted.values,myRegression$residuals)

write.csv(ResidualPlotData,file.choose())

QQData=qqnorm(myRegression$residuals)

write.csv(QQData,file.choose())