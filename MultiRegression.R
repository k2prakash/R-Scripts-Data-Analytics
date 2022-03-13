library(readxl)
library(GGally)
library(ggplot2)
library(gridExtra)
library(lattice)
mfile <- file.choose()
#mtcars_data <- read.csv(mfile, header = TRUE, sep = ",")
excel_data <- read_excel(mfile, sheet = "CostDriversNonStandardized")
excel_data
names(excel_data) <- gsub(" ", "_", names(excel_data))
head(excel_data)
#pairs(excel_data[,1:6], panel = panel.smooth, upper.panel=NULL)
ggcorr(excel_data[1:8],  hjust = 0.4, size = 2)

library(corrplot)
corrplot(excel_data[1:6], type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

sc1 <- ggplot(excel_data, aes(x=Direct_Labor_Hours, y=Total_Operating_Cost)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title = "Total Operating Cost vs Direct Labor Hours", 
       y = "Operating Costs",
       x = "Labor Hours")


sc2 <- ggplot(excel_data, aes(x=Direct_Labor_Hours, y=New_Menu_Items_Developed)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title = "Total Operating Cost vs Menu Items Developed", 
       y = "Operating Costs",
       x = "Menu Items Developed")


sc3 <- ggplot(excel_data, aes(x=Direct_Labor_Hours,
                            y=Number_of_Deliveries__Customer_Meals)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title = "Total Operating Cost vs Number of Deliveries (customer meals)", 
       y = "Operating Costs",
       x = "Number of Deliveries (Customer Meals)")


sc4 <- ggplot(excel_data, aes(x=Direct_Labor_Hours,
                            y=Number_of_Deliveries__Donated_Meals)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title = "Total Operating Cost vs Number of Deliveries (donated meals)", 
       y = "Operating Costs",
       x = "Number of Deliveries (Donated Meals)")
par(mfrow=c(1, 4))  # divide graph area in 4 columns
plot(sc1)
plot(sc2)
plot(sc3)
plot(sc4)
par(mfrow=c(1, 1))  # divide graph area back to 1 column
library(ggplot2)
pushViewport(viewport(layout = grid.layout(1, 2)))
print(sc1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(sc2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

RegressionResults = lm(Total_Operating_Cost ~  Direct_Labor_Hours +
                                      New_Menu_Items_Developed +
                                      Number_of_Deliveries__Customer_Meals,
                       
                                     data = excel_data)

summary(RegressionResults)
RegressionSummary=summary(RegressionResults)
RegressionSummary$coefficients
plot(RegressionResults)


ggcorr(excel_data[3:7])