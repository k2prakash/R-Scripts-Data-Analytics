# This is an R-Script that will plot a Country's relative score in terms of
# its Ease of Business Rank, The Corruption Index Score, Annual GDP and 
# its Happiness Scale. The 4 variables are encoded in such a manner that
# The X-Values represent a nation's Ease of Business Rank, Y-Values represent
# the country's Corruption Index Score. 

# The GDP of a country is represented by a circle. The Country's GDP is used to 
# calculate the radius of the circle. Since we are plotting the graphs of 
# countries in steps of 500B Dollars, and the Countries in this script have GDPs
# between 500 and 1000 Billion Dollars, the radius is determined by dividing the 
# GDP by 100.

# Lastly, the color of the Circle represents the Happiness Index associated with
# the country. 4 Ranks are grouped into a single color bucket. The ceiling of the
# the Happiness rank when divided by four will determine the color bucket from 
# which the color for the circle is chosen

# Author: Kaushik Prakash:
# Code Version: 10/24/2020: V1

# Specify the libraries that will be used for our script
library(readxl)
library(tidyverse)
library(plotrix)

# Define the color buckets that will be used to indiciate the Happiness Level
happiness_matrix_color_codes <- matrix(c("#FF0000","#FF1100","#FF2300","#FF3400",
                                       "#FF4600","#FF5700","#FF6900","#FF7B00",
                                       "#FF8C00","#FF9E00","#FFAF00","#FFC100",
                                       "#FFD300","#FFE400","#FFF600","#F7FF00",
                                       "#E5FF00","#D4FF00","#E3FDE4","#CCFCCD",
                                       "#B4fBB6","#A4FBA7","#85FA89","#6DF972",
                                       "#0FF517","#08D30E","#07BC0D","#06A40B",
                                       "#058509","#046d07"))

# Store the Data from the Excel Sheet into a Dataframe.
extracted_data <- read_excel("Various_Global_ranks.xlsx")
global_ranks <- as_tibble(extracted_data)

# Separate out the countries that have a GDP > 500 Billion Dollars and
# GDP < 1000 Billion DOllars

gdp_1kplus <- global_ranks %>% filter(`GDP - In Billions of US Dollars` > 100,
                                      `GDP - In Billions of US Dollars` < 1000,
                                      `Corruption Index Score` > 50)

#gdp_1kplus <- global_ranks %>% filter(`GDP - In Billions of US Dollars` > 1000)
# Define the size for the plot 
x <- c(1:120)
y <- c(1:120)

# Space the X-Labels such that they are in increments of 5 till 30.
# Also store the variables in descending order.
xlabels <- sort(seq(0,30,5), decreasing = TRUE)

# Set the Ticks to match the number of labels from the previous step
# Since we have a total of 6 labels when counting from 5 onwards,
# set the ticks to 6, by choosing the X-Plot to be 90 and placing ticks 
# every 15 units to get the 6 markers on which the labels will be placed.
ticks <- seq(0,90,15)

# Plot the working area
plot (x, y, axes = FALSE, type = "n", 
      ylab = "Corruption Index Score:(High score implies Less Corrupt)",
      xlab = "Ease of Business Rank:(Low rank implies more Business Friendly)",
      main = "Plot of Indicators of Countries with Annual GDP between $100 Billion and $1K Billion
      And corruption Index above 50")

# Specifiy the Custom built X-Axis with the ticks and the labels
axis(1, at=ticks, labels = xlabels, las=0) 

# Specifiy the Custom Y-Axis with the ticks
axis(2, seq(0, 90, 10))

countries_data_frame <- data.frame(matrix(,nrow = nrow(gdp_1kplus),
                                          ncol = 4))
for(i in 1:nrow(gdp_1kplus)) {
  country <- gdp_1kplus[i,] # Get the country Row
  country_name_df <- country[,1] # Store the Country name in a dataframe var
  country_name_value <- as.character(country_name_df[1,1]) # Convert the name
                                                           # to a string
  # Store the countrry's Corruption Index in a Dataframe
  country_corrupt_index_df <- country[,8] 
                                        
  # Convert the Country's Corruption Index to a numeric
  country_corrupt_index_value <- as.numeric(country_corrupt_index_df[1,1])
  
  # Store the country's Ease of Business Rank as a dataframe
  country_ease_of_business_df <- country[,2]
  
  # Convert the country's Ease of business rank to a numeric
  country_ease_of_business_value <- as.numeric(country_ease_of_business_df[1,1])
  
  # Store the country's GDP as a dataframe
  country_gdp_df <- country[,4]
  
  # Convert the country's GDP to a numeric
  country_gdp_value <- as.numeric(country_gdp_df[1,1])
  
  # Store the country's Happiness Rank as a dataframe
  country_happiness_rank_df <- country[,5]
  
  # Convert the country's Happiness Rank to a numeric
  country_happiness_rank_value <- as.numeric(country_happiness_rank_df[1,1])
  
  # Create a String Vector to store the Country Name
  text_label_vector <- c(country_name_value)
 
  # Divide the GDP value by 100 & round the value to 3 decimals for the radius  
  radius_gdp <- round((country_gdp_value/100), digits = 3)
  
  # Set the Country's name offset that will be displayed to the right of the 
  # circle.This value is the hal of the radius value for purposes of
  # easier display
  #country_text_offset_value <- (radius_gdp/2)
  country_text_offset_value <- (radius_gdp)
  # Create a String variable that will concatenate the GDP value and letter B
  gdp_text <- paste(as.character(country_gdp_value),"B")
  
  # Store the gdp_text into a vector
  #gdp_label_vector <- c(gdp_text)
  
  # The value for the circle to be placed is based on the Ease of Business
  # Rank of the country. Since we chose the 
  # ranks for the first 30 countries, and our scale is out of 90,
  # we need to multiple the rank by 3. Also since we are displaying the values
  # from right to left, we need to subtract this number by 90, to plot the 
  # circle at the correct X-Axis value.
  x_value <- (90 - (country_ease_of_business_value* 3))
  y_value <- country_corrupt_index_value
  
  # The color index is obtained by dividing the happiness rank by 4 and
  # taking its ceil value. Since lower the rank means a better value and
  # the better (green) colors are at the bottom of the color bucket,
  # subtract 31 with this value to get the correct Happiness Index value.
  color_index <- 31-(ceiling(country_happiness_rank_value/4))
  
  # Obtain the color in Hex from the color bucket based on the index of the 
  # rank obtained
  color_value <- happiness_matrix_color_codes[color_index,]
  
  # Draw a circle with the X-Axis value, Y Axis value, the radius and the 
  # color calculated from the last few steps
  draw.circle(x_value,y_value,radius_gdp,border="black",
              col=color_value,lty=1,lwd=1)
  
  # Place the Country's Name on top of the Circle
  #text(x_value, y_value, labels = text_label_vector, pos=3,
  #     offset = country_text_offset_value)
  
  # Place the Country's GDP value below the Country's name.
  #text(x_value+8, y_value-5, labels = gdp_label_vector, pos=3,
  #     offset = country_text_offset_value, cex = 0.75, col="blue")
  #country_data_frame <-c(x_value,y_value,country_name_value,
  #                       country_text_offset_value)
  
  countries_data_frame[i,1] <- x_value
  countries_data_frame[i,2] <- y_value
  countries_data_frame[i,3] <- country_name_value
  countries_data_frame[i,4] <- country_text_offset_value
  countries_data_frame[i,5] <- gdp_text
  
}

structure(countries_data_frame)
text_pos <- 1

for(i in 1:nrow(countries_data_frame)) {
  if (i == 1){
    x_value <- countries_data_frame[i,1]
    y_value <- countries_data_frame[i,2]
    country_text_offset_value <- countries_data_frame[i,4]
    # Create a String Vector to store the Country Name
    text_label_vector <- c(countries_data_frame[i,3])
    
    # Store the gdp_text into a vector
    gdp_label_vector <- c(countries_data_frame[i,5])
    
    text(x_value, y_value, labels = text_label_vector, pos=text_pos,
         offset = 0, cex = 0.75, font=2)
       
    # Place the Country's GDP value below the Country's name.
    text(x_value, y_value - 5 - country_text_offset_value, 
         labels = gdp_label_vector, pos= text_pos,
         offset = 1, cex = 0.75, col="blue")
  } 
  else {
    x_value <- countries_data_frame[i,1]
    y_value <- countries_data_frame[i,2]
    country_text_offset_value <- countries_data_frame[i,4]
    # Create a String Vector to store the Country Name
    text_label_vector <- c(countries_data_frame[i,3])
    # Store the gdp_text into a vector
    gdp_label_vector <- c(countries_data_frame[i,5])
    vicinity_found <- FALSE
    for (j in 1:i) {
      if (j == i) {
        next
      }
     
      #if ((abs(x_value - countries_data_frame[j,1]) < 5) || 
      #    (abs(y_value - countries_data_frame[j,2]) < 5 )) {
      #  vicinity_found <- TRUE
      #  break
      #} 
      
      if ((abs(x_value - countries_data_frame[j,1]) < 7)) {
        vicinity_found <- TRUE
        break
      } 
      
    }
    print(vicinity_found)
    if (isTRUE(vicinity_found)) {
      random_offset <- i*5
      if ((y_value - random_offset) < 0) {
        random_offset <- i*2
      }
      arrows(x_value,y_value,x_value,y_value-random_offset,length = 0.10,
             lty = 6)
      text_xvalue <- x_value
      text_yvalue <- y_value - random_offset
      text(text_xvalue+1, text_yvalue, labels = text_label_vector, pos=1,
           offset = 0, cex = 0.75, font=2)
      
      # Place the Country's GDP value below the Country's name.
      text(text_xvalue+1, text_yvalue-3, labels = gdp_label_vector, pos= 1,
           offset = 0, cex = 0.75, col="blue")
    }
    else {
      text(x_value, y_value, labels = text_label_vector, pos=text_pos,
           offset = 0, cex = 0.75, font=2)
      
      # Place the Country's GDP value below the Country's name.
      text(x_value, y_value - 3 - country_text_offset_value, 
           labels = gdp_label_vector, pos= text_pos,
           offset = 1, cex = 0.75, col="blue")
    }
    vicinity_found <- FALSE
  }
}


# Specify the region of the plot to be used for the heatmap of the Happiness
# Index
x_left <- 105
x_right <- 110
y_bottom <- 88
y_top <- 90


# Iterate through the color bucket to build the Heatmap
for(i in 1:nrow(happiness_matrix_color_codes)) {
  
  # Get the Color Value in Hex and store it in a variable
  box_color <- happiness_matrix_color_codes[i,]
  
  # Create a rectangle box that with the color from the previous step
  rect(x_left,y_bottom,x_right,y_top,col=box_color, border = 0)
  
  # Decrement the Y Axis values by 2, so that we can continue to create
  # additional colored boxes below
  y_bottom <- y_bottom - 2
  y_top <- y_top - 2
}


# Create the label for the Happiness Scale
happiness_text_vector <- matrix(c("Happiness\nIndex\nScale"))
text(107, 95, labels = happiness_text_vector, pos=3, offset = 0, cex=1,
     font=2 )

# Create a legend for the plot
legend("bottomright", inset=.02, title="GDP Units",
       c("1 radial unit = 100 Billion USD"), horiz=TRUE, cex=0.8)