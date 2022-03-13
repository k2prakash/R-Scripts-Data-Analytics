hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv",
                    sep=",", header = TRUE)
hotdogs
# hotdogs$Dogs.eaten
# barplot(hotdogs$Dogs.eaten)

# Color vector where USA was the winner

#fill_colors <- c()
#for (i in 1:length(hotdogs$Country)){
#  if (hotdogs$Country[i] == "United States") {
#    fill_colors <- c(fill_colors, "#821122")
#  } else {
#    fill_colors <- c(fill_colors, "#cccccc")
#  }
#}

fill_colors <- c()
for (i in 1:length(hotdogs$New.record)){
  if (hotdogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors, "green")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

barplot(hotdogs$Dogs.eaten, 
        names.arg = hotdogs$Year, col = fill_colors,border = NA, 
        xlab = "Year", ylab = "Hot dogs and buns eaten", space = 0.3,
        main = "Nathan's Hot Dogs Eating Contest Results, 1980-2010")
