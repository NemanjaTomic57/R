library(ggplot2)
library(tidyr)

scatterplot_01 = function() {
  # Read data
  data <- read.csv2(file = "./winequality-red.csv", sep = ",")
  
  # Create a new column for the ratio
  data$pH_quality_ratio <- as.numeric(data$pH) / as.numeric(data$quality)
  
  # Now plot the histogram of the ratio
  ggplot(data, aes(x = pH_quality_ratio)) +
    geom_histogram(binwidth = 0.01) +
    labs(title = "Alcohol to Quality Ratio in Red Wine",
         x = "Alcohol/Quality Ratio",
         y = "Count")
}

scatterplot_02 = function() {
  x = 1:100
  y = log(x)
  
  x_y = data.frame(x = x, y = y)
  
  plot = ggplot(data = x_y, aes(x = x, y = y)) + 
    geom_point()
  
  print(plot)
}

plot_vadeaths = function() {
  vadeaths = as.data.frame(VADeaths)
  vadeaths$AgeGroup = rownames(vadeaths)
  
  # Bereinige die Daten mit tidyr, um lediglich drei Spalten 'Age Group', 'Area + Gender' und 'Deaths zu haben
  vadeaths_long = pivot_longer(vadeaths, cols = -AgeGroup, names_to = "Area + Gender", values_to = "Deaths")
  
  plot = ggplot(vadeaths_long, aes(x = AgeGroup, y = Deaths)) + 
    geom_col(fill = "orange") +
    theme_minimal() +
    labs(title = "VA Deaths by Age Group and Area + Gender",
         x = "Age Group",
         y = "Deaths per 1000")
  
  print(plot)
}

plot_kpi_data = function() {
  kpi_data = read.csv2("./kpi_data.csv", sep=",")
  
  # Convert to numeric for correct plotting
  kpi_data$Quantity = as.numeric(kpi_data$Quantity)
  kpi_data$Unit_Price = as.numeric(kpi_data$Unit_Price)
  
  filtered_data = kpi_data[, c("Quantity", "Unit_Price")]
  
  plot = ggplot(filtered_data, aes(x = Quantity, y = Unit_Price)) +
    geom_point() +
    labs(title = "Quantity vs Unit Price", x = "Quantity", y = "Unit Price") +
    theme_minimal()
  
  print(plot)
}

read_and_edit_csv = function() {
  customer_data = read.csv2(url('https://gitlab.com/DBEAcademy/data-science-demo/-/raw/main/beispiel_daten.csv'), sep = ',')
  
  customer_data$Land[customer_data$Land == 'USA'] = 'Vereinigte Staaten'
  
  write.csv2(customer_data, 'customer_data.csv')
}

read_and_edit_csv()
