# Install and load necessary libraries
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

# Create an example data frame with two levels of hierarchy
data <- data.frame(
  Category = rep(c("A", "B", "C"), each = 6),
  Subcategory = rep(1:6, 3),
  Value = c(15, 25, 40, 12, 8, 5, 30, 20, 15, 40, 20, 10, 10, 20, 30, 25, 15, 30)
)

# Convert the data frame to a format suitable for plotly
data$Subcategory <- paste(data$Category, data$Subcategory, sep = "-")
data <- data[, c("Category", "Subcategory", "Value")]
colnames(data) <- c("parent", "id", "value")

# Create an interactive sunburst chart using plotly
interactive_sunburst <- plot_ly(data,
                                type = "sunburst",
                                ids = data$id,
                                parents = data$parent,
                                values = data$value,
                                labels = data$id,
                                branchvalues = "total",
                                hovertext = data$value) %>%
  layout(title = "Interactive Sunburst Chart with Drilldown",
         margin = list(l = 0, r = 0, b = 0, t = 50))

# Display the interactive sunburst chart with drilldown
interactive_sunburst
