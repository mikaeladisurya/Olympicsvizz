library(plotly)

# Loading athlete data
host_country <- read.csv(paste0(getwd(),"/DATA/host_medal.csv"))

# Add a new column with the sum of gold_as_host and gold_b_host
host_country$medal_diff <- host_country$medal_as_host - host_country$medal_b_host

fig <- plot_ly(host_country, x = ~Year, y = ~medal_diff,
               name = 'Difference of Medals', type = 'scatter', 
               mode = 'lines+markers', text = ~paste("NOC: ", NOC))

fig
