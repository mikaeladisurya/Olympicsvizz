library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

# Loading athlete data
host_country <- read.csv(paste0(getwd(),"/DATA/host_country_code.csv"))

# join athlete data with host country codes to add NOC for hosting country
summer_olympic <-  merge(x = summer_olympic, y = host_country, by = "City", all.x = TRUE) # country_code data just the one that intersect

medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Year, Host_IOC) %>%
  summarise(medal_count = sum(!is.na(Medal)))

medal_per_country <- data.frame(medal_per_country)

# Filter the host country for each Year
host_country_filter <- medal_per_country %>%
  filter(NOC==Host_IOC)

x = c(host_country_filter$Year)
y = c(host_country_filter$medal_count)
df_host_country <- data.frame(x, y)

sorted_df <- df_host_country[order(df_host_country$x), ]

fig <- plot_ly(sorted_df, x = ~x, y = ~y,
               name = 'Host Country', type = 'scatter', 
               mode = 'lines+markers')

fig

trace_0 <- rnorm(100, mean = 5)
x <- c(1:100)

data <- data.frame(x, trace_0)

fig <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines+markers')

fig
