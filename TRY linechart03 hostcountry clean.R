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

sorted_host_country <- host_country_filter[order(host_country_filter$Year), ]

fig <- plot_ly(sorted_host_country, x = ~Year, y = ~medal_count,
               name = 'Host Country', type = 'scatter', 
               mode = 'lines+markers')

fig