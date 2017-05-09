#load required packages
library(readr)
library(dplyr)
library(htmlwidgets)
library(highcharter)
library(RColorBrewer)
library(leaflet)
library(rgdal)
library(dygraphs)
library(quantmod)
library(DT)
library(httr)
library(rvest)
library(geojsonio)

#read gdp data
gdp <- read.csv("2016 GDP per state.csv")

#read nih by state data
nih <- read.csv("2016 NIH funding per state.csv")

#read nih by city data
nih_by_city <- read.csv("2016 NIH funding by city geocoding result.csv")

#read population data
pop <- read.csv("2016 state population.csv")

#get summary of state data
summary(nih_by_city$STATE)

#create subset of nih_by_city for only United States, excluding territories
nih_by_city_clean <- subset(nih_by_city, COUNTRY_1 =="UNITED STATES" & STATE != "AS" & STATE !=  "GU" & STATE != "VI" & STATE != "PR")


#create dissolved nih_by_city_clean to aggregate data to cities
nih_by_city_clean_dissolved <- nih_by_city_clean %>%
  group_by(Match_addr) %>%
  summarize(total_funding = sum(FUNDING, na.rm = TRUE), city = first(City), state = first(STATE), lat = first(lat), lon = first(lon))

#rename state column in nih
colnames(nih)[colnames(nih)=="LOCATION"] <- "State"

#merge gdp/nih datasets with pop based on common field
nih_pop = merge(nih,pop)

#add new field with NIH funding per capita
nih_pop$NIH_perCapita <- nih_pop$FUNDING / nih_pop$Population

#download map data from highcharts
mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))
glimpse(mapdata)

#make map of states with number of NIH awards
hcmap("countries/us/us-all", data = nih_pop, value = "AWARDS",
      joinBy = c("name", "State"), name = "NIH Awards",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(format = '{point.x:,.0f}'))


#make map of states with NIH funding per capita
hcmap("countries/us/us-all", data = nih_pop, value = "NIH_perCapita",
      joinBy = c("name", "State"), name = "2016 NIH funding per capita",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(format = '{point.x:,.0f}'))

#get us geojson data for bubble mapping
data("usgeojson")

#create duplicate column called z for mapping based on funding field
nih_by_city_clean_dissolved$z <- nih_by_city_clean_dissolved$total_funding

#remove first row with no latitude/longitude
nih_by_city_clean_dissolved = nih_by_city_clean_dissolved[-1,]

#make bubble map of NIH funding locations
highchart(type = "map") %>% 
  hc_title(text = "NIH Funding Locations") %>% 
  hc_add_series(mapData =usgeojson, showInLegend = FALSE,
                nullColor = "#D3D3D3", borderColor= '#A0A0A0') %>% 
  hc_add_series(data = nih_by_city_clean_dissolved, type = "mapbubble", name = "Funding Location",  minSize = 1, maxSize = 30, 
                dataLabels = list(enabled = FALSE),
                color = 'rgba(57, 86, 139, 0.5)') %>% 
  hc_tooltip(useHTML = TRUE, headerFormat ="", 
             pointFormat = "<b>{point.Match_addr}:</b> <br> ${point.total_funding:,.0f}") %>%
  hc_mapNavigation(enabled = TRUE)
