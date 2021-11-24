# Puerto Rico Migration Flows, 2011-2018
# US Census Migration Flows API
# May 3, 2021
# By: Matthew Martinez

library(tidyverse)
library(tidycensus)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(plotly)

# Loop for grabbing 2018 PR migration data - using this to get geospatial data
centroidsFull <- NULL

for (i in 2018){
  centroids <- get_flows(
    geography="county",
    state="Puerto Rico",
    year = i,
    geometry = TRUE
  )
  
  centroids <- centroids %>% 
  mutate(Year = i)
  centroidsFull <- rbind(centroidsFull, centroids)
}

# Turning into DF object
centroidsFullDf <- as.data.frame(centroidsFull)

# making two new geographic identifier columns - county and state
centroidsFullDf$Location <- str_split(centroidsFullDf$FULL2_NAME, ", ") %>% sapply("[", 1)
centroidsFullDf$State <- str_split(centroidsFullDf$FULL2_NAME, ", ") %>% sapply("[", 2)

# splitting up the lon and lat from the centroid variable
centroidsFullDf$Long <- str_split(centroidsFullDf$centroid2, ", ") %>% sapply("[", 1)
centroidsFullDf$Lat <- str_split(centroidsFullDf$centroid2, ", ") %>% sapply("[", 2)

# string clean up on lon and lat
centroidsFullDf$Long <- as.numeric(str_sub(centroidsFullDf$Long, start=3))
centroidsFullDf$Lat <- as.numeric(str_sub(centroidsFullDf$Lat, end=-2))

# 
centroidsFullDf <- centroidsFullDf %>%
  filter(is.na(State) == FALSE) %>%
  select(Location, State, GEOID2, FULL2_NAME, Lat, Long)

# removing 
centroidsFullDf <- centroidsFullDf %>% distinct()

# Loop for grabbing 2011-2018 PR migration data
prFlowsFull <- NULL

for (i in 2018){
  prFlows <- get_flows(
    geography="county",
    state="Puerto Rico",
    year = i
  )
  
  prFlows <- prFlows %>% 
    mutate(Year = i)
  prFlowsFull <- rbind(prFlowsFull, prFlows)
}

prFlowsFullDf <- prFlowsFull

# using dplyr to group by and sum up number of migrants by location
prFlowsFullDf <- prFlowsFullDf %>%
  filter(is.na(GEOID2) == FALSE & 
           variable == "MOVEDOUT") # %>% # only interested in number of individuals who have MOVED IN to a location
  #group_by(GEOID2) %>% summarise(estimate = sum(estimate, na.rm=TRUE))

# merging geospatial data to the migration total data by GEOID2
prFlowsMerge <- merge(prFlowsFullDf, centroidsFullDf, by="GEOID2", all.x=TRUE)

# removing intra puerto rico migration (only concerned about migration totals to the US outside of PR)
# Removing places where the totals are 0
# only working with counties (some locations are cities and overlap with counties) so restricting GEOID2 to 5 characters (county FIPS)
prFlowsMerge <- prFlowsMerge %>% filter(is.na(FULL2_NAME) == FALSE & 
                                          State != "Puerto Rico" &
                                          estimate != 0 & 
                                          nchar(GEOID2) < 6)


# Using Leaflet to make a bubble map
# This map only shows locations where PR migrants moved to
popup_map <- paste0("<b>Number of migrants from Puerto Rico, 2014-2018</b> <br>",
                    "Location: ", prFlowsMerge$Location, ", ", prFlowsMerge$State, "<br>",
                           "Estimate: ", prFlowsMerge$estimate)

map <- leaflet(prFlowsMerge) %>% 
  setView(-98.16367158811981, 40.46473324068266, zoom=4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircles(lng = ~Long, 
             lat = ~Lat, 
             weight = 1,
             radius = ~sqrt(estimate) * 500, 
             popup = popup_map,
             color = "#007ec2",
             fillColor = "#007ec2",
             stroke = FALSE, 
             fillOpacity = 0.5
             )

map

# bubble map with Plotly looks nice
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  showlakes = TRUE,
  lakecolor = toRGB('white'),
  landcolor = "#d1dce0", #DBE9EE #DAE5ED
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig <- plot_geo(prFlowsMerge, 
                locationmode = 'USA-states', 
                sizes = c(2, 1500),
                marker = list(color = '#007ec2', 
                              opacity=.4, 
                              line = list(width = 0, 
                                          color = '#007ec2')))

fig <- fig %>% add_markers(
  x = ~Long, 
  y = ~Lat, 
  size = ~estimate, 
  hoverinfo = "text", 
  hoverlabel=list(bgcolor="#FFFFFF"),
  text = ~paste0(#"<b>Migrants from Puerto Rico, 2010-2018</b> <br>",
                 "<b>", prFlowsMerge$Location, ", ", prFlowsMerge$State, "</b> <br>",
                 "<b>Estimate: </b>", formatC(prFlowsMerge$estimate, big.mark=","))) %>% 
layout(geo = g, 
       showlegend=FALSE, 
       title="Puerto Rican Migration To Mainland United States <br /> Totals By County, 2014-2018",
       font="Arial") %>% 
  config(displayModeBar = F,
         scrollZoom=TRUE)

fig



quantile(prFlowsMerge$estimate, probs = seq(0, 1, 0.20))

mean(prFlowsMerge$estimate)
sd(prFlowsMerge$estimate)

prFlowsMerge$estimateCat[prFlowsMerge$estimate <= 9] <- "Very Low"
prFlowsMerge$estimateCat[prFlowsMerge$estimate > 9 & prFlowsMerge$estimate <= 24] <- "Low"
prFlowsMerge$estimateCat[prFlowsMerge$estimate > 24 & prFlowsMerge$estimate <= 61.4] <- "Moderate"
prFlowsMerge$estimateCat[prFlowsMerge$estimate > 61.4] <- "High"
prFlowsMerge$estimateCat[prFlowsMerge$estimate > 156.6] <- "Very High"

prFlowsMerge$estimateCat <- factor(prFlowsMerge$estimateCat, levels=c("Very Low","Low","Moderate","High", "Very High"), ordered=T)


fig2 <- plot_geo(prFlowsMerge, lat = ~Lat, lon = ~Long)
fig2 <- fig2 %>% add_markers(
  text = ~paste0("<b>", prFlowsMerge$Location, ", ", prFlowsMerge$State, "</b> <br>",
                 "<b>Estimate: </b>", formatC(prFlowsMerge$estimate, big.mark=",")),
  color = ~estimateCat,
  colors = c("#fee0d2", "#fc9272","#ef3b2c", "#a50f15", "#67000d"),
  symbol = I("circle"), 
  size = I(30), 
  hoverinfo = "text"
)
fig2 <- fig2 %>% layout(
  title = 'Puerto Rican Migration To Mainland United States<br /> Totals By County, 2014-2018', 
  geo = g,
  legend=list(title=list(text='<b> Migration Level </b>'),
              y=.001,
              x=.5,
              xanchor="center",
              orientation="h")
)

fig2