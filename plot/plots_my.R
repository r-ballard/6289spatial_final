# Load in packages
library(dplyr)
library(ggplot2)
library(ggbreak)
library(sf)
library(dplyr)
library(leaflet)

# Load in NOAA flood data
floods <- read.csv("NOAA_flood.csv")
head(floods)

# Cast date columns as dates
floods$BEGIN_DATE <- as.Date(floods$BEGIN_DATE,"%m/%d/%Y")
class(floods$BEGIN_DATE)

# Extract the years for each flood
floods$year <- as.numeric(format(floods$BEGIN_DATE,"%Y"))

# Aggregate floods by year
yearly_floods <- floods %>% group_by(year) %>% count()
yearly_floods <- data.frame(yearly_floods)
plot(x = yearly_floods$year,y = yearly_floods$n, type = "l")

# Create dataframe of dates with 0 flood events included
data <- data.frame("year" = 1996:2024)
data <- merge(data, yearly_floods, by = "year", all.x = TRUE)
data$n[is.na(data$n)] <- 0

# Get range of x values for rectangle
x_min <- min(data$year - 1)
x_max <- max(data$year + 1)

# Basic plot
ggplot(data, aes(x = year, y = n)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 1) +
  scale_y_break(c(23.9, 50.3)) +
  scale_x_continuous(breaks = min(yearly_floods$year):max(yearly_floods$year)) +
  theme_minimal() +
  geom_rect(aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = 25),
            fill = NA, color = "black", size = 1) +
  geom_rect(aes(xmin = x_min, xmax = x_max, ymin = 50, ymax = 58),
            fill = NA, color = "black", size = 1) +
  labs(x = "Year",
       y = "Number of flood occurrences",
       title = "DC Flood Events by Year") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    panel.grid.major.x = element_line(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.major.y = element_line(),    
    panel.grid.minor.y = element_blank()    
  )

# Trying to load FEMA data
# Load the sf package
library(sf)

# Set the path to the extracted geodatabase or shapefile
# Replace 'path_to_your_data' with the actual path to your data
data_path <- "110001_20241031"  # For geodatabase
# data_path <- "path_to_your_data/your_shapefile.shp"  # For shapefile

# List available layers in the geodatabase
st_layers(data_path)

# Read a specific layer, for example, the Flood Hazard Zones layer
# Replace 'S_Fld_Haz_Ar' with the actual layer name you're interested in
flood_zones <- st_read(dsn = data_path, layer = "S_Fld_Haz_Ar")

# View the first few rows of the data
head(flood_zones)

# Plot the flood zones
plot(st_geometry(flood_zones))

# Adding the flood points
flood_points_df <- data.frame("id" = floods$EVENT_ID,
                              "lat" = floods$BEGIN_LAT,
                              "lon" = floods$BEGIN_LON,
                              "Date" = floods$BEGIN_DATE)
flood_points_df <- na.omit(flood_points_df)
flood_points_sf <- st_as_sf(flood_points_df, coords = c("lon", "lat"), crs = 4326)

# Match CRS of flood zones
flood_points_sf <- st_transform(flood_points_sf, crs = st_crs(flood_zones))

# Check for validity
validity <- st_is_valid(flood_zones)

# If any are FALSE, fix them
flood_zones_fixed <- st_make_valid(flood_zones)

# Perform spatial join to check if point is in a flood zone
floods_with_zones <- st_join(flood_points_sf, flood_zones_fixed, join = st_intersects)

# Create new column to indicate whether its in the flood zone or not
floods_with_zones <- st_join(flood_points_sf, flood_zones_fixed, join = st_intersects, left = TRUE)

# Loading flood zones
flood_zones <- st_read("S_Fld_Haz_Ar.shp")  # adjust if needed
plot(flood_zones["FLD_ZONE"])  # Or "ZONE_SUBTY", depending on FEMA schema

# Make sure points are in same CRS
flood_points_sf <- st_transform(flood_points_sf, st_crs(flood_zones))
flood_points_sf_fixed <- st_make_valid(flood_points_sf)

# Check for validity
validity <- st_is_valid(flood_points_sf_fixed)

# If any are FALSE, fix them
flood_zones_fixed <- st_make_valid(flood_zones)
st_is_valid(flood_zones_fixed)

# Spatial join
floods_with_zones <- st_join(flood_points_sf, flood_zones_fixed, join = st_intersects, left = TRUE)
floods_with_zones_fixed <- st_make_valid(floods_with_zones)

# Assign flood zone status
floods_with_zones_fixed$in_flood_zone <- floods_with_zones$FLD_ZONE %in% c("A", "AE", "VE")  # add more if needed

# Map!
ggplot() +
  geom_sf(data = flood_zones, aes(fill = FLD_ZONE), alpha = 0.4, color = NA) +
  geom_sf(data = floods_with_zones_fixed, aes(color = in_flood_zone), size = 2) +
  scale_color_manual(values = c("black", "red"), labels = c("Outside", "Inside")) +
  labs(title = "DC Flood Events and FEMA Flood Zones", color = "Flood Zone Status") +
  theme_minimal()

# Making an interactive map ####################################################
# Start with summary statistics
# e.g. count number of flood events and mean depth per FLD_ZONE
zone_stats <- floods_with_zones_fixed %>%
  st_join(flood_zones_fixed, left = FALSE) %>%       
  st_drop_geometry() %>%
  group_by(FLD_ZONE.y) %>%
  summarise(
    n_events   = n(),
    mean_depth = mean(DEPTH.x, na.rm = TRUE)
  )

zone_stats$FLD_ZONE <- zone_stats$FLD_ZONE.y

# join those back onto the polygons
flood_zones_stats <- flood_zones %>%
  left_join(zone_stats, by = "FLD_ZONE")

# create a color palette for the zones
pal <- colorFactor("Spectral", domain = flood_zones_stats$FLD_ZONE)

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = flood_zones_stats,
    fillColor   = ~pal(FLD_ZONE),
    fillOpacity = 0.4,
    weight      = 0,
    popup       = ~paste0(
      "<strong>Zone:</strong> ", FLD_ZONE, "<br/>",
      "<strong>Events:</strong> ", n_events, "<br/>",
      "<strong>Mean depth:</strong> ", round(mean_depth,2), " m"
    )
  ) %>%
  addCircleMarkers(
    data  = floods_with_zones_fixed,
    color = ~ifelse(in_flood_zone, "red","black"),
    radius = 5,
    popup  = ~paste0(
      "<strong>Date:</strong> ", Date, "<br/>",
      "<strong>In zone?</strong> ", ifelse(in_flood_zone,"Yes","No")
    )
  )



