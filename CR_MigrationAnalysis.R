#****************************************************************************************************************************************************
# R code to estimate migration difficulty (river km, elevation) for Copper River sockeye using a combination of river network, 
# aerial survey results (example here is from 2005 NVE study), and digital elevation data.
# Program estimates metrics for all the main spawning groups of CR sockeye

#Created by Pete Rand, PWSSC

#****************************************************************************************************************************************************

#Chunk H:

#Required packages
library(sf)
library(elevatr)
library(tmap)
library(raster)
library(plyr)
library(readxl)
library(lubridate)
library(tmap)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(dplyr)
library(classInt)

#Produce map and plot tagging location and ground receiver locations

setwd("~/InputFiles/CR_ShapeFiles/")

#Read in shape files
CopperT<-sf::read_sf("CopperRTribs.shp")
class(CopperT)
#watershed boundary
CopperWB<-sf::read_sf("CopperRiverWBD.shp")

#convert to coordinates
CopperT = st_transform(CopperT, "+init=epsg:4326")
CopperWB = st_transform(CopperWB, "+init=epsg:4326")

#Read in elevation data
dem_Copper<-get_elev_raster(CopperWB,,z=7)
elev_crop = crop(dem_Copper, CopperWB)

slope = terrain(elev_crop, opt = "slope")
aspect = terrain(elev_crop, opt = "aspect")

hill = hillShade(slope, aspect, angle = 40, direction = 270)
masked_CR <- mask(hill, CopperWB)

#River network
river_sf = st_as_sf(CopperT)

# Ensure CRS consistency between CopperT and elev_crop
CT <- st_transform(CopperT, crs(elev_crop))  # Transform CopperT to the CRS of the raster

# Extract coordinates (longitude and latitude) from CopperT
coordinates_xy <- st_coordinates(CT)[, 1:2]  # Keep only the first two columns (longitude and latitude)

# Extract elevation values for each point from the raster
elevation_values <- extract(elev_crop, coordinates_xy)

# Create a data frame with x, y coordinates and corresponding elevation values (in meters ASL)
elevation_df <- data.frame(
  x = coordinates_xy[, 1],  # Longitude
  y = coordinates_xy[, 2],  # Latitude
  elevation = unlist(elevation_values)  # Elevation, unlisted in case it's a list
)

# View the result
head(elevation_df)

setwd("~/PlaceNames/")

Copper_PlaceNames<-read.csv(file="Copper_PlaceNames.csv")
Copper_PlaceNames<-Copper_PlaceNames[c(-5,-6),]
placenames_sf = st_as_sf (Copper_PlaceNames,coords = c("Longitude","Latitude"),crs = 4326, agr = "constant")

#Read in receiver station positions
Receiver_Loc<-read.csv(file="C:/Users/PeteRand/OneDrive - Prince William Sound Science Center/RWorkingDirectory/CopperTelemetry/Receiver_GPS_Locations/Receiver_Loc_For2005.csv")
receivers_sf = st_as_sf (Receiver_Loc,coords = c("Longitude","Latitude"),crs = 4326, agr = "constant")

#This produces map with ground receiver telemetry stations as red points.

map_CR = tm_shape(masked_CR) +
  tm_raster(palette = gray(7:10 / 10), style = "cont", legend.show = FALSE) +tm_shape(river_sf)+tm_lines(col = "blue") +
  tm_compass(position = c("right", "top"), size = 2) +tm_graticules(x=c(-146,-142),y=c(61,62,63),labels.size=1.2)+
  tm_scale_bar(position = c("right", "bottom"), width = 0.15, text.size =.6)+
  tm_shape(placenames_sf)+ tm_symbols(col = "black", size = .7, scale = .8,border.lwd=NA)+
  tm_text("Site",just="left",scale=.8,xmod=.1,ymod=-.3)+
  tm_shape(receivers_sf)+ tm_symbols(col = "red", size = .7, scale = .8,border.lwd=NA)+
  tm_text("Site.",just="left",scale=.8,xmod=.1,ymod=-.3)+
  tm_xlab("Longitude", size = 1.5, rotation = 0, space = 0)+
  tm_ylab("Latitude", size = 1.5, rotation = 90, space = 0)


map_CR


#Read in and plot aerial data
setwd("~/InputFiles/AerialDetections_2005/")

# Initialize an empty list to store the dataframes
survey_data <- list()

# List of file names
file_names <- c(
  "aerial Sept 20-21 and 28.csv",
  "aerial0805-07 and 31.csv",
  "aerial0902-03.csv",
  "aerial0928 and 30.csv",
  "aerial070705.csv",
  "aerial070905.csv",
  "aerial080105.csv",
  "aerial090105.csv"
)

# Loop over the file names to read in the data
for (file_name in file_names) {
  file_path <- file.path(getwd(), "Aerial_Recx_2005", file_name)
  survey_df <- read.csv(file_path)
  # Conditionally remove columns 12 to 15 for the specific file
  if (file_name == "aerial0805-07 and 31.csv") {
    survey_df <- survey_df[, -c(12:15)]
  }
  survey_data[[file_name]] <- survey_df
}

# Combine all dataframes into a single dataframe
All_Fish_Air <- do.call(rbind, survey_data)

#Need to clean up with dplyr

All_Fish_Air<-ddply(All_Fish_Air,.(Fr,Code,Day),summarise,Latitude=mean(Latitude),Longitude=mean(Longitude),Sig=mean(Sig))


# Remove right-most digit "0" for codes that have 2 or more digits
All_Fish_Air$Code <- ifelse(nchar(All_Fish_Air$Code) > 1 & substr(All_Fish_Air$Code, nchar(All_Fish_Air$Code), nchar(All_Fish_Air$Code)) == "0",
                            as.integer(substr(All_Fish_Air$Code, 1, nchar(All_Fish_Air$Code)-1)),
                            All_Fish_Air$Code)

# Remove "3" from codes that have two or more digits and have a "3" in the right most position (should consult with ATS on this - this seems flakey)
All_Fish_Air$Code <- ifelse(nchar(All_Fish_Air$Code) > 1 & substr(All_Fish_Air$Code, nchar(All_Fish_Air$Code), nchar(All_Fish_Air$Code)) == "3",
                            as.integer(substr(All_Fish_Air$Code, 1, nchar(All_Fish_Air$Code)-1)),
                            All_Fish_Air$Code)

# Filter out any remaining codes that exceed 100 (one occurrence)
All_Fish_Air <- All_Fish_Air[All_Fish_Air$Code <= 100, ]

colnames(All_Fish_Air)<-c("TagF2","TagCode","Day","Latitude","Longitude","Sig")

#Fix frequencies
All_Fish_Air$TagF2<-sub('.*(\\d{3}).*', '\\1',All_Fish_Air$TagF2)
All_Fish_Air$TagF2<-as.numeric(All_Fish_Air$TagF2)

#Need to generate TagID
All_Fish_Air<- All_Fish_Air %>%
  mutate(TagID = paste0(TagF2, "_", TagCode))


#Read in tagging data and process it

#This is file (worksheet in a spreadsheet) shared by Matt Piche in early 2024 for FCA project

Tag<-read_excel("~/InputFiles/TaggingData/SRT TFA_19Mar2006.xls", sheet = "Tag fates")

#Deal with dates and times
Tag$TAG_DATE<-as.POSIXct(Tag$TAG_DATE,format="%m/%d/%y")
Tag$TAG_DATE<-as.POSIXct(Tag$TAG_DATE,format="%j")
Tag$DOY_Tag<-format(Tag$TAG_DATE,"%j")
Tag$DOY_Tag<-as.numeric(Tag$DOY_Tag)
Tag$Year<-as.POSIXct(Tag$TAG_DATE,format="%y")
Tag$Year<-format(Tag$TAG_DATE,"%Y")
Tag$TAG_TIME<-hms(Tag$TAG_TIME)
#Rename column headers so join is possible
colnames(Tag)[4]  <- "TagF2"
colnames(Tag)[5]  <- "TagCode"
#Create TagID code
Tag<- Tag %>%
  mutate(TagID = paste0(TagF2, "_", TagCode))

#Now generate TAG_NO based on TagID and time of flight

All_Fish_Air <- All_Fish_Air %>%
  left_join(Tag %>% select(TagID, DOY_Tag, DISAB_DATE, TAG_NO), 
            by = "TagID")
# Format DISAB_DATE and calculate DOY_Dis, handling NA values
All_Fish_Air$DOY_Dis <- ifelse(is.na(All_Fish_Air$DISAB_DATE), NA, as.numeric(format(All_Fish_Air$DISAB_DATE,"%j")))

All_Fish_Air <- All_Fish_Air %>%
  mutate(TAG_NO = ifelse(Day >= DOY_Tag & 
                           (is.na(DOY_Dis) | Day <= DOY_Dis), 
                         as.character(TAG_NO), 
                         NA_character_))

All_Fish_Air$TAG_NO<-as.numeric(All_Fish_Air$TAG_NO)

# Filter out rows with NA in TAG_NO field
All_Fish_Air <- All_Fish_Air[!is.na(All_Fish_Air$TAG_NO), ]

#Create sf object for plotting in ggplot
fish_sf = st_as_sf(All_Fish_Air, coords = c("Longitude", "Latitude"), 
                   crs = 4326, agr = "constant")

#Option here to plot location of last detection from the air

#Deal with dups, selected last detection in tribs
fish_sf<-fish_sf %>% 
  group_by(TAG_NO) %>% 
  filter(Day==max(Day))

#Produce map with plot of radio tag locations from aerial survey
map_CR = tm_shape(masked_CR) +
  tm_raster(palette = gray(7:10 / 10), style = "cont", legend.show = FALSE) +tm_shape(river_sf)+tm_lines(col = "blue") +
  tm_compass(position = c("right", "top"), size = 2) +tm_graticules(x=c(-146,-142),y=c(61,62,63),labels.size=1.2)+
  tm_scale_bar(position = c("right", "bottom"), width = 0.15, text.size =.6)+
  tm_shape(placenames_sf)+ tm_symbols(col = "black", size = .7, scale = .8,border.lwd=NA)+
  tm_text("Site",just="left",scale=.8,xmod=.3,ymod=-.3)+
  tm_shape(fish_sf) +
  tm_symbols(col = "red", size = .7, scale = .3,border.lwd=NA)+ tm_grid(labels.size=0.7,alpha=.1) +
  tm_xlab("Longitude", size = 1.5, rotation = 0, space = 0)+
  tm_ylab("Latitude", size = 1.5, rotation = 90, space = 0)

map_CR

#Snap aerial survey to stream network (this code chunk takes a minute or two to run)

# Initialize an empty list to collect results
results <- vector("list", nrow(fish_sf))

# Initialize a vector to store the names of nearest river segments
nearest_segment_names <- character(nrow(fish_sf))

# Initialize a vector to store AWC codes for nearest river segments
nearest_segment_AWC <- character(nrow(fish_sf))

# Iterate through each row in fish_sf
for (i in seq_len(nrow(fish_sf))) {
  # Extract a single row
  test_row <- fish_sf[i, ]
  
  # Find the nearest river segment
  nearest_segment <- river_sf[st_nearest_feature(test_row$geometry, river_sf), ]
  
  # Extract the 'NAME' of the nearest river segment and store it
  nearest_segment_names[i] <- st_drop_geometry(nearest_segment) %>% pull(NAME)
  
  # Extract the 'STREAM_COD' of the nearest river segment and store it
  nearest_segment_AWC[i] <- st_drop_geometry(nearest_segment) %>% pull(STREAM_COD)
  
  # Get the line to the nearest segment
  line_to_point <- st_nearest_points(test_row$geometry, st_geometry(nearest_segment))
  
  # Safely extract the closest point
  extracted_points <- st_cast(line_to_point, "POINT")
  closest_point <- if (length(extracted_points) >= 2) extracted_points[2] else st_point(c(NA, NA))
  
  # Calculate the distance
  distance <- if (!st_is_empty(closest_point)) st_distance(test_row$geometry, closest_point) else NA
  
  # Determine whether to snap to the nearest point
  snapped_geometry <- if (!is.na(distance) && as.numeric(distance) <= 400) {
    closest_point
  } else {
    test_row$geometry
  }
  
  # Store results as a list (snapped geometry, distance, and segment name)
  results[[i]] <- tibble(
    geometry = snapped_geometry,
    distance = as.numeric(distance),
    nearest_segment_name = nearest_segment_names[i],
    nearest_segment_AWC = nearest_segment_AWC[i]# Store the river segment name
  )
}

# Combine results into a single sf object, keeping the geometry, distance, and nearest segment name columns
results_sf <- do.call(rbind, lapply(results, function(res) st_sf(res, crs = st_crs(fish_sf))))

#Trim AWC to subwatershed group
results_sf$nearest_segment_SubwatershedCOD <- substr(results_sf$nearest_segment_AWC,1,17)

# Add the snapped geometry, distance, and river segment name back into the fish_sf dataset
closest_points_2005 <- cbind(fish_sf, results_sf)

# Convert to sf object if needed
closest_points_2005 <- st_sf(closest_points_2005, crs = st_crs(fish_sf))

# Print the first few rows of the result to check
head(closest_points_2005)

# Filter for locations not on the "Copper River"
closest_points_tribs_2005 <- closest_points_2005 %>%
  filter(nearest_segment_name != "Copper River")

#Plot locations of tagged fish that are in tribs (not mainstem) locations
map_CR = tm_shape(masked_CR) +
  tm_raster(palette = gray(7:10 / 10), style = "cont", legend.show = FALSE) +tm_shape(river_sf)+tm_lines(col = "blue") +
  tm_compass(position = c("right", "top"), size = 2) +tm_graticules(x=c(-146,-142),y=c(61,62,63),labels.size=1.2)+
  tm_scale_bar(position = c("right", "bottom"), width = 0.15, text.size =.6)+
  tm_shape(placenames_sf)+ tm_symbols(col = "black", size = .7, scale = .8,border.lwd=NA)+
  tm_text("Site",just="left",scale=.8,xmod=.3,ymod=-.3)+
  tm_shape(closest_points_tribs_2005) +
  tm_symbols(col = "red", size = .7, scale = .3,border.lwd=NA)+ tm_grid(labels.size=0.7,alpha=.1) +
  tm_xlab("Longitude", size = 1.5, rotation = 0, space = 0)+
  tm_ylab("Latitude", size = 1.5, rotation = 90, space = 0)

map_CR

#Estimate distance traveled for each fish that are located in tributaries
# Step 1: Create the river mouth point using the provided coordinates (location here is Baird tagging site - might want to use lower river site, tagging site is about 70 km from flats)
river_mouth_coords <- st_sfc(st_point(c(-144.513210, 60.816209)), crs = 4326)

# Step 2: Assign this as the river mouth for distance calculations
mouth_of_river <- st_sf(geometry = river_mouth_coords)

# Verify the river mouth point
print(mouth_of_river)

# Step 3: Calculate the river distance for each fish
fish_distances <- closest_points_tribs_2005 %>%
  mutate(
    # Calculate distance from fish location to the river mouth point
    river_distance = st_distance(geometry, mouth_of_river$geometry)
  )

# View the calculated distances
head(fish_distances)

#Create Air_Tags df that contains relevant data (tag IDs, dates and locations)

Air_Tags <- closest_points_2005 %>%
  #  mutate(SpawnG_Name = nearest_segment$NAME,River_AWC_Code = nearest_segment$STREAM_COD,Subwatershed_AWC = substr(nearest_segment$STREAM_COD,1,17)) %>%
  mutate(SpawnG_Name = nearest_segment_name,SpawnG_AWC = nearest_segment_AWC,SpawnG_SubwatershedCOD = nearest_segment_SubwatershedCOD) %>%
  st_set_geometry(NULL) %>%
  #  select(TAG_NO, Day, SpawnG_Name,River_AWC_Code,Subwatershed_AWC) %>%
  select(TAG_NO, Day, SpawnG_Name,SpawnG_AWC,SpawnG_SubwatershedCOD) %>%
  as.data.frame()

# Crosswalk between name of watersheds and AWC codes 
Klutina <- c("212-20-10080-2401")
Upper_Copper <- c("212-20-10080-2605")
Gulkana <- c("212-20-10080-2461")
Chitina <- c("212-20-10080-2300")
Tonsina <- c("212-20-10080-2331")
Tazlina <- c("212-20-10080-2431")
Lower_Copper <- c("212-20-10080-2100", "212-20-10080-2159") #This includes Bremner and Tasnuna

# Create a look up table
lookup_table_awc_watersheds <- data.frame(
  Tributary = c(rep("Klutina", length(Klutina)), 
                rep("Upper Copper", length(Upper_Copper)), 
                rep("Gulkana", length(Gulkana)), 
                rep("Chitina", length(Chitina)), 
                rep("Tonsina", length(Tonsina)), 
                rep("Tazlina", length(Tazlina)), 
                rep("Lower Copper", length(Lower_Copper))),
  Segment_Code = c(Klutina, Upper_Copper, Gulkana, Chitina, Tonsina, Tazlina, Lower_Copper)
)

# Join Air_Tags with the lookup table
Air_Tags <- Air_Tags %>%
  left_join(lookup_table_awc_watersheds, by = c("SpawnG_SubwatershedCOD" = "Segment_Code"))

# Select columns after join
Air_Tags <- Air_Tags %>%
  select(TAG_NO, Day, SpawnG_Name, SpawnG_AWC, SpawnG_SubwatershedCOD, Tributary)

#Assign AWC's that were not assigned using CR stream network above

Air_Tags <- Air_Tags %>%
  mutate(SpawnG_Name=ifelse(SpawnG_SubwatershedCOD=="212-20-10080-2100-3089","Bremner River",SpawnG_Name))%>%
  mutate(SpawnG_Name=ifelse(SpawnG_SubwatershedCOD=="212-20-10080-2605-3101","Slana River",SpawnG_Name))%>%
  mutate(SpawnG_Name=ifelse(SpawnG_SubwatershedCOD=="212-20-10080-2159-3006","Tasnuna River",SpawnG_Name))%>%
  mutate(SpawnG_Name=ifelse(SpawnG_SubwatershedCOD=="212-20-10080-2461-3091-4042","Victor Creek",SpawnG_Name))%>%
  mutate(SpawnG_Name=ifelse(SpawnG_SubwatershedCOD=="212-20-10080-2481-3188","Gakona River",SpawnG_Name))%>%
  mutate(Tributary=ifelse(SpawnG_SubwatershedCOD=="212-20-10080-2481-3188","Upper Copper",Tributary)) %>%
  mutate(Tributary=ifelse(SpawnG_SubwatershedCOD=="212-20-10080-2655","Upper Copper",Tributary)) #This is Tanada Creek


#Join with fish distances and produce summary of distance traveled for each "stock"

#Include only the last day of detection
fish_distances<-fish_distances %>% 
  group_by(TAG_NO) %>% 
  filter(Day==max(Day))

Air_Tags<-Air_Tags %>% 
  group_by(TAG_NO) %>% 
  filter(Day==max(Day))


# Join Air_Tags with the lookup table
fish_distances <- fish_distances %>%
  left_join(Air_Tags, by = c("TAG_NO"))

# Assign CRS to elevation_df if it's missing (assuming WGS 84)
elevation_df <- st_as_sf(elevation_df, coords = c("x", "y"), crs = 4326)

# Ensure the CRS of fish_distances and elevation_df match
FD <- st_transform(fish_distances, crs = st_crs(elevation_df))

# Extract the coordinates (longitude and latitude) from the fish_distances geometry
fish_coords <- st_coordinates(FD$geometry)[, 1:2]  # Get longitude and latitude

# Create a spatial object for fish coordinates (ensuring CRS is the same as elevation_df)
fish_coords_sf <- st_as_sf(data.frame(x = fish_coords[, 1], y = fish_coords[, 2]), 
                           coords = c("x", "y"), crs = st_crs(elevation_df))

# Find the nearest elevation point for each fish
nearest_elevation_idx <- st_nearest_feature(fish_coords_sf, elevation_df)

# Extract the corresponding elevation values from elevation_df
FD$elevation <- elevation_df$elevation[nearest_elevation_idx]

# View the updated fish_distances data frame with elevation
head(FD)

Summary_ByPopulation <- ddply(FD, .(SpawnG_Name), summarise,
                              TravelDistance = as.integer(round(mean(river_distance_km))),
                              SpawnG_Elevation = as.integer(round(mean(elevation)))
)

# View the result
head(Summary_ByPopulation)

#Produce map with elevation
# Generate breaks within the restricted range
breaks <- classIntervals(FD$elevation, n = 9, style = "pretty")$brks
print(breaks)  # Verify the generated breaks


map_CR <- tm_shape(masked_CR) +
  tm_raster(palette = gray(7:10 / 10), style = "cont", legend.show = FALSE) +
  tm_shape(river_sf) + tm_lines(col = "blue") +
  tm_compass(position = c("right", "top"), size = 2) +
  tm_graticules(x = c(-146, -142), y = c(61, 62, 63), labels.size = 1.2) +
  tm_shape(placenames_sf) + tm_symbols(col = "black", size = .7, scale = .8, border.lwd = NA) +
  tm_text("Site", just = "left", scale = .8, xmod = .3, ymod = -0.3) +
  tm_shape(FD) +
  tm_symbols(
    col = "elevation",  # Use numeric column for color mapping
    palette = brewer.pal(9, "YlOrRd"),  # Color palette
    breaks = breaks,  # Explicitly set breaks
    size = 1.2, scale = 0.5, border.lwd = NA, 
    title.col = "Elevation (m ASL)"
  ) +
  tm_grid(labels.size = 0.7, alpha = .1) +
  tm_xlab("Longitude", size = 1.5, rotation = 0, space = 0) +
  tm_ylab("Latitude", size = 1.5, rotation = 90, space = 0) +
  tm_layout(
    legend.outside = TRUE,         # Place the legend outside the map
    legend.outside.position = "bottom",  # Position at the bottom
    legend.outside.size = 0.5,     # Adjust the width of the legend
    legend.title.size = 0.8,       # Size of legend title
    legend.text.size = 0.6,        # Size of legend text
    legend.width = 2,              # Adjust the width of the legend
    legend.height = 0.5            # Adjust the height of the legend
  ) +
  tm_scale_bar(position = c("right", "bottom"), width = 0.15, text.size = 0.6)  # Scale bar at bottom-right

map_CR

#Plot map with distances data

# Add a new column with distances in kilometers
fish_distances <- fish_distances %>%
  mutate(river_distance_km = as.numeric(river_distance / 1000))

# Restrict distances to the range 4500 to 267000
restricted_distances <- fish_distances$river_distance_km
restricted_distances <- restricted_distances[restricted_distances >= 4.5 & restricted_distances <= 267]

# Generate breaks within the restricted range
breaks <- classIntervals(restricted_distances, n = 12, style = "pretty")$brks
print(breaks)  # Verify the generated breaks

map_CR <- tm_shape(masked_CR) +
  tm_raster(palette = gray(7:10 / 10), style = "cont", legend.show = FALSE) +
  tm_shape(river_sf) + tm_lines(col = "blue") +
  tm_compass(position = c("right", "top"), size = 2) +
  tm_graticules(x = c(-146, -142), y = c(61, 62, 63), labels.size = 1.2) +
  tm_scale_bar(position = c("right", "bottom"), width = 0.15, text.size = 1.4) +
  tm_shape(placenames_sf) + tm_symbols(col = "black", size = .7, scale = .8, border.lwd = NA) +
  tm_text("Site", just = "left", scale = .8, xmod = .3, ymod = -0.3) +
  tm_shape(fish_distances) +
  tm_symbols(
    col = "river_distance_km",  # Use numeric column for color mapping
    palette = brewer.pal(12, "YlOrRd"),  # Color palette
    #breaks = seq(4500, 267000, length.out = 10),  # Explicitly set breaks
    breaks=breaks,
    size = 2.3, scale = 0.3, border.lwd = NA, 
    title.col = "River Distance (km)"  # Add legend title
  ) +
  tm_grid(labels.size = 0.7, alpha = .1) +
  tm_xlab("Longitude", size = 1.2, rotation = 0, space = 0) +
  tm_ylab("Latitude", size = 1.2, rotation = 90, space = 0) +
  tm_layout(
    legend.outside = TRUE,         # Place the legend outside the map
    legend.outside.position = "bottom",  # Position at the bottom
    legend.outside.size = 0.5,     # Adjust the width of the legend
    legend.title.size = 1.8,       # Size of legend title
    legend.text.size = 1.2        # Size of legend text
  )

# View the map
map_CR

