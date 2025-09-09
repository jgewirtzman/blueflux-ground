# Minimal publication-ready map
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(dplyr)
library(maps)
library(mapdata)

# Define plot locations
locations <- data.frame(
  site = c("MI", "RB", "SRS-5", "SRS-6", "FLM-30", "CP-40", "BL", "SE-1"),
  latitude = c(25.929833, 25.862889, 25.377028, 25.364639, 25.159889, 25.149278, 25.157833, 25.353889),
  longitude = c(-81.672167, -81.561194, -81.032361, -81.077944, -80.912000, -80.911028, -80.922694, -80.381000)
)

# Get high-resolution Florida coastline data
florida_hires <- map_data("worldHires", region = "USA:Florida")

# Get Natural Earth Florida data for comparison
florida_ne <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "Florida")

# Define study area bounds with minimal padding
lat_range <- range(locations$latitude)
lon_range <- range(locations$longitude)
lat_padding <- diff(lat_range) * 0.08
lon_padding <- diff(lon_range) * 0.08

study_bounds <- list(
  xmin = lon_range[1] - lon_padding,
  xmax = lon_range[2] + lon_padding,
  ymin = lat_range[1] - lat_padding,
  ymax = lat_range[2] + lat_padding
)

# Create detailed map with high-resolution coastline and proper labels
detailed_map <- ggplot() +
  # Add detailed Florida coastline and islands
  geom_polygon(data = florida_hires,
               aes(x = long, y = lat, group = group),
               fill = "grey80",
               color = "grey60",
               size = 0.1) +
  
  # Add study sites as clear, larger symbols
  geom_point(data = locations,
             aes(x = longitude, y = latitude),
             color = "black",
             fill = "white",
             shape = 21,
             size = 4,
             stroke = 1.5) +
  
  # Add clear labels with strong repelling
  geom_text_repel(data = locations,
                  aes(x = longitude, y = latitude, label = site),
                  size = 4,
                  color = "black",
                  fontface = "bold",
                  box.padding = 0.8,
                  point.padding = 0.8,
                  segment.color = "black",
                  segment.size = 0.5,
                  min.segment.length = 0,
                  max.overlaps = Inf,
                  force = 3,
                  force_pull = 2) +
  
  # Set bounds with fixed aspect ratio
  coord_fixed(ratio = 1.3,
              xlim = c(study_bounds$xmin, study_bounds$xmax),
              ylim = c(study_bounds$ymin, study_bounds$ymax)) +
  
  # Minimal theme
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(5, 5, 5, 5)
  )

# Display the detailed map (this should be the main output)
print(detailed_map)

# Alternative simpler version using Natural Earth data
simple_map <- ggplot() +
  # Add Florida using Natural Earth data
  geom_sf(data = florida_ne,
          fill = "grey80",
          color = "grey60",
          size = 0.2) +
  
  # Add study sites as clear, larger symbols
  geom_point(data = locations,
             aes(x = longitude, y = latitude),
             color = "black",
             fill = "white",
             shape = 21,
             size = 4,
             stroke = 1.5) +
  
  # Add clear labels with strong repelling
  geom_text_repel(data = locations,
                  aes(x = longitude, y = latitude, label = site),
                  size = 4,
                  color = "black",
                  fontface = "bold",
                  box.padding = 0.8,
                  point.padding = 0.8,
                  segment.color = "black",
                  segment.size = 0.5,
                  min.segment.length = 0,
                  max.overlaps = Inf,
                  force = 3,
                  force_pull = 2) +
  
  # Set bounds
  coord_sf(xlim = c(study_bounds$xmin, study_bounds$xmax),
           ylim = c(study_bounds$ymin, study_bounds$ymax),
           expand = FALSE) +
  
  # Minimal theme
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(5, 5, 5, 5)
  )

print("Simple version:")
print(simple_map)

# Save both versions
ggsave("detailed_study_sites.pdf", detailed_map, 
       width = 150, height = 120, units = "mm", dpi = 300)

ggsave("simple_study_sites.pdf", simple_map, 
       width = 150, height = 120, units = "mm", dpi = 300)