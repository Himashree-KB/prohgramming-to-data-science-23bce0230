# Install package
install.packages("leaflet")

# Load library
library(leaflet)

# Step 1: Initialize map
map1 <- leaflet() %>%
  addTiles() %>%
  setView(lng = 80.1309, lat = 12.9699, zoom = 10)

map1

# Step 2: Add marker with popup
map2 <- map1 %>%
  addMarkers(lng = 80.1309, lat = 12.9699,
             popup = "Hello from Katpadi!")

map2

# Step 3: Add circle marker
map3 <- map2 %>%
  addCircleMarkers(
    lng = 80.2, lat = 12.95,
    color = "red",
    radius = 8,
    label = "This is a circle marker!"
  )

map3
