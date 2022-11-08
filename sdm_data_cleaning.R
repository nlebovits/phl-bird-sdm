library(tidyverse)
library(sf)
library(janitor)
library(lubridate)
library(tmap)
library(mapview)


# import eda package for EDA?

ebird = read.table("C:/Users/Nissim/Desktop/Fall 2022/Floodplain Management/Final Project Data/eBird/ebd_US-PA-101_relSep-2022.txt",
           sep = "\t", 
           header = TRUE,
           fill = TRUE) |>
           clean_names()

ebird_i = ebird |>
            dplyr::select(taxonomic_order,
                          category,
                          taxon_concept_id,
                          common_name,
                          scientific_name,
                          exotic_code,
                          observation_count,
                          breeding_code,
                          breeding_category,
                          county_code,
                          locality,
                          locality_id,
                          latitude,
                          longitude,
                          observation_date,
                          time_observations_started,
                          observer_id,
                          approved,
                          effort_distance_km,
                          number_observers,
                          protocol_type,
                          duration_minutes) |>
            dplyr::filter(approved == "1") # make sure R doesn't confuse dplyr::filter with stats::filter

# function to convert time observation to hours since midnight
# pulled directly from https://cornelllabofornithology.github.io/ebird-best-practices/ebird.html#ebird-zf
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

ebird_i$observation_date = as_date(ebird_i$observation_date)

ebird_i = ebird_i |>
            mutate(
              # convert X to NA
              observation_count = if_else(observation_count == "X", 
                                          NA_character_, observation_count),
              observation_count = as.integer(observation_count),
              # effort_distance_km to 0 for non-travelling counts
              effort_distance_km = if_else(protocol_type != "Traveling", 
                                           "0", effort_distance_km),
              # convert time to decimal hours since midnight
              time_observations_started = time_to_decimal(time_observations_started),
              # split date into year and day of year
              year = year(observation_date),
              day_of_year = yday(observation_date)
            ) |>
          filter(
            # effort filters
            duration_minutes <= 5 * 60,
            effort_distance_km <= 5,
            # last 10 years of data
            year >= 2010,
            # 10 or fewer observers
            number_observers <= 10)

ebird_last_decade = ebird_i |>
                      filter(year >= 2012 & year <= 2021)


# Major increase in observations beginning in 2020 corresponds to the pandemic uptick in birding!
# we can stick to the last ten years of data to try to confirm that these hotspots remain the same and that 
# the recent years aren't exerting undue influence over our data

# count by year

countXyear = ebird_sf |>
                group_by(year) |>
                  tally() |>
                  st_drop_geometry()

ggplot(countXyear) +
  geom_col(aes(x = year, y = n))

# count by scientific name
countXspecies = ebird_sf |>
  group_by(scientific_name) |>
  tally() |>
  st_drop_geometry()

countXspecies = countXspecies[order(-countXspecies$n), ]

ggplot(countXspecies[1:20,]) +
  geom_col(aes(x = scientific_name, y = n)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# count by common name
countXcommon_name = ebird_sf |>
  group_by(common_name) |>
  tally() |>
  st_drop_geometry()

countXcommon_name = countXcommon_name[order(-countXcommon_name$n), ]

ggplot(countXcommon_name[1:20,]) +
  geom_col(aes(x = common_name, y = n)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


                    

##### Let's make a quick map of PHL city limits and then create a hexagon over it

### then we can aggregate the points to the hexagons and visualize them that way

ebird_sf = sf::st_as_sf(ebird_last_decade,
                        coords = c("longitude",
                                   "latitude"),
                        crs = st_crs("EPSG:4326")) |>
  st_transform(st_crs("EPSG:2272")) #NAD83/PA South (ftUS)

samp = ebird_sf[1:500,]

mapview(samp)

phl_bounds = st_read("C:/Users/Nissim/Desktop/Fall 2022/Spat Stats/phl_city_limits/City_Limits.shp") |>
  st_transform(crs = st_crs(ebird_sf))

tm_shape(phl_bounds) + 
  tm_borders() +
  tm_shape(ebird_sf) +
  tm_dots(col = "red", alpha = 0.1) + 
  tm_facets(by = "year")

phl_grid <- phl_bounds %>%
  st_make_grid(st_bbox(.), square = FALSE, cellsize = 2640) %>% #currently using half mile cells
  st_sf() %>% 
  mutate(hex_id = row_number())

phl_grid = phl_grid[phl_bounds,]


ebird_sf_2021 = ebird_sf |>
                  filter(year == "2021")

ebird_sf_2020 = ebird_sf |>
                  filter(year == "2020")

ebird_sf_2019= ebird_sf |>
                  filter(year == "2019")

ebird_sf_2018 = ebird_sf |>
                  filter(year == "2018")

ebird_sf_2017 = ebird_sf |>
                  filter(year == "2017")





phl_birds_hex = phl_grid |>
                    mutate(counts_2021 = lengths(st_intersects(phl_grid, ebird_sf_2021)),
                           counts_2020 = lengths(st_intersects(phl_grid, ebird_sf_2020)),
                           counts_2019 = lengths(st_intersects(phl_grid, ebird_sf_2019)),
                           counts_2018 = lengths(st_intersects(phl_grid, ebird_sf_2018)),
                           counts_2017 = lengths(st_intersects(phl_grid, ebird_sf_2017)))

tmap_mode("view")

hex_2021 = tm_shape(phl_birds_hex) + 
                  tm_polygons(col = "counts_2021", 
                              style = "jenks", 
                              palette = "plasma", 
                              alpha = 0.5, 
                              id = "counts_2021") +
                  tm_layout(title = "2021 Counts")


hex_2020 = tm_shape(phl_birds_hex) + 
                tm_polygons(col = "counts_2020", 
                            style = "jenks", 
                            palette = "plasma", 
                            alpha = 0.5, 
                            id = "counts_2020") +
                tm_layout(title = "2020 Counts")


hex_2019 = tm_shape(phl_birds_hex) + 
                  tm_polygons(col = "counts_2019", 
                              style = "jenks", 
                              palette = "plasma", 
                              alpha = 0.5, 
                              id = "counts_2019") +
                  tm_layout(title = "2019 Counts")


hex_2018 = tm_shape(phl_birds_hex) + 
                  tm_polygons(col = "counts_2018", 
                              style = "jenks", 
                              palette = "plasma", 
                              alpha = 0.5, 
                              id = "counts_218") +
                  tm_layout(title = "2018 Counts")


hex_2017 = tm_shape(phl_birds_hex) + 
                tm_polygons(col = "counts_2017", 
                            style = "jenks", 
                            palette = "plasma", 
                            alpha = 0.5, 
                            id = "counts_2017") +
                tm_layout(title = "2017 Counts")


tmap_arrange(hex_2021,
             hex_2020,
             hex_2019,
             hex_2018,
             hex_2017)



# would it be appropriate to now use local moran's i to find hotspots?
# we can look at these year over year













# https://geocompr.robinlovelace.net/raster-vector.html?q=rasterize#rasterization
                    