library(tidyverse)
library(sf)
library(janitor)
library(lubridate)
library(tmap)
library(mapview)
library(janitor)


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

countXyear = ebird_last_decade |>
                group_by(year) |>
                  tally() 

ggplot(countXyear) +
  geom_col(aes(x = year, y = n))

# count by scientific name
countXspecies = ebird_last_decade |>
                    group_by(scientific_name) |>
                    tally()

countXspecies = countXspecies[order(-countXspecies$n), ]

ggplot(countXspecies[1:20,]) +
  geom_col(aes(x = scientific_name, y = n)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# count by common name
countXcommon_name = ebird_last_decade|>
                        group_by(common_name) |>
                        tally()

countXcommon_name = countXcommon_name[order(-countXcommon_name$n), ]

ggplot(countXcommon_name[1:20,]) +
  geom_col(aes(x = common_name, y = n)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


                    
# add IUCN data

iucn_redlist = readxl::read_xlsx("C:/Users/Nissim/Desktop/Fall 2022/Floodplain Management/Final Project Data/Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_6b.xlsx")


colnames(iucn_redlist) = iucn_redlist[2, ] 

iucn_redlist = iucn_redlist |>
                clean_names() |>
                as.data.frame() |>
                filter(!is.na(family)) |>
                dplyr::select(scientific_name,
                              x2022_iucn_red_list_category)|>
                dplyr::rename(iucn_redlist_cat = x2022_iucn_red_list_category)

# filter for the IUCN categories that we want
# full list here: http://datazone.birdlife.org/species/spcredcat
# we need to include only recognized species with sufficient data
# also exluding extinct species, etc.


iucn_wanted = c("CR (PE)",
                "CR",
                "EN",
                "VU",
                "NT",
                "LC")

iucn_redlist = iucn_redlist |>
                  filter(iucn_redlist_cat %in% iucn_wanted)

ebird_last_decade = left_join(ebird_last_decade, iucn_redlist, by = "scientific_name")


# count by iucn status
countXiucn_cat = ebird_last_decade|>
                        group_by(iucn_redlist_cat) |>
                        tally()

countXiucn_cat = countXiucn_cat[order(-countXiucn_cat$n), ]

ggplot(countXiucn_cat[1:20,]) +
  geom_col(aes(x = iucn_redlist_cat, y = log(n))) + 
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
  tm_facets(by = "iucn_redlist_cat")

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



library(sp)
library(spdep)
library(sfdep)

# create our queen weight matrix

# grab geometry
phl_birds_hex = phl_birds_hex |>
  mutate(nb_geom = st_geometry(phl_birds_hex), #geoms for poly2nb
         nb = st_contiguity(nb_geom), # generate neighbors
         weights = st_weights(nb)) # weight matrices from neighbors

summary(phl_birds_hex$nb)


birds_hex_lines = nb2lines(phl_birds_hex$nb, 
                          coords = st_centroid(phl_birds_hex$geometry), 
                          as_sf = TRUE)

tm_shape(phl_birds_hex) + 
  tm_borders(col = "grey", lwd = 0.5) + 
  tm_shape(phl_birds_hex) +
  tm_dots() +
  tm_shape(birds_hex_lines) +
  tm_lines(col = "red") +
  tm_layout(frame = FALSE)


# Local Moran's I

# local Moran's I analysis with queen weight matrix; print results

#2021

lisa_2021 = local_moran(phl_birds_hex$counts_2021, phl_birds_hex$nb, phl_birds_hex$weights, nsim = 999)


lisa_2021 = cbind(phl_birds_hex, 
                     lisa_2021)

pal = c("#2166AC",
        "#92C5DE",
        "#F4A582",
        "#B2182B")

lisa_21_map = tm_shape(lisa_2021) +
  tm_fill(col = "mean", palette = pal, title = "LISA Clusters 2021")+
  tm_borders(col = "white", lwd = 0.05) +
  tm_layout(frame = FALSE, main.title = "LISA Clusters 2021")

#2020

lisa_2020 = local_moran(phl_birds_hex$counts_2020, phl_birds_hex$nb, phl_birds_hex$weights, nsim = 999)


lisa_2020 = cbind(phl_birds_hex, 
                  lisa_2020)


lisa_20_map = tm_shape(lisa_2020) +
  tm_fill(col = "mean", palette = pal, title = "LISA Clusters 2020")+
  tm_borders(col = "white", lwd = 0.05) +
  tm_layout(frame = FALSE, main.title = "LISA Clusters 2020")

#2019

lisa_2019 = local_moran(phl_birds_hex$counts_2019, phl_birds_hex$nb, phl_birds_hex$weights, nsim = 999)


lisa_2019 = cbind(phl_birds_hex, 
                  lisa_2019)

lisa_19_map = tm_shape(lisa_2019) +
  tm_fill(col = "mean", palette = pal, title = "LISA Clusters 2019")+
  tm_borders(col = "white", lwd = 0.05) +
  tm_layout(frame = FALSE, main.title = "LISA Clusters 2019")

#2018

lisa_2018 = local_moran(phl_birds_hex$counts_2018, phl_birds_hex$nb, phl_birds_hex$weights, nsim = 999)


lisa_2018 = cbind(phl_birds_hex, 
                  lisa_2018)

lisa_18_map = tm_shape(lisa_2018) +
  tm_fill(col = "mean", palette = pal, title = "LISA Clusters 2018")+
  tm_borders(col = "white", lwd = 0.05) +
  tm_layout(frame = FALSE, main.title = "LISA Clusters 2018")

#2017

lisa_2017 = local_moran(phl_birds_hex$counts_2017, phl_birds_hex$nb, phl_birds_hex$weights, nsim = 999)


lisa_2017 = cbind(phl_birds_hex, 
                  lisa_2017)

lisa_17_map = tm_shape(lisa_2017) +
  tm_fill(col = "mean", palette = pal, title = "LISA Clusters 2017")+
  tm_borders(col = "white", lwd = 0.05) +
  tm_layout(frame = FALSE, main.title = "LISA Clusters 2017")



tmap_arrange(lisa_21_map,
             lisa_20_map,
             lisa_19_map,
             lisa_18_map,
             lisa_17_map)


#### if I cbind() stuff together and include a column for year, I can use tmap_animate() and set facet = year, which will allow me to create
# an animated map.
# I could then even try going back to 2012 or whatever to show change in hotspots over time.
    # to do this, I would have to subset the lisa stats, change colnames to mean_XXXX, and then cbind() them all in to a new dataframe

# https://geocompr.robinlovelace.net/raster-vector.html?q=rasterize#rasterization
                    