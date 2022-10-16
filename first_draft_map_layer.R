#----
#Oct 5, 2022
# can use:
  # slope
  # contour
  # aspect
  # hillshade
  # distance to water
      # distance to river
  # distance to parks over a certain size
  # land cover
  # using zonal geometry, *adjacent* (or nearby) land cover (to account for rapid, small-scale changes in urban land cover)
  # fragmentation
  # pop density
  # impervious cover
  # distance to arterials
    # distance to collectors
  # tree cover
  # tree cover type?
  # climate zone (there are two in phl)

#once I have a model, I can simply do forecasting by plugging predicted NOAA data into the model!!
 

#----
#pre Oct 5, 2022

#final floodplain mgmt project

#identify biodiversity hotspots w gettis oords
#compare to NOAA floodplain data
#identify threats

#(less complicated than species distribution modeling)
#alternatively, could I look specifically at distribution of endangered 
#& rare bird species to identify threats specifically to them?
#this would entail getting familiar with ebird data & how to clean it statistically
#https://cornelllabofornithology.github.io/ebird-best-practices/


#poke around on ropensci & other databases to see if there's anything good that I could pull in here.
#Also check in with PWD, PPR to see what exists here
#& check with Ted to see if he has any insight from the perspective of urban shift


#21/9/2022 notes:
#I can create a citywide raster of biodiversity
#then overlay the noaa data
#(both need to be at the larger pixel cell size between the two)
#then use map algebra to combine them
#and score each individual cell for 
#1) amount of biodiversity and
#2) risk of sea level rise (none to high)
##can visualize this using a bivariate choropleth map

#can at least ise arcgis to generate layers for:
# slope
# contour
# aspect (compass direction of descent)
# hillshade (shaded relief or insolation)
# this is a cool visualization tool--probably the basis for rayshader package
#for all of these, check raster GIS class notes

###
# remember to distinguish between abudance vs. richness. I'm studying the latter.
