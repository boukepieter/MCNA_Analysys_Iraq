locations <- read.xlsx("c:/Users/REACH-IRQ-GIS/Documents/2019 MCNA/201905 MCNA KoBo/kml_files/locations_overview.xlsx",
                      "Sheet2")
locations <- locations[,-1]
locs <- unique(locations$strata)
for (i in 1:length(locs)){
  selection <- locations[which(locations$strata==locs[i]),]
  write.csv(selection,sprintf("c:/Users/REACH-IRQ-GIS/Documents/2019 MCNA/201905 MCNA KoBo/kml_files/Locations_per_district/%s_locations.csv",
                              locs[i]), row.names = F)
}
