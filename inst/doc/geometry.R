## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=4
)
options(scipen = 9999)

## ----libs----------------------------------------------------------------
example_file <- tempfile()

file.copy(from = system.file('extdata/example_huc_eta.nc', package = 'ncdfgeom'), 
					to = example_file, 
					overwrite = TRUE) -> quiet

polygons <- sf::read_sf(system.file('extdata/example_huc_eta.json', package = 'ncdfgeom'))

polygons <- dplyr::select(polygons, LOADDATE, AREASQKM, HUC12, NAME)

plot(sf::st_geometry(polygons))

## ----dump_polygons, echo=FALSE, cache=TRUE, eval = FALSE-----------------
#  try({ncdump <- system(paste("ncdump -h", example_file), intern = TRUE)
#       cat(ncdump ,sep = "\n")}, silent = TRUE)

## ----demo----------------------------------------------------------------
(vars <- ncmeta::nc_vars(example_file))

ncdfgeom::write_geometry(nc_file=example_file,
                         geom_data = polygons, 
                         instance_dim_name = "station", 
                         variables = vars$name) -> example_file

## ----dump_polygons_ts, echo=FALSE----------------------------------------
try({ncdump <- system(paste("ncdump -h", example_file), intern = TRUE)
cat(ncdump ,sep = "\n")}, silent = TRUE)

## ----read, warning=F-----------------------------------------------------
polygons_sf <- ncdfgeom::read_geometry(example_file)

plot(sf::st_geometry(polygons_sf))
sf::write_sf(polygons_sf, "polygons.gpkg")

## ----cleanup, echo=F-----------------------------------------------------
temp <- file.remove(example_file, "polygons.gpkg")

