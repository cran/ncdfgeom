## ----setup_1, include = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=4
)
options(scipen = 9999)

## ----libs, message=FALSE, warning=FALSE---------------------------------------
library(sf)
library(dplyr)
library(ncdfgeom)

## ----data_shape_2-------------------------------------------------------------
prcp_data <- readRDS(system.file("extdata/climdiv-pcpndv.rds", package = "ncdfgeom"))
print(prcp_data, max_extra_cols = 0)
plot(prcp_data$date, prcp_data$`0101`, col = "red", 
     xlab = "date", ylab = "monthly precip (inches)", main = "Sample Timeseries for 0101-'Northern Valley'")
lines(prcp_data$date, prcp_data$`0101`)

## ----data_shape---------------------------------------------------------------
climdiv_poly <- read_sf(system.file("extdata/climdiv.gpkg", package = "ncdfgeom"))
print(climdiv_poly)
plot(st_geometry(climdiv_poly), main = "Climate Divisions with 0101-'Northern Valley' Highlighted")
plot(st_geometry(filter(climdiv_poly, CLIMDIV == "0101")), col = "red", add = TRUE)

## ----write_ts, warning = FALSE------------------------------------------------
climdiv_centroids <- climdiv_poly %>%
  st_transform(5070) %>% # Albers Equal Area
  st_set_agr("constant") %>%
  st_centroid() %>%
  st_transform(4269) %>% #NAD83 Lat/Lon
  st_coordinates() %>%
  as.data.frame()

nc_file <- "climdiv_prcp.nc"

prcp_dates <- prcp_data$date
prcp_data <- select(prcp_data, -date)
prcp_meta <- list(name = "climdiv_prcp_inches", 
                  long_name = "Estimated Monthly Precipitation (Inches)")

write_timeseries_dsg(nc_file = nc_file, 
                     instance_names = climdiv_poly$CLIMDIV, 
                     lats = climdiv_centroids$Y, 
                     lons = climdiv_centroids$X, 
                     times = prcp_dates, 
                     data = prcp_data, 
                     data_unit = rep("inches", (ncol(prcp_data) - 1)), 
                     data_prec = "float", 
                     data_metadata = prcp_meta, 
                     attributes = list(title = "Demonstation of ncdfgeom"), 
                     add_to_existing = FALSE)

climdiv_poly <- st_sf(st_cast(climdiv_poly, "MULTIPOLYGON"))

write_geometry(nc_file = "climdiv_prcp.nc", 
               geom_data = climdiv_poly,
               variables = "climdiv_prcp_inches")

## ----ncdump-------------------------------------------------------------------
try({ncdump <- system(paste("ncdump -h", nc_file), intern = TRUE)
cat(ncdump, sep = "\n")}, silent = TRUE)

## ----read---------------------------------------------------------------------
# First read the timeseries.
prcp_data <- read_timeseries_dsg("climdiv_prcp.nc")

# Now read the geometry.
climdiv_poly <- read_geometry("climdiv_prcp.nc")

## ----data_summary-------------------------------------------------------------
names(prcp_data)
class(prcp_data$time)
names(prcp_data$varmeta$climdiv_prcp_inches)
prcp_data$data_unit
prcp_data$data_prec
str(names(prcp_data$data_frames$climdiv_prcp_inches))
prcp_data$global_attributes
names(climdiv_poly)

## ----p_colors_source, echo=FALSE----------------------------------------------
# Because we've gotta have pretty colors!
p_colors <- function (n, name = c("precip_colors")) {
# Thanks! https://quantdev.ssri.psu.edu/tutorials/generating-custom-color-palette-function-r
    p_rgb <- col2rgb(c("#FAFBF3", "#F0F8E3", "#D4E9CA", 
                       "#BBE0CE", "#B7DAD0", "#B0CCD7", 
                       "#A9B8D7", "#A297C2", "#8F6F9E", 
                       "#684A77", "#41234D"))
    precip_colors = rgb(p_rgb[1,],p_rgb[2,],p_rgb[3,],maxColorValue = 255)
    name = match.arg(name)
    orig = eval(parse(text = name))
    rgb = t(col2rgb(orig))
    temp = matrix(NA, ncol = 3, nrow = n)
    x = seq(0, 1, , length(orig))
    xg = seq(0, 1, , n)
    for (k in 1:3) {
        hold = spline(x, rgb[, k], n = n)$y
        hold[hold < 0] = 0
        hold[hold > 255] = 255
        temp[, k] = round(hold)
    }
    palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
    palette
}

## ----plot, fig.height=6, fig.width=8------------------------------------------
climdiv_poly <- climdiv_poly %>%
  st_transform(3857) %>% # web mercator
  st_simplify(dTolerance = 5000)

title <- paste0("\n Sum of: ", prcp_data$varmeta$climdiv_prcp_inches$long_name, "\n", 
                format(prcp_data$time[1], 
                         "%Y-%m", tz = "UTC"), " - ", 
                format(prcp_data$time[length(prcp_data$time)], 
                         "%Y-%m", tz = "UTC"))

prcp_sum <- apply(prcp_data$data_frames$climdiv_prcp_inches, 
                  2, sum, na.rm = TRUE)

prcp <- data.frame(CLIMDIV = names(prcp_sum), 
                   prcp = as.numeric(prcp_sum), 
                   stringsAsFactors = FALSE) %>%
  right_join(climdiv_poly, by = "CLIMDIV") %>% 
  st_as_sf()

plot(prcp["prcp"], lwd = 0.1, pal = p_colors, 
     breaks = seq(0, 14000, 1000),
     main = title,
     key.pos = 3, key.length = lcm(20))
  

## ----setup_dontrun, eval = FALSE----------------------------------------------
#  # Description here: ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/divisional-readme.txt
#  prcp_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpndv-v1.0.0-20190408"
#  
#  prcp_file <- "prcp.txt"
#  
#  download.file(url = prcp_url, destfile = prcp_file, quiet = TRUE)
#  
#  division_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/CONUS_CLIMATE_DIVISIONS.shp.zip"
#  division_file <- "CONUS_CLIMATE_DIVISIONS.shp.zip"
#  
#  download.file(url = division_url, destfile = division_file, quiet = TRUE)
#  unzip("CONUS_CLIMATE_DIVISIONS.shp.zip")
#  
#  climdiv_poly <- read_sf("GIS.OFFICIAL_CLIM_DIVISIONS.shp") %>%
#    select("CLIMDIV", CLIMDIV_NAME = "NAME") %>%
#    mutate(CLIMDIV = ifelse(nchar(as.character(CLIMDIV)) == 3,
#                            paste0("0",as.character(CLIMDIV)),
#                            as.character(CLIMDIV))) %>%
#    st_simplify(dTolerance = 0.0125)
#  
#  month_strings <- c("jan", "feb", "mar", "apr", "may", "jun",
#                     "jul", "aug", "sep", "oct", "nov", "dec")
#  
#  prcp_data <- read.table(prcp_file, header = FALSE,
#                          colClasses = c("character",
#                                         rep("numeric", 12)),
#                          col.names = c("meta", month_strings))
#  
#  # Here we gather the data into a long format and prep it for ncdfgeom.
#  prcp_data <- prcp_data %>%
#    gather(key = "month", value = "precip_inches", -meta) %>%
#    mutate(climdiv = paste0(substr(meta, 1, 2), substr(meta, 3, 4)),
#           year = substr(meta, 7, 10),
#           precip_inches = ifelse(precip_inches < 0, NA, precip_inches)) %>%
#    mutate(date = as.Date(paste0("01", "-", month, "-", year),
#                          format = "%d-%b-%Y")) %>%
#    select(-meta, -month, -year) %>%
#    filter(climdiv %in% climdiv_poly$CLIMDIV) %>%
#    spread(key = "climdiv", value = "precip_inches") %>%
#    filter(!is.na(`0101`))
#    as_tibble()
#  
#  # Now make sure things are in the same order.
#  climdiv_names <- names(prcp_data)[2:length(names(prcp_data))]
#  climdiv_row_order <- match(climdiv_names, climdiv_poly$CLIMDIV)
#  climdiv_poly <- climdiv_poly[climdiv_row_order, ]
#  
#  sf::write_sf(climdiv_poly, "climdiv.gpkg")
#  saveRDS(prcp_data, "climdiv-pcpndv.rds")
#  
#  unlink("GIS*")
#  unlink("CONUS_CLIMATE_DIVISIONS.shp.zip")
#  unlink("prcp.txt")

## ----p_colors, eval=FALSE-----------------------------------------------------
#  p_colors <- function (n, name = c("precip_colors")) {
#  # Thanks! https://quantdev.ssri.psu.edu/tutorials/generating-custom-color-palette-function-r
#      p_rgb <- col2rgb(c("#FAFBF3", "#F0F8E3", "#D4E9CA",
#                         "#BBE0CE", "#B7DAD0", "#B0CCD7",
#                         "#A9B8D7", "#A297C2", "#8F6F9E",
#                         "#684A77", "#41234D"))
#      precip_colors = rgb(p_rgb[1,],p_rgb[2,],p_rgb[3,],maxColorValue = 255)
#      name = match.arg(name)
#      orig = eval(parse(text = name))
#      rgb = t(col2rgb(orig))
#      temp = matrix(NA, ncol = 3, nrow = n)
#      x = seq(0, 1, , length(orig))
#      xg = seq(0, 1, , n)
#      for (k in 1:3) {
#          hold = spline(x, rgb[, k], n = n)$y
#          hold[hold < 0] = 0
#          hold[hold > 255] = 255
#          temp[, k] = round(hold)
#      }
#      palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
#      palette
#  }

## ----cleanup, echo=FALSE------------------------------------------------------
unlink("climdiv_prcp.nc")

