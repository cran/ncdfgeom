## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=4
)
options(scipen = 9999)

## ----attributes---------------------------------------------------------------
attribute_file<-system.file('extdata/yahara_alb_attributes.csv', package = "ncdfgeom")

attributes <- read.csv(attribute_file, colClasses='character')
lats <- as.numeric(attributes$YCOORD)
lons <- as.numeric(attributes$XCOORD)
alts <- rep(1,length(lats)) # Making up altitude for the sake of demonstration.

## ----timeseries---------------------------------------------------------------
timeseries_file <- system.file('extdata/yahara_alb_gdp_file.csv', package = "ncdfgeom")

raw_data <- geoknife::parseTimeseries(timeseries_file, delim=',', with.units=TRUE)

timeseries_data <- raw_data[2:(ncol(raw_data) - 3)]

time <- raw_data$DateTime

long_name <- paste(raw_data$variable[1], 'area weighted', raw_data$statistic[1], 'in', 
                   raw_data$units[1], sep=' ')

meta <- list(name=raw_data$variable[1], long_name=long_name)

## ----write--------------------------------------------------------------------
nc_summary<-'example summary'
nc_date_create<-'2099-01-01'
nc_creator_name='example creator'
nc_creator_email='example@test.com'
nc_project='example ncdfgeom'
nc_proc_level='just an example no processing'
nc_title<-'example title'

global_attributes<-list(title = nc_title, 
                        summary = nc_summary, 
                        date_created = nc_date_create, 
                        creator_name = nc_creator_name,
                        creator_email = nc_creator_email, 
                        project = nc_project,
                        processing_level = nc_proc_level)

ncdfgeom::write_timeseries_dsg(nc_file = "demo_nc.nc", 
                               instance_names = names(timeseries_data),
                               lats = lats, 
                               lons = lons, 
                               alts = alts,
                               times = time, 
                               data = timeseries_data,
                               data_unit = raw_data$units[1],
                               data_prec = 'double',
                               data_metadata = meta,
                               attributes = global_attributes) -> nc_file


## ----dim----------------------------------------------------------------------
ncmeta::nc_dims(nc_file)

## ----var----------------------------------------------------------------------
ncmeta::nc_vars(nc_file)

## ----dim2---------------------------------------------------------------------
ncmeta::nc_dims(nc_file)

## ----dump_polygons, echo=FALSE------------------------------------------------
try({ncdump <- system(paste("ncdump -h", nc_file), intern = TRUE)
cat(ncdump ,sep = "\n")}, silent = TRUE)

## ----read---------------------------------------------------------------------
timeseries_dataset <- ncdfgeom::read_timeseries_dsg(nc_file)
names(timeseries_dataset)

## ----delete, echo=F, message=F------------------------------------------------
t <- file.remove(nc_file)

