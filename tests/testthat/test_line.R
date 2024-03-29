context("line")

test_that("linedata works", {
  testthat::skip_if_not(require("ncdf4"))
  lineData <- get_fixture_data("linestring")
  nc_file <- write_geometry(nc_file=tempfile(), geom_data = lineData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc, pkg.env$x_nodes)), st_coordinates(lineData)[,"X"])
  expect_equal(as.numeric(ncvar_get(nc, pkg.env$y_nodes)), st_coordinates(lineData)[,"Y"])

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "line")

  returnLineData<-read_geometry(nc_file)
  check_geom(lineData, returnLineData)
})

test_that("multiLine data works", {
  testthat::skip_if_not(require("ncdf4"))
  lineData <- get_fixture_data("multilinestring")
  nc_file <- write_geometry(nc_file=tempfile(), geom_data = lineData)
  nc<-nc_open(nc_file)

  num_coords <- c(nrow(st_geometry(lineData)[[1]][[1]]),
  								nrow(st_geometry(lineData)[[1]][[2]]))
  
  expect_equal(as.numeric(ncvar_get(nc,pkg.env$node_count_var_name)),
               sum(num_coords))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$part_node_count_var_name)),
               num_coords)

  returnLineData<-read_geometry(nc_file)
  check_geom(lineData, returnLineData)
})

test_that("multiline data frame works", {
  testthat::skip_if_not(require("ncdf4"))
  lineData <- get_fixture_data("multilinestring")
  testdata<-as.data.frame(list("name"=c("test_name"), "id"=c(1)), stringsAsFactors = FALSE)
  lineData <- sf::st_sf(dplyr::bind_cols(lineData, testdata))
  nc_file <- write_geometry(nc_file=tempfile(), geom_data = lineData)

  nc<-nc_open(nc_file)
  
  returnLineData<-read_geometry(nc_file)
  check_geom(lineData, returnLineData)
})

test_that("shapefile line data works", {
  testthat::skip_if_not(require("ncdf4"))
  lineData <- sf::st_zm(sf::read_sf("data/NHDLine/NHDLine.shp"))
  nc_file <- write_geometry(nc_file=tempfile(), 
                      geom_data = lineData)
  nc<-nc_open(nc_file)
  returnLineData<-read_geometry(nc_file)
  check_geom(lineData, returnLineData)
  sf::st_geometry(lineData) <- NULL
  sf::st_geometry(returnLineData) <- NULL
  for(name in names(lineData)) {
    if(class(lineData[name][[1]]) == "Date") lineData[[name]] <- as.character(lineData[[name]])
    expect_equal(class(lineData[name][[1]]), class(returnLineData[name][[1]]))
  }
  for(name in names(lineData)) {
    if(is.character(returnLineData[name][[1]])) lineData[name][is.na(lineData[name])] <- ""
    expect_equal(c(lineData[name]), c(returnLineData[name]))
  }
})

test_that("NHDPlus Multilinestring", {
  f <- system.file("extdata/nhdp_flowline_sample.gpkg", package = "ncdfgeom")
  
  test_dat <- sf::read_sf(f)
  
  test_nc <- expect_warning(write_geometry(tempfile(), test_dat), "Found more than two dimensions in geometry. Removing Z and M content.")
  
  test_dat_2 <- read_geometry(test_nc)
  
  expect_equal(class(test_dat_2$FDATE), "character") # coerced to character
  expect_equal(class(sf::st_geometry(test_dat_2)[[1]])[1], "XY") 
  
})

