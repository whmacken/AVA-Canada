# Setup -------------------------------------------------------------------
##adds data from a VPro db to a postgres db created in init-db.R
library(DBI)
library(sf)
# create an R script with read/write privileged credentials:
# username <- ""
# password <- ""
source("creds.R") # username, password for pg
pgCon <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = 'ava_canada', 
  host = host,port = port, user = username, password = password)
dataTables <- c("env", "veg", "mineral", "humus", "admin", "metadata")
lookupTables <- c("USysAllSpecs", "USysTableOfLists")
copy_table <- function(accCon, pgCon, name, prefix = '') {
  dataTable <- DBI::dbReadTable(conn = accCon, name = sprintf('%s%s', prefix, 
    name))
  names(dataTable) <- convert_snake_case(names(dataTable))
  if (prefix == 'AVACanada2024_') { ### name of access project tables
    prefix <- 'ava_canada_' ## name of postgres db tables
  }
  DBI::dbAppendTable(conn = pgCon, name = DBI::SQL(sprintf('%s%s', prefix, name)), 
    value = dataTable)
}

convert_snake_case <- function(x) {
  gsub('([a-z]{2,})([A-Z])', '\\1_\\2', x) |> 
    tolower()
}


dbExecute(pgCon, "DELETE FROM ava_canada_env")
dbExecute(pgCon, "DELETE FROM ava_canada_veg")
dbExecute(pgCon, "DELETE FROM ava_canada_mineral")
dbExecute(pgCon, "DELETE FROM ava_canada_humus")
dbExecute(pgCon, "DELETE FROM ava_canada_admin")
dbExecute(pgCon, "DELETE FROM ava_canada_metadata")

# Append Data from Vpro to postgres Tables ------------------------------------------------------

accessPath <- "E:/Arctic/Vpro64_Data_Arctic/AVACanada2024.accdb"

accCon <- dbConnect(odbc::odbc(), .connection_string = 
    sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;",
      accessPath))
lapply(dataTables, copy_table, accCon = accCon, pgCon = pgCon,
  prefix = 'AVACanada2024_')

# Append Lookup Table -----------------------------------------------------

# accessPath <- "E:/Arctic/Vpro64_Arctic/VPro64.accdb"
# accCon <- dbConnect(odbc::odbc(), .connection_string =
#     sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;",
#       accessPath))
# lapply(lookupTables, copy_table, accCon = accCon, pgCon = pgCon,
#   prefix = '')


accessPath <- "data/VPro64-23Feb2023/VPro64/VPro64.accdb"
accCon <- dbConnect(odbc::odbc(), .connection_string =
                      sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;",
                              accessPath))
lapply(lookupTables, copy_table, accCon = accCon, pgCon = pgCon,
       prefix = '')
# Write Zones Table -------------------------------------------------------

zones <- data.table::fread('data/BC_BGCs_Info_v12_15.csv')
data.table::setnames(zones, tolower)
DBI::dbWriteTable(pgCon, name = DBI::SQL('becmaster_zones'), value = zones,
  overwrite = TRUE)

# Write Pub Refs ----------------------------------------------------------
# 
# accessPath <- "data/tbl_BEC_References.accdb"
# accCon <- dbConnect(odbc::odbc(), .connection_string =
#     sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;",
#       accessPath))
# references <- DBI::dbReadTable(accCon, name = 'tblBEC_References')
# names(references)[1] <- 'id'
# names(references) <- tolower(names(references))
# DBI::dbAppendTable(pgCon, name = DBI::SQL('ava_canada_references'),
#   value = references)
# pubs <- data.table::fread('data/AllPublished_LMH_SU.csv')
# names(pubs) <- c('plot_number', 'site_unit', 'ref_short_name')
# Encoding(pubs$site_unit)
# pubs$site_unit <- iconv(pubs$site_unit, "latin1", "UTF-8")
# DBI::dbWriteTable(pgCon, name = DBI::SQL('ava_canada_link_refs_plots'),
#   value = pubs, overwrite = TRUE)


# Add Arctic CAVM ------------------------------------------------------

cavm <- sf::st_read('data/cavm_canada.gpkg') |> 
  sf::st_transform(4326) |>  st_zm(cavm)
names(cavm) <- tolower(names(cavm))
leaflet::leaflet(cavm) |> 
  leaflet::addTiles() |> 
  leaflet::addPolygons()
DBI::dbWriteTable(pgCon, value = cavm, name = DBI::SQL('CAVM'),
  overwrite = TRUE)

# Add CASBEC BGC-----------------------------------------------------------
# bgc <- sf::st_read('data/casbec_canada.gpkg') |> 
#   sf::st_transform(4326) |>  st_zm(bgc)
# names(bgc) <- tolower(names(bgc))
# leaflet::leaflet(bgc) |> 
#   leaflet::addTiles() |> 
#   leaflet::addPolygons()
# DBI::dbWriteTable(pgCon, value = bgc, name = DBI::SQL('CASBEC'),
#                   overwrite = TRUE)

# Provinces ---------------------------------------------------------------

provinces <- sf::st_read('data/canadian_provinces.gpkg')
names(provinces) <- tolower(names(provinces))
DBI::dbWriteTable(pgCon, value = provinces, name = DBI::SQL('Provinces'),
  overwrite = TRUE)

fg_areas <- sf::st_read('data/fieldguide_areas.gpkg')
names(fg_areas) <- tolower(names(fg_areas))
DBI::dbWriteTable(pgCon, value = fg_areas, name = DBI::SQL('Fieldguides'),
                  overwrite = TRUE)

# Grant Privileges --------------------------------------------------------

# DBI::dbExecute(pgCon, 
#   'GRANT SELECT ON ALL TABLES IN SCHEMA public to becmaster_reader')

# Fin ---------------------------------------------------------------------

dbDisconnect(accCon)
dbDisconnect(pgCon)

