### this creates the file with all of the required datatables for functioning on the shiny servers
### run this if data is changed in the Vpro database of plots or other CAVM related spatial files

source("creds.R") # username, password for pg
pgCon <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = 'ava_canada', 
  host = Sys.getenv('PG_SM_HOST'), user = username, password = password)
gpkg <- 'app/becplots/ava_canada.gpkg'
for (pgTable in DBI::dbListTables(pgCon)) {
  sf::st_read(dsn = pgCon, layer = pgTable) |> 
    sf::st_write(dsn = gpkg, layer = pgTable, append = FALSE)
}
sf::st_layers(gpkg)
