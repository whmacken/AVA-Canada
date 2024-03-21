source("creds.R") # username, password for pg
pgCon <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = 'ava_canada', 
  host = Sys.getenv('PG_SM_HOST'), user = username, password = password)
gpkg <- 'app/becplots/ava_canada.gpkg'
for (pgTable in DBI::dbListTables(pgCon)) {
  sf::st_read(dsn = pgCon, layer = pgTable) |> 
    sf::st_write(dsn = gpkg, layer = pgTable)
}
sf::st_layers(gpkg)
