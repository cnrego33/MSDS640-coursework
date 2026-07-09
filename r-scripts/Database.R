#===============================================================================
# Install packages and libraries needed 
#===============================================================================
# install.packages("RPostgres")
library(DBI)
library(RPostgres)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "SESA Project",
                 host = "localhost",
                 port = 5432,
                 user = "postgres", 
                 password = "sesaproject")

dbListTables(con)


#===============================================================================
# Load EBird Data into SESA Project database 
#===============================================================================
# set project root 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"

ebd_clean <- readRDS(file.path(project_root, "data", "ebd_clean.rds"))
dbWriteTable(con, "ebd_occurences", as.data.frame(ebd_clean), overwrite=TRUE)
dbListTables(con)

dbGetQuery(con, "SELECT COUNT(*) FROM ebd_occurences")

dbDisconnect(con)


#===============================================================================
# Load Hotspot Data into SESA Project database 
#===============================================================================
dbWriteTable(con, "hotspot_sites", as.data.frame(hotspot_sites), overwrite = TRUE)
dbGetQuery(con, "SELECT COUNT(*) FROM hotspot_sites")[[1]]
dbDisconnect(con)



#===============================================================================
# Load Habitat Data into SESA Project database 
#===============================================================================
dbWriteTable(con, "habitat_extracted", habitat_table, overwrite = TRUE)
dbGetQuery(con, "SELECT COUNT(*) FROM habitat_extracted")[[1]]
dbDisconnect(con)
