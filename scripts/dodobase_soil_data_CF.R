library(RJDBC)
library(plyr)

setwd("C:/Users/cflagg/Documents/GitHub/AvalonSoilProject/data/dodo_data")

# establish connection
drv <- JDBC("org.postgresql.Driver", "C:/sql_workbench/postgresql-8.4-703.jdbc4.jar")
# drv <- JDBC("org.postgresql.Driver", "C:/Users/ahoekspaans/Documents/Workbench-Build117/postgresql-8.4-703.jdbc4.jar")
conn <- dbConnect (drv, "jdbc:postgresql://10.100.148.32:5432/dodobase", "fsu", "fsurocks")

tlist<-dbGetTables(conn) # gives the list of things in the dodobase

# grab data, syntax = (server connection, schema.table)
soil_biomass <- dbReadTable(conn, "biomass_neon.soil_pit_biomass")
soil_horizons <- dbReadTable(conn, "biomass_neon.soil_pit_horizons")
soil_methods <- dbReadTable(conn, "biomass_neon.soil_pit_methods")
soil_properties <- dbReadTable(conn, "biomass_neon.soil_pit_properties")

# store the files in a list so we can plyr it
fileListing <- list(soil_biomass = soil_biomass, soil_horizons = soil_horizons, soil_methods = soil_methods
                    , soil_prop = soil_properties)


# doing this with l_ply is not straightforward:
# http://stackoverflow.com/questions/3548263/l-ply-how-to-pass-the-lists-name-attribute-into-the-function

# have to add name attribute to fileListing, which is different than just using names()
for(ename in names(fileListing)) {
  attr(fileListing[[ename]], "ename") <- ename
}

# then a function for assigning the list's name attribute to the individual plyr piece
ename <- function(x) attr(x, "ename") # states for element name

# export data from list to individual data frames - l_ply does not produce correct output
ldply(fileListing, function(input){
  cli_name = ename(input) # declare name with ename function
  write.csv(input, file = paste(cli_name,".csv", sep="")) # append to file
})




