library(RJDBC)

drv <- JDBC("org.postgresql.Driver", "C:/sql_workbench/postgresql-8.4-703.jdbc4.jar")

# drv <- JDBC("org.postgresql.Driver", "C:/sql_workbench/postgresql-9.3-1102.jdbc4.jar") # also works

conn <- dbConnect (drv, "jdbc:postgresql://10.100.148.32:5432/dodobase", "fsu", "fsurocks")

tlist<-dbGetTables(conn) # gives the list of things in the dodobase

# test query - from a different table
test2 <- dbGetQuery(conn, "SELECT tag_id 
                    FROM mammals_neon.rmnp_2012_captures 
                    WHERE ear_tag_replaced 
                    Is Not Null")

head(test2)

# grab data, syntax = (server connection, schema.table) # soil pit schema = "biomass_neon"
# soil_biomass <- dbReadTable(conn, "biomass_neon.soil_pit_biomass")
soil_biomass2 <- dbReadTable(conn, "biomass_neon.soil_pit_biomass2") # updated data with no replicates
soil_horizons <- dbReadTable(conn, "biomass_neon.soil_pit_horizons")
soil_methods <- dbReadTable(conn, "biomass_neon.soil_pit_methods")
soil_properties <- dbReadTable(conn, "biomass_neon.soil_pit_properties")

# check duplicates
# 1) use duplicated() to flag rows; 2) summarize total_biomass to see if duplicates have same data
soil_biomass$dupe <- duplicated(soil_biomass)

library(plyr)

# check that masses are the same
soil_bio_check <- ddply(soil_biomass, .(site_id,profile,dupe), summarize, 
            tot_check = sum(total_biomass),
            fine_root_check = sum(fine_root_mass_g),
            coarse_root_check = sum(coarse_root_mass_g))

# split into true and false data frames, merge, then compare
soil_b1 <- soil_bio_check[which(soil_bio_check$dupe == "TRUE"),]
soil_b2 <- soil_bio_check[which(soil_bio_check$dupe == "FALSE"),]

# merge, or just compare directly
soil_bio_check2 <- data.frame(site_id = soil_b1$site_id, profile =soil_b1$profile,total_biomass_diff = soil_b1$tot_check - soil_b2$tot_check)
# quick summary - values should only be 0 if they are exactly the same
sum(soil_bio_check2$total_biomass_diff); range(soil_bio_check2$total_biomass_diff)

# proportional mass calculator - need to loop through each depth and it's mass plus the total mass from previous

prop_mass_depth <- function(df){
  # for each unique site_id, order the columns consecutively by depth
  # then create an empty variable that tracks total mass
}

View(soil_biomass)

# 
soil_biomass_nodupe <- soil_biomass[which(soil_biomass$dupe == "FALSE"),]
soil_bio <- ddply(soil_biomass2, .(site_id,profile),mutate, tot_live_biomass = sum(fine_root_mass_g,coarse_root_mass_g))

write.csv(soil_biomass2, file = "V:/Plants/Soil_pit_root_biomass/data/soil pit data/soil_pit_biomass_cf.csv")

### How to write a new table to the DoDoBASE - not sure how to delete data and update without deleting entire table
## dbWriteTable(conn, "biomass_neon.soil_pit_biomass2",value = soil_biomass2)
## 
