# import the data - pick the file with the popup window
library(dplyr)
df <- read.csv(file.choose(), header=T, stringsAsFactors = FALSE)
 
# leave out harvard
df2 <- dplyr::filter(df, site_id != "D01HARV")

# check duplicates for harvard only
check <- dplyr::filter(df, site_id == "D01HARV") # only grab the harvard sites
check$dupeFlag <- duplicated(check) # adds a column that tells you if it is duplicated or not
View(check)

df[c(1189:1261),]

# summarize 
library(plyr)
plyr::ddply(df2, .(site_id, profile, depth), summarize, 
            newLiveBiomass_g = sum(fine_root_mass_g,coarse_root_mass_g))

select(df, site_id, profile, upper_depth) %>% duplicated

