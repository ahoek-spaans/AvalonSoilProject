getwd()
setwd("C:/Users/ahoek-spaans/Documents/Github/AvalonSoilProject/data/dodo_data")
soil_biomass<-read.csv("soil_biomass.csv",header = TRUE,stringsAsFactors = FALSE)
soil_biomass
head(soil_biomass)
soil_prop <- read.csv("soil_prop.csv",header=TRUE,stringsAsFactors = FALSE)
names(soil_prop)
soiltable <- merge(soil_biomass,soil_prop, by=("site_id"))
soiltable     
names(soiltable)
library(ggplot2)
library(dplyr)

df <- soiltable
# a way to choose a file manually : 
# df <- read.csv(file.choose(), header=T)
head(df)

x <- df[,"depth.y"]

head("depth.y")
y <- as.numeric(as.character(df[, "mg_cm_3"]), stringsAsFactors = FALSE)
head("root_prop")

y <- df[, 23]

library(lattice)
library(nlme)
library(MASS)
#not sure why xyplot isnt working
xyplot(depth ~  root_prop| site_id, data=df, ylab = "upper_depth", xlab = "root_prop_of", main = "biomass at depth for 29 NEON sites", layout =c(6,5), ylim = c(200,0), pch = 16, col =1,
       panel.xyplot("root_prop", "depth.y"))
       beta <- nlsList("root_prop" ~ 1-B^("depth.y") | "site_id", start = list(B=0.9), data = subset(df, select=c("site_id", "profile", "depth.y", "root_prop")))
       model <- nlme("root_prop" ~ 1-B^("depth.y") | "site_id", "model" = "ML",  data =subset(df, select=c("site_id", "profile", "depth.y", "root_prop")), groups = "site_id") 
       panel=panel.superpose(beta, panel.groups=function(beta)
         limits <- seq(0, 200),
         line <- lines(limits, predict("beta"), data.frame(x=limits)
         fit.fun <- function(lines)predict(beta)
         panel.curve(fit.fun)


Y = 1-B^(d) #B (beta) is a numerical index of rooting distribution, where d=depth, and Y=the proportion of roots from the surface to depth. 

step(AIC)

beta <- nlsList(root_prop_of_total ~ 1-B^("depth.y") | site_id, start = list(B=0.9), data = subset(df, select=c("site_id", "profile", "depth.y", "root_prop_of_total")))

beta <- nlsList("root_prop" ~ 1-B^(depth) | "site_id", start = list(B=0.8), data = fig1)
m1 = nls(lignin~a*exp(-b*aromatic)+c, start=list(a=0.4, b=19.6, c=0.05), data=fig3)

data = subset(df, select=c(site_id, depth, root_prop_of_total))









lm(y~x, subset(df, subset=(site_id == "D01HARV")))
   
   lot(x,y, subset(df,site_id== "D01HARV"))
       
       plot(lm(y~x))
       
       HARV <- subset(df, select=c(depth, mg.cm.3), subset=(site_id == "D01HARV")
       
       x1 <- HARV[, "depth.y"]  
       y1 <- HARV[, "mg.cm.3"]     
       plot(x1,y1)
       
       site_id <- df2[, 1]
       
       HARV <- subset(df, select=c(depth, mg.cm.3), subset=(site_id == "D01HARV"))
       
       
       
       
       plotall <- function(site_id){
         df <- soiltable
         #site_ids <- df[, 1]
         df2 <- subset(df, select=c(site_id, depth, mg.cm.3))
         df3 <- split(df2, df2$site_id) #divides the data frame into smaller data frames broken out by site. 
         x <- as.numeric(df3[, "depth.y"])
         y <- as.numeric(df3[, "mg.cm.3"])
         
         
         
         
         
         
         
       }
       
       site_id = "D01HARV"
       