# This program cleans publicly available FDA TIMS violations data
# hautahi

# Packages
library(dplyr)

# Load FDA Inspections data
fda15 <- read.csv("./data/2015/publicOCE_y2015_miYes_XY_finalAfterRound2.csv",stringsAsFactors = F) %>% filter(scrubber=="ZP4")
fda16 <- read.csv("./data/2016/publicOCE_y2016_miYes_XY_finalAfterRound2.csv",stringsAsFactors = F) %>% filter(scrubber=="ZP4")

fda15 <- fda15 %>% mutate(Violation=ifelse(Decision.Type=="No Violations Observed","No Violations","Violation"))
fda16 <- fda16 %>% mutate(Violation=ifelse(Decision.Type=="No Violations Observed","No Violations","Violation"))

# Load state level shapefiles
library(rgdal)
stateshp <- readOGR(dsn = "data/state_shp", layer = "cb_2016_us_state_5m")

# Extract DC state polygons
wash <- stateshp[stateshp$GEOID=="11",]
cal <- stateshp[stateshp$GEOID=="06",]
flo <- stateshp[stateshp$GEOID=="12",]

# Make fda data spatial
coordinates(fda15) <- ~ X+Y
proj4string(fda15) <- proj4string(stateshp)
fda15@data$x <- data.frame(fda15@coords)$X
fda15@data$y <- data.frame(fda15@coords)$Y

coordinates(fda16) <- ~ X+Y
proj4string(fda16) <- proj4string(stateshp)
fda16@data$x <- data.frame(fda16@coords)$X
fda16@data$y <- data.frame(fda16@coords)$Y

# Extract the inspections within DC
wash15 <- fda15[wash, ]
wash16 <- fda16[wash, ]

# Extract the inspections within California
cal15 <- fda15[cal, ]
cal16 <- fda16[cal, ]

# Extract the inspections within Florida
flo15 <- fda15[flo, ]
flo16 <- fda16[flo, ]

# COnvert points into dataframe for plots
dwash15 <- wash15@data
dwash16 <- wash16@data
dcal15 <- cal15@data
dcal16 <- cal16@data
dflo15 <- flo15@data
dflo16 <- flo16@data

# ------------------------
# Plot maps
# ------------------------

library(ggplot2)

format <- theme_bw()+ theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
                  plot.title = element_text(lineheight=2, size=16,face="bold",hjust = 0.5),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  legend.title = element_blank(),
                  legend.position="bottom",
                  legend.text=element_text(size=13)) 

png("2015cal.png")
ggplot() + geom_polygon(data=cal,aes(x=long, y=lat, group = group),colour="white")+ format +
  geom_point(data=dcal15,aes(x=x, y=y,color=Violation))+
   coord_equal() + ggtitle("2015 Inspections")+ scale_color_brewer(palette="Dark2")
dev.off()

png("2016cal.png")
ggplot() + geom_polygon(data=cal,aes(x=long, y=lat, group = group),colour="white")+ format +
  geom_point(data=dcal16,aes(x=x, y=y,color=Violation))+
  coord_equal() + ggtitle("2016 Inspections")+ scale_color_brewer(palette="Dark2")
dev.off()

png("2015flo.png")
ggplot() + geom_polygon(data=flo,aes(x=long, y=lat, group = group),colour="white")+ format +
  geom_point(data=dflo15,aes(x=x, y=y,color=Violation))+
  coord_equal() + ggtitle("2015 Inspections")+ scale_color_brewer(palette="Dark2")
dev.off()

png("2016flo.png")
ggplot() + geom_polygon(data=flo,aes(x=long, y=lat, group = group),colour="white")+ format +
  geom_point(data=dflo16,aes(x=x, y=y,color=Violation))+
  coord_equal() + ggtitle("2016 Inspections")+ scale_color_brewer(palette="Dark2")
dev.off()

# ------------------------
# Plot with open maps
# ------------------------

format1 <- theme_bw()+ theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
                            plot.title = element_text(lineheight=2, size=16,face="bold",hjust = 0.5),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.border = element_blank(),
                            legend.title = element_blank(),
                            legend.position="bottom",
                            legend.text=element_text(size=13)) 

library(ggmap)
gflo <- get_map(location="Florida",maptype = "roadmap", source = "google",zoom=7)

png("2015flo_b.png")
ggmap(gflo) + geom_point(data=dflo15,mapping=aes(x=x, y=y,color=Violation))+format1+ scale_color_brewer(palette="Dark2")+ggtitle("2015 Inspections")
dev.off()

png("2016flo_b.png")
ggmap(gflo) + geom_point(data=dflo16,mapping=aes(x=x, y=y,color=Violation))+format1+ scale_color_brewer(palette="Dark2")+ggtitle("2016 Inspections")
dev.off()

gcal <- get_map(location=cal@bbox,maptype = "roadmap", source = "google",zoom=6)
png("2015cal_b.png")
ggmap(gcal) + geom_point(data=dcal15,mapping=aes(x=x, y=y,color=Violation))+format1+ scale_color_brewer(palette="Dark2")+ggtitle("2015 Inspections")
dev.off()

png("2016cal_b.png")
ggmap(gcal) + geom_point(data=dcal16,mapping=aes(x=x, y=y,color=Violation))+format1+ scale_color_brewer(palette="Dark2")+ggtitle("2016 Inspections")
dev.off()





# ------------------------
# Archive
# ------------------------

# ggplot stuff
library(maptools); library(plyr)
# wash15 <- wash15[!is.na(wash15$black),]
# wash15@data$id = rownames(wash15@data)
# wash15.points = fortify(wash15, region="id")
# wash15.df = join(wash15.points, wash15@data, by="id")

plt_15 <- ggplot(wash15.df) + 
  aes(long,lat,group=group,fill=black) + 
  geom_polygon()+geom_path(color="white",size=0.5)+theme_bw()+
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(lineheight=2, size=20,face="bold",hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=13))+
  coord_equal() + ggtitle("2015 ACS")

win.metafile("2009ACSDC_black.wmf")
plt_9
dev.off()

win.metafile("2015ACSDC_black.wmf")
plt_15
dev.off()