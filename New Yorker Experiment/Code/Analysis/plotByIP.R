library(maps)
library(maptools)
data(stateMapEnv)

?map

# trying out the example from the documentation
library(RgoogleMaps)
mymarkers <- cbind.data.frame(lat = c(38.898648,38.889112, 38.880940),
lon = c(-77.037692, -77.050273, -77.03660), size = c("tiny","tiny","tiny"),
col = c("blue", "green", "red"), char = c("","",""));

bb <- qbbox(lat = mymarkers[,"lat"], lon = mymarkers[,"lon"])

MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "DC.jpg", GRAYSCALE =T,
markers = mymarkers);

zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR));

png("OverlayTest.jpg",640,640);
tmp <- PlotOnStaticMap(MyMap,lat = mymarkers[,"lat"], lon = mymarkers[,"lon"],
cex=1.5,pch=20,col=c("blue", "green", "red"), add=F);
tmp <- PlotOnStaticMap(MyMap,lat = mymarkers[,"lat"], lon = mymarkers[,"lon"],
col=c("purple"), add=T, FUN = lines, lwd = 2)
dev.off()


# trying it with my data
data = as.data.frame(read.csv("/Users/ksauer/Desktop/MTurk/new-yorker-expt/ips_with_location.csv"))
colnames(data)

markers = cbind.data.frame(lat=data$latitude, lon=data$longitude, size=rep("small", nrow(data)), col=rep("red", nrow(data)), char=rep("1", nrow(data)))
dim(markers)

bb <- qbbox(lat = markers[,"lat"], lon = markers[,"lon"])

kristal.Map <- GetMap.bbox(bb$lonR, bb$latR, destfile = "~/Desktop/map.jpg");

kristal.map = GetMap(zoom=0)

zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR));


jpeg("~/Desktop/OverlayTest.jpg",640,640);
tmp <- PlotOnStaticMap(kristal.map,lat = markers[,"lat"], lon = markers[,"lon"],
cex=1.5,pch=20,col=markers[,"col"], add=F);
dev.off()

plot(data$latitude, data$longitude)