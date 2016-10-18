
setwd("C:\Users\bharathkumar\Desktop\Predictive analysis")
getwd()


a <- 10
b <- c(2, 3, 4)
length(a)
length(b)

# plot
c <- seq(from = 3, to = 20)
c
plot(c)

# random sample of numbers from the list and plotS
d <- sample(c, size = 10)
d
plot(d)

# normally distributed and plot it
e <- rnorm(n = 10000, mean = 2.5, sd = 0.1)
hist(e)
plot(density(e))



lga.profile <- read.csv("Vic 2013 LGA Profiles NoPc1.csv")
lga.profile$LGA
lga.profile$LGA[1:79]


lga <- lga.profile$House.21[1:79]
health<- lga.profile$Health.15[1:79]
medical <- lga.profile$Medical.3[1:79]


health
plot(health)
hist(health)

medical
plot(medical)
hist(medical)
medical[1]
is.numeric(medical[1])


sub("%", "","78.234%",fixed=TRUE)
sub("%", "","78.234%",fixed=TRUE)/100
as.numeric(sub("%", "","78.234%",fixed=TRUE))/100
medical <- as.numeric(sub("%", "",lga.profile$Medical.1[1:79],fixed=TRUE))/100
plot(medical)
hist(medical)


plot(x=health, y=medical, col="red", main="Health vs Medical")
boxplot(health)
boxplot(medical)


mat <- cbind(health, medical)
mat
mat[2:5, 1:2]
mat[2, 2]
mat[2,]


cu <- data.frame(health, medical)
cu
mean(cu$health)
max(cu$medical)
mean(cu$medical)


plot(x = mat[,1], y=mat[,2], main="Health vs medical",
     xlab="Health", ylab="medical / 1000" )


plot(cu, col="blue", main="Health vs medical",
     xlab="Health", ylab="Medical / 1000" )
abline(h=mean(cu$medical), col="orange")


plot(cu, col="red", main="Health vs medical",
     type="n", xlab="Health", ylab="Medical / 1000" )
segments(x0=cu$health, y0=cu$medical, x1=cu$health, 
         y1=rep(mean(cu$medical), length(cu$medical)), 
         col=rgb(0,0,1,0.3))
abline(h=mean(cu$medical), col="orange")
points(cu, col=rgb(0,0,1,0.5), pch=20, cex=1)


install.packages("calibrate", dependencies = TRUE)
library(calibrate)


max(medical)
match(max(medical), medical)
lga[match(max(medical), medical)]

medical.highest <- match(max(medical), medical)
textxy(cu$health[medical.highest]-0.001, 
       cu$medical[medical.highest]-10, 
       lga[medical.highest], col="red", cex=0.7)



install.packages("ggmap", dependencies = TRUE)
library("ggmap")


map.coords <- read.csv(text=readLines("Vic 2013 LGA Profiles NoPc1.csv")[(1:80)])
map.coords[(1:5),]
is.data.frame(map.coords)
map.coords["Crime"] <- medical
map.coords[(1:5),]


map <- qmap("Melbourne, Victoria, Australia", zoom=8, maptype="terrain")

# Plot the map with all points
map + 
  geom_point(data=map.coords, 
             aes(x=Longitude, y=Latitude, size=Crime), 
             color="red", alpha=0.3) +
  scale_size_continuous(range = c(3, 25), 
                        breaks=c(50, 100, 150, 200))





unk.names <- c("Pie in the Sky", "Great Surfing", "Middle of Nowhere", "Bonnie Doon", "Gooram", 
               "Euroa", "Goulburn River", "Korweinguboora")
unk.lats <- c(-37.855596, -38.351386, -37.733653, -37.026837, -36.892962, -36.752250, -36.785253, -37.456643)
unk.longs <- c(145.365799, 144.300879, 145.166513, 145.880146, 145.610981, 145.569783, 145.144062, 144.135963)
unk.crimes <- c(15, 50, 20, 50, 100, 80, 10, 150) # Note used
unk.places <- data.frame(Names=unk.names, UnkLat=unk.lats, UnkLong=unk.longs, Crime=unk.crimes)
unk.places

# Create a simple k-NN classifier
install.packages("class", dependencies = TRUE)
library(class)


lga.locs <- cbind(map.coords$Longitude, map.coords$Latitude) #, map.coords$Crime) # Add crimes here
lga.names <- factor(map.coords$LGA)


place.locs <- cbind(unk.places$UnkLong, unk.places$UnkLat) #, unk.places$Crime) # Add crimes here

# Let us use k-nn classifier to find the closest LGA, try k=1-5 
close.lga.names <- knn(lga.locs, place.locs, lga.names, k = 1, prob=TRUE, use.all = TRUE)
close.lga.names

# Create a full description of the new location with their LGA details
place.descr <- cbind(unk.places,map.coords[close.lga.names,])
place.descr

# Get the map of victoria, try maptype satellite or hybrid
map <- qmap("Melbourne, Victoria, Australia", zoom=8, maptype="terrain")

# Plot the map with all points
map + 
  geom_point(data=map.coords, 
             aes(x=Longitude, y=Latitude, size=Crime), 
             color="red", alpha=0.3) +
  geom_point(data=place.descr, 
             aes(x=UnkLong, y=UnkLat, size=Crime), 
             color="blue", alpha=0.7) +
  geom_segment(data=place.descr, 
             aes(x=UnkLong, y=UnkLat, xend=Longitude, yend = Latitude),
             color="blue", alpha=0.7) +
  scale_size_continuous(range = c(3, 25), 
                        breaks=c(50, 100, 150, 200))
  
