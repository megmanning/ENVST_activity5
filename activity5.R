#first time using package you have to install it
install.packages("ggplot2")
library(ggplot2)

ggplot(data = pr,
       aes(x = year,
           y = totalP,
           color = NAME))+
  geom_point()+
  geom_path()

#read in weather station from the data folder
datW <- read.csv("/Users/margaretmanning/Desktop/Data Science/a02/noaa2011124.csv")

#specifiy that the name column should be a factor
datW$NAME <- as.factor(datW$NAME)

#set up a vector of all names for each level 
nameS <- levels(datW$NAME)
nameS

#make a dataframe
datP <- na.omit(data.frame(PRCP = datW$PRCP,
                   NAME = datW$NAME,
                   year = datW$year))
#total annual precip for each site, you must make an aggregate function first
precip <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum", na.rm = TRUE)

#total annual precip
precip <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="sum", na.rm = TRUE)
colnames(precip) <- c("NAME", "year", "totalP")

#add the x column from aggregate looking at the length of obs in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x

#make a new dataframe 
pr <- precip[precip$ncount >=364, ]

#look only at livermore and morrisville precip
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

#make a plot of california precip
plot(ca$year, ca$totalP)

#make a plot of california precip
plot(pr$year, pr$totalP)

#make a plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual Precipitation (mm)",
     xlab = "Year")

#make a plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual Precipitation (mm)",
     xlab = "Year",
     yaxt = "n")
#add y axis
#arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
#las = 2 changes the labels to be read in horizontal direction
axis(2, seq(200,800, by=200), las=2)

#add new york (morrisvile) to plot using points function
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

#change axes so range of points is visible 
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual Precipitation (mm)",
     xlab = "Year",
     yaxt = "n",
     ylim = c(0,1600))
#add y axixs
axis(2, seq(0,1600, by=400), las=2)
#add NY
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

#add legend 
legend("topleft", #position
       c("California", "New York"), #labels
       col = c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1
       bty="n") #always use this argument otherwise an ugly box is drawn 

#make a plot comparing MAP for Morrisville and Mandan
#look at Mandan
ma <- pr[pr$NAME == nameS[3], ]
plot(ny$year, ny$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual Precipitation (mm)",
     xlab = "Year",
     yaxt = "n",
     ylim = c(0,1600))
#add y axixs
axis(2, seq(0,1600, by=400), las=2)
#add NY
points(ma$year, ma$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

legend("topleft", #position
       c("New York", "Mandan"), #labels
       col = c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1
       bty="n") #always use this argument otherwise an ugly box is drawn 

#first time using package you have to install it
install.packages("ggplot2")
library(ggplot2)

ggplot(data = pr,aes(x = year,y = totalP, color = NAME))+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="Year", y="Annual Precipitation")+ #make axis labels
  theme_classic()+ #change plot theme
  scale_color_manual(values = c("coral1", "cadetblue1", "darkgreen", "lightsalmon", "palevioletred1"))

#making a violin plot
ggplot(data=datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933, 0.953, 0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2, size=0.25, fill="grey90")+ #add grey boxxplots and make them ~20% smaller than normal with 25% thinner lines than normal
  theme_classic() #get rid of ugly gridlines

#daily patterns within a year 
sub <- datW[datW$NAME == nameS[4] & datW$year == 1974,]

#specify date format
#%Y means a four number year 
#- indicates that the date uses dashed to separate
#%m means month
#%d means day 
sub$DATE <- as.Date(sub$DATE, "%Y-%m-%d")

#make a plot now that date is specified 
ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(xx="Year", y="Maximum Temperature (C)")

#barplots 
ggplot(data = sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="Year", y="Daily Precipitation (mm)")

#make a plot for precip and max temp in 1974 for Aberdeen
abd <- datW[datW$NAME == nameS[1] & datW$year == 1974,]
abd$DATE <- as.Date(abd$DATE, "%Y-%m-%d")

#daily precip
ggplot(data = abd, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="Year", y="Daily Precipitation (mm)")

#maximum temp
ggplot(data=abd, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="Year", y="Maximum Temperature (C)")

#compare daily minimum temps over 2 decades
#first name site
liv <- datW[datW$NAME == nameS[2] & datW$year >= 2000,]
liv$DATE <- as.Date(liv$DATE, "%Y-%m-%d")

#average per year tmin
tmin <- aggregate(liv$TMIN, by=list(liv$NAME, liv$year), FUN="mean", na.rm=TRUE)
colnames(tmin) <- c("NAME", "YEAR", "TMIN")
ggplot(data=tmin, aes(x=YEAR, y=TMIN))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="Year", y="Minimum Temperature (C)", title="Average Yearly Minimum Temperature Livermore,CA")
