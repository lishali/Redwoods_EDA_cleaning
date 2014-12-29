setwd(file.path("/Users/LishaLi/Desktop/215A/RedwoodsLab/Data_analysis"))
data <- readRDS("final_cleaned.rds")
data <- tbl_df(data)
data
library(dplyr)
library(ggplot2)
installpackage(infotheo)
library(grid)
library(infotheo)
library("scales")
#tables show if 
data

#let's re 


#don't forget to separate the trees

#rename humid_temp to temperature, drop humid_adj, and network topology variabls.  
colnames(data)[colnames(data) == 'humid_temp'] <- 'temp'

# pass 


#let's study some relationships
###################################################################################
#temporal data.  

summary(data["epoch"])

ggplot(data)+geom_histogram(aes(x=epoch), binwidth = 100)

ggplot(data)+geom_histogram(aes(x=epoch), binwidth = 200)

ggplot(data)+geom_histogram(aes(x=epoch), binwidth = 50)

#the above gives us a good sense of how many measurements were taken with time.  By this, we know 
#that we have approximately three time periods where data was collected.  A lot in the begining up until 2000 epoch time
#then mostly 


setwd(file.path("/Users/LishaLi/Desktop/215A/Lab1/data"))
locs <- read.table('mote-location-data.txt', header=T)
locs <- arrange(locs, ID)

#rename locs id to nodeid so I can do an inner join on id 

colnames(locs)[colnames(locs) == 'ID'] <- 'nodeid'
colnames(locs)

#combine locs data with my cleaned dataset

data <- left_join(data, locs, by=c("nodeid"))
data <-tbl_df(data)

data

summary(data)
#got some NAs for height, i.e information for certain nodes was not collected, but no big problem



data <- arrange(data, -(Height))
height.order <- c(unique(data$nodeid))

filter(test, nodeid ==78)
c(test$nodeid)
data$nodeid.height <- ordered(data$nodeid, levels =  height.order)

arrange(data, nodeid.height)
select(data, nodeid.height)
#yes this worked!

######################################################################################
######################################################################################
######################################################################################
######################################################################################


##################this is ##################################################################
#this is for lab 2
summary(data$epoch)
data <- mutate(data, epoch.day=epoch%%288)
select(data, epoch, epoch.day)
summary(data$epoch.day)
data <- arrange(data, epoch.day)
select(data, epoch.day, epoch, humidity, humid_temp)
summary(data$epoch.day)

epoch.0 <- filter(data, epoch.day==0)
epoch.0

epoch.141 <- filter(data, epoch.day==141)
epoch.141
#now let's plot the temperature accross the entire period of this same time period

ggplot(epoch.0, aes(x=epoch, y=humid_temp))+geom_point()

ggplot(epoch.141, aes(x=epoch, y=humid_temp))+geom_point(aes(colour=Height))

######################################################################################
######################################################################################
######################################################################################
######################################################################################

#let's look at the distribution of data with height.  

histo.height.epoch <- ggplot(na.exclude(data),aes(x=epoch,fill=as.factor(Height), order = -as.numeric(Height)))
histo.height.epoch + geom_histogram(binwidth = 200) + theme(legend.key.size = unit(0.3, "cm"))+scale_fill_discrete(name="Height")


histo.height.epoch + geom_histogram(binwidth = 200, position = "fill") + theme(legend.key.size = unit(0.3, "cm"))+scale_fill_discrete(name="Height")
summary(filter(data, epoch > 5000)$Height)
#let's look at the distribution of data with Direc.  

histo.direc.epoch <- ggplot(data,aes(x=epoch,fill=Direc))
histo.direc.epoch + geom_histogram(binwidth = 200) + theme(legend.key.size = unit(0.3, "cm"))


#alot of SW points, but by over 80 000 epoch, most all of the WNW, WSW points died, and W.  hence drop in their measured temperature
#by 3000 there was no E, ESE directions.  

######################################################################################

#look at temperature fluctuations with time, but according to height

temp.height <-ggplot(data, aes(x=epoch, y = temp))
graph2 <- temp.height + geom_point(aes(colour = Height))+ scale_colour_gradient(high = "yellow", low = "blue")

temp.height.top5000 <-ggplot(top.1000, aes(x=epoch, y = temp))
graph2.top <- temp.height.top5000 + geom_point(aes(colour = Height))+ scale_colour_gradient(high = "yellow", low = "blue")


humidity.height <-ggplot(top.1000, aes(x=epoch, y=humidity))
graph3 <- temp.height.top5000 + geom_point(aes(colour = Height))+ scale_colour_gradient(high = "green", low = "red")
graph3

top.1000 <- filter(data, count >5000)

#Again, not very indicative since the distributions are not similiar.  
#let's look at the sampled ones.  
 
top.1000 <- filter(data, count >5000)
sample.nodes <- sample(top.1000$nodeid, 12, replace=F)
sample.nodes

temp.height.top5000 <-ggplot(top.1000, aes(x=epoch, y = temp))
graph2.top <- temp.height.top5000 + geom_point(aes(colour = Height))+ scale_colour_gradient(low = "yellow", high = "blue")

#ordered, new variable nodeid.ordered, 

node.set.random <- filter(data, nodeid %in% sample.nodes) %>% arrange(desc(Height))
#arrange in decsending so that we can  see this with respect to 
temp.height.sample <-ggplot(node.set.random, aes(x=epoch, y = temp))
temp.height <- temp.height.sample  + geom_point(aes(colour=Height)) +facet_wrap(~ nodeid.height, ncol=1)
temp.height + xlim(0,2000)
temp.height + xlim(3500,4000)
temp.height + xlim(4000,4500)

temp.height + xlim(8000,10000)
temp.height + xlim(3000,8000)

temp.height + xlim(0, 8000)

#to be honest, not much correlation of height with temperature...


#how about not through all epoch, but begining versus end...

#this is the aggreagated data and is somewhat messy, let's just look at the datapoints with over 5000 observations

#my colouring was to indicate height as yellow, and low height as blue, interestingingly, it still shows correlations with temperature 


#recall that the measurement transitions of 2000 peoch and 10 000 epoch
#let's zoom down to when the height was most homogeneous
# it appears that the most extreme temperature changes happened near the bottom of the tree, but it is not by much as displayed by the linked plots

graph2 + xlim(0, 2000)

graph2 + xlim(2000, 10000)

graph2 + xlim(2000, 10000)

graph2 + xlim(5000, 10000) #near the end, it seems that the canopies are getting warmer, and the lower temperatures are enjoyed by the lower part of the tree.  This makes sense, as by that epoch, we are in full on summer (low os 20 highs of over 30) 
#confirm
 

#really confirms what I noticed above
graph2 + xlim(9000, 10000) 

#colour now by Direction instead of height

graph3 <-temp.height + geom_point(aes(colour = Direc, alpha = 1/1000))

#as per observation of distribution changes
graph3 + xlim(0,3000)

graph3 + xlim(5000, 8000)

#seems pretty homogeneous, E probably has more high points, but there are also a lot more S, SW points, so the data is mostly stringed together by it's measurements





######################################################################################

#humidity with height 

humid.height <-ggplot(data, aes(x = epoch, y = humidity))

graph4 <- humid.height + geom_point(aes(color=Height, alpha = 1/1000))+ scale_colour_gradient(high = "skyblue1")
 #the yellow choice of colour corresponds to canopy

graph4 + xlim(0, 10000)

graph4 + xlim(0, 2000)

graph4 + xlim(5000, 10000)

#humidity with direction (don't expect any correlation)

humid.direc <-ggplot(data, aes(x = epoch, y = humidity))

graph5 <- humid.height + geom_point(aes(color=Direc, alpha = 1/1000))

######################################################################################

#hamabot

hamabot.height <- ggplot(data, aes(x = epoch, y = hamabot))
graph6 <- hamabot.height + geom_point(aes(color = Height, alpha = 1))+ scale_colour_gradient(high = "orange", low = "blue")


hamatop.height <- ggplot(data, aes(x = epoch, y = hamatop))
graph7 <- hamabot.height + geom_point(aes(color = Height, alpha = 1))+ scale_colour_gradient(high = "orange", low = "blue")

graph7 + xlim(2000, 10000)

graph7 + xlim(0, 2000)

graph7 + xlim(1800, 2000)
#it's really werid the hamatop did nto drop to zero.... but I suppose there is still light in the forest
#canaopy had no light, but some other parts still had a bit of light even during night...
#most were not seeing light  
graph7 + xlim(5000, 10000)

hamatop.direc <- ggplot(data, aes(x = epoch, y = hamatop))
graph8 <- hamabot.height + geom_point(aes(color = Direc, alpha = 1))

graph8 + xlim(0,500)
#mostly SW nodes, S and W not much east, so this is not telling me much 


#the jumps just make me believe that the sensor could only measure at such quanta 
######################################################################################
######################################################################################
######################################################################################

#Let at the timeseries of some nodes, different locations..

count.nodeid.observations <- group_by(data, nodeid) %>% summarize(n=n()) %>% arrange(desc(n))
count.nodeid.observations

data <-group_by(data, nodeid) %>% mutate(count = n()) %>% arrange(desc(count))
data <-tbl_df(data)
data <- filter(data)
#create a df where we have the observation count
data
#  nodeid     n
1      42 12634
2     197 11594
3     127 11379
4       5 10072
5     118 10035

node.42 <- filter(data, nodeid == 42)

node.42 <- tbl_df(node.42)
node.42
node42.temp <- ggplot(node.42, aes(x=epoch, y = temp))
node42.temp.humidity <- ggplot(node.42, aes(x=humidity, y = temp))

node42.temp.humidity+geom_point(aes(color = voltage, alpha = 1/1000))
#no concern for correlation

node42.temp +geom_point(aes(colour = humidity))
#cool, temperature and humidity seems to be correlated.  

#let's check if this is true for some other nodes, spread against different heights

node.set <-filter(data, nodeid==42 | nodeid == 197 | nodeid == 127 | nodeid == 5 | nodeid == 118)

p <- ggplot(node.set, aes(x = temp, y = humidity, group = nodeid))

p+geom_point(aes(colour = Height, alpha = 1/500000)) + scale_colour_gradient(high = "orange") + geom_smooth(aes(group = nodeid), method = "lm", se = F)
#just to visually check the distribution of heights and Direct of the nodeids we are considering
p2 <- ggplot(node.set, aes(x = hamatop, y = temp, group = Height))
p2+geom_point(aes(colour = Height, alpha = 1/500000)) + scale_colour_gradient(high = "orange") + geom_smooth(aes(group = nodeid), method = "lm", se = F)
#ok, that was a terrible distribution

p3 <- ggplot(node.set, aes(x = hamabot, y = temp, group = Height))
p3+geom_point(aes(colour = Height, alpha = 1/500000)) + scale_colour_gradient(high = "orange") + geom_smooth(aes(group = nodeid), method = "lm", se = F)


ggplot(node.set, aes(x = Height, y = nodeid))+geom_point(aes(colour= Direc))

#let's more or less be random, since I last checked, nodeid and height distribution are completely uncorrelated

node.100.130 = filter(data, nodeid >100 & nodeid < 130 )
ggplot(node.100.130, aes(x = Height, y = nodeid))+geom_point(aes(colour= Direc))
humid.temp <- ggplot(node.100.130, aes(x = temp, y = humidity, group = nodeid))
humid.temp + geom_point(aes(colour = nodeid, alpha = 1/500000)) + scale_colour_gradient(high = "orange") + geom_smooth(aes(group = nodeid), method = "lm", se = F)
 #114 not very good as there are not very much datapoints, so is 109


#interesting for me since it siganals that higher humidity and low temperatures, perhaps sheltered? 

#how does humidity relate to height?  

#Why don't I just sample a few rows from the nodes that have more than 5000 observations
#######################################################################
#I randomly sample 16 points with over 5000 observations, and then graph them.  

#the correlation is incredibly robust  
top.7000 <- filter(data, count >7000)
sample.nodes <- sample(top.7000$nodeid, 15, replace=F)
sample.nodes

node.set.random <- filter(data, nodeid %in% sample.nodes) %>% arrange(desc(Height))
#arrange in decsending so that we can  see this with respect to 
select(node.set.random, Height)
humid.temp <- ggplot(node.set.random, aes(x=temp, y = humidity))
humid.temp + geom_point(aes(colour=Height, alpha=1/500000)) + geom_smooth(aes(group = nodeid), method = "lm", se = T)+facet_wrap(~ nodeid.height)+scale_colour_gradient(high = "orchid")
##great!






humid.temp + geom_point(aes(colour=Height)) + geom_smooth(aes(group = nodeid), method = "lm", se = F)+facet_wrap(~ nodeid.height)
#that's cool  


#homatop hamabot correlated

#######################################################################
#######################################################################
#######################################################################

#hamatop versus temperature


node.set.random
hamatop.temp <- ggplot(node.set.random, aes(x=hamatop, y = temp))
hamatop.temp + geom_point(aes(colour=Height)) + geom_smooth(aes(group = nodeid), method = "lm", se = F)+facet_wrap(~ nodeid.height)
#######################################################################
#######################################################################
#######################################################################




#hamabot versus temperater

#not so well correlated
node.set.random
hamabot.temp <- ggplot(node.set.random, aes(x=hamabot, y = temp))
hamabot.temp + geom_point(aes(colour=Height)) + geom_smooth(aes(group = nodeid), method = "lm", se = F)+facet_wrap(~ nodeid.height)
#######################################################################
#######################################################################
#######################################################################
#epoch versus hamabop

#we will restrict the epochs since we know that a small frame is nicer and more informative for hamatops

#i.e the info for one day
#cool, clearly the higher the more light tips they get
hamabot.epoch <- ggplot(node.set.random, aes(x=epoch, y = hamabot))
hamabot.epoch + geom_point(aes(colour=Height)) + facet_wrap(~ nodeid.height) + xlim(3000, 3500)
hamabot.epoch + geom_point(aes(colour=Height),) + facet_wrap(~ nodeid.height, ncol = 1) + xlim(3000, 8000)+scale_colour_gradient(high = "gold2")

hamabot.epoch + geom_point(aes(colour=Height), colour = "orange") + facet_wrap(~ nodeid.height, ncol = 1) + xlim(7500, 8000)


#Kelvin colour units  
#the above does better if we don't sample so many points

#nice, same!



#how about hamatop

hamatop.epoch <- ggplot(node.set.random, aes(x=epoch, y = hamatop))
hamatop.epoch + geom_point(aes(colour=Height)) + facet_wrap(~ nodeid) + xlim(3000, 3500)
hamatop.epoch + geom_point(aes(colour=Height)) + facet_wrap(~ nodeid.height, ncol = 1) + xlim(3000, 8000)

data.high <- filter(data, Height >50)
nrow(data.high) #138437
data.med <- filter(data, Height>30 & Height <50)
nrow(data.med) #127809
data.low <- filter(data, Height>10 & Height <30)
nrow(data.low) #35447  so need to change transparency of first two plots to make pictre comparable...
35447/138437==0.2560515
35447/127809
#data.Height <- mutate(data1, Height.level = )

hamatop.all <- ggplot(na.omit(data), aes(x=epoch, y = hamatop, alpha = 0.0001))+scale_colour_gradient(high = "skyblue1", low = "orangered4")
hamatop.all + geom_point(aes(colour=Height))+xlim(3000, 8000)
#Look at only homabot in high regions, versus hamabot in low regions. 
hamatop.high <- ggplot(data.high, aes(x=epoch, y = hamatop, alpha = 0.25))

hamatop.high + geom_point(aes(colour=Height))
#played around with different transparencies, but the picture is still clear, the top recieved more ultra light
hamatop.med <- ggplot(data.med, aes(x=epoch, y = hamatop, alpha = 0.27))
hamatop.med + geom_point(aes(colour=Height))

hamatop.low <- ggplot(data.low, aes(x=epoch, y = hamatop))
hamatop.low + geom_point(aes(colour=Height))

#ok, need to have similar number of points.  
#hamatop however definitely has more uniform distribution regardless of height.  
#not sure also if the sparsity of lower points has to do with not having that many observations



update.packages(checkBuilt=TRUE)
install.packages('ggplot2', dependencies = TRUE)
y
#######################################################################

#box plots?  

library(ggplot2)
boxplot.hamatop <- ggplot(data.high, aes(x=epoch, y = hamatop, alpha = 0.25))
####binning values

data$binned.height <- cut(data$Height, c(10, 20, 30, 40, 50, 60, 70))
summary(data$bin)

dataepochfilter <- filter(data, epoch >3000)
hamabot.dis <- ggplot(dataepochfilter, aes(binned.height, hamabot))

hamabot.dis + geom_boxplot(aes(fill = Tree), outlier.colour = alpha("yellow", 0.05), outlier.size = 3) 


#############

hamatop.dis <- ggplot(na.omit(dataepochfilter), aes(binned.height, hamatop))

hamatop.dis + geom_boxplot(aes(fill = Tree), outlier.colour = alpha("gold1", 0.05), outlier.size = 3) 
###################### awesome that works!!!!!!!!
