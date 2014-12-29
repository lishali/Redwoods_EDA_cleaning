setwd(file.path("/Users/LishaLi/Desktop/215A/Lab1/data"))
library(dplyr)
library(ggplot2)
installpackage(infotheo)
library(grid)
library(infotheo)
library("scales")

data <- readRDS("final_cleaned.rds")
data <- tbl_df(data)
data
locs <- read.table('mote-location-data.txt', header=T)
locs <- arrange(locs, ID)

names(data)
#distribution of temperature: 

tempdist <- ggplot(data, aes(x=humid_temp))#+#geom_histogram(binwidth=.5,fill = "white", aes(color="blue"))
tempdist + geom_density(kernel = "normal")

ggplot(data, aes(x=temp))+geom_density(kernal = "rectangular", alpha=.5, fill="red")


colnames(data)[colnames(data) == 'humid_temp'] <- 'temp'
names(locs)[1] <- "nodeid"
data <- left_join(data, locs, by=c("nodeid"))
data <-tbl_df(data)
colnames(data)
data
save(data, file="lab2data.Rds")
load("lab2data.Rds")

summary(data)


summary(data$epoch)
data <- mutate(data, epoch.day=epoch%%288)
select(data, epoch, epoch.day)
summary(data$epoch.day)
data <- arrange(data, epoch.day)
select(data, epoch.day, epoch, humidity, temp)
summary(data$epoch.day)

epoch.0 <- filter(data, epoch.day==0)
epoch.0
epoch.0 <-arrange(epoch.0, epoch)
epoch.0.normed <- filter(epoch.0, epoch >3000 & epoch < 8000)
nrow(epoch.0.normed)
epoch.0.normed <- group_by()

epoch.141 <- filter(data, epoch.day==141, epoch > 3000& epoch < 8000 )
epoch.141
select(epoch.141, epoch.day)
#now let's plot the temperature accross the entire period of this same time period

ggplot(epoch.0, aes(x=epoch, y=temp))+geom_point()

ggplot(epoch.141, aes(x=epoch, y=temp))+geom_point(aes(colour=factor(nodeid)))+stat_smooth(method = "loess", formula = y ~ poly(x,2), size = 1)



#nodeid same number of observations between 3000 and 8000.  


#use nodeid and check that each day once per node.  

test <- group_by(epoch.0, nodeid) %>% mutate(node.id.count=n()) %>% arrange(-(node.id.count))
select(test, node.id.count)

test2 <- epoch.0


#we have 1040 nodes
filter(epoch.0, n)

plot.0 <- filter(test, epoch >3000 & epoch <8000 $nodeid <120 )
save(plot.0, file="plot_0.Rds")
load("plot_0.Rds")
load("plot_loess.Rds")
names(plot.0)
p <- ggplot(filtered, aes(x=time, y=temp))+geom_point(aes(colour=factor(nodeid)))
summary(plot.0$epoch)
save(p, file="plot_loess.Rds")

p+ stat_smooth(method = "loess", formula = y ~ poly(x,2), size = 1)

p+ stat_smooth(method = "loess", formula = y ~ poly(x,1), size = 1)+facet_wrap(~nodeid, ncol=2)

filtered <- filter(plot.0, nodeid==46 | nodeid==70 | nodeid==74 |nodeid==77 | nodeid==80 |nodeid==105 |nodeid==110 |nodeid==113 |nodeid==118 |nodeid==119)
save(filtered, "yea.Rds")

p1
