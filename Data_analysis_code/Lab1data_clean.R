library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
options(warn=0)
#remember that you load plyr when you needed the rename function, however needed to manually unload it 
# in order to get summarise working again.  
detach("package:plyr", unload=TRUE)
detach("package:scale", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
###########################################################################################
###########################################################################################
#OUTLINE 

#PART I : Filter log and net for outliers
#log (line 44)
#net (line 256)

#PART II: amalgamation of datasets
#multiples deletion (line 466) from net and log
#calibration (line 1000)
# amalgamation of the 3 datasets  (line 1135)

#PART III: locs data look (line 1117)

#PART IV: Evaluation of their outlier rejection method (line 456)

###########################################################################################
###########################################################################################

setwd(file.path("/Users/LishaLi/Desktop/215A/Lab1/data"))
log <- read.csv('sonoma-data-log.csv',header=T)
net <- read.csv('sonoma-data-net.csv',header=T)
all <- read.csv('sonoma-data-all.csv', header=T)
locs <- read.table('mote-location-data.txt', header=T)

log <- tbl_df(log)
net <- tbl_df(net)
all <- tbl_df(all)
locs <- tbl_df(locs)

#Range for each sensor: 
#Sensor Min Median±Quartile Max
#Temperature (C) 6.6 14.1 ± 3.7 32.6
#Humidity (%RH) 16.4 65.5 ± 17.9 100.2
#Incident PAR 0 38.4 ± 84.8 2154
#Reflected PAR 0 0 ± 0 180

#I'm going to go through each column of both net and log dataset, and check for abnormalities (any negative values...etc)
#i.e check distribution, a priori assuming normality, and continuous like values (for instance, I don't expect the temperature to fluate 20 degrees between two subsequent epochs..)
######################################################################################
######################################################################################
######################################################################################
#log
######################################################################################
#humidity

colnames(log)

#humidity
ggplot(log)+geom_point(aes(x=epoch, y=humidity))
#took a while to plot, but seems to be a reasonably distirbution, aside from some negative values
#let's see what's going on there: 

nega.humid <- filter(log, humidity <0)
group_by(nega.humid, nodeid)%>%summarise(n=n())

#we have found 3 suspicious nodeid that have caused this, let's see if all values associated with them are funny

node.29 <- filter(log, nodeid==29)
summary(node.29)

ggplot(node.29) + geom_point(aes(x=epoch, y=humidity)) #constant
ggplot(node.29) + geom_point(aes(x = epoch, y = humid_temp)) #constant
ggplot(node.29) + geom_point(aes(x = epoch, y = hamatop))
ggplot(node.29) + geom_point(aes(x = epoch, y = hamabot))
#well, that settles it, nodeid 29 produced constant humidty data, hence definitely an error.  
#let's go ahead and delete all of node 29's output humidity and temperature output (set to NA)

log[log["nodeid"] == 29, ]["humidity"] <- NA
log[log["nodeid"] == 29, ]["humid_temp"] <- NA


#let's check out node 198

node.198 <- filter(log, nodeid==198)

ggplot(node.198)+geom_point(aes(x=epoch, y=humidity))
#looks pretty normal, just that one outlier, so let's go ahead delete it safely
#infact, it looks like it took a lot of crazy readings aside from negative humidity (nega temp, insane hamatop...etc)

filter(log, nodeid==198, humidity<0)

#so delete

log <- subset(log,humidity!=-5145.10)

#finally let's look at node 65535

filter(log, nodeid==65535)
#this guy's measurements are also off the charts (ridiculously high tempature, too high depth...we can delete), but only one datapoint, so we just deleted it

log <- filter(log, humidity >-1)

#alright, just to be safe, let's check the summary for log's humidity data

summary(log["humidity"]) #looks quite normal

ggplot(log)+geom_point(aes(x=epoch, y=humidity)) #looks like it is varying by day, so quite normal
#and good for timeseries later

######################################################################################
#humid_temp

ggplot(log)+geom_point(aes(x=epoch, y=humid_temp))
#looks quite normal, we did a good job getting rid of the faulty nodes in the previous step.  
#There is no reason to reject sub zero temperatures, even they existed, though that is suspicisou for spring in Califnornia
summary(log["humid_temp"])
#completely reasonable mean, min, max....etc for temperature, as to be expected in californai in the spring (checking online for averages for the time)

######################################################################################
#hamatop (top: incident PAR)

ggplot(log)+geom_point(aes(x=epoch, y=hamatop))

summary(log["hamatop"])

#ok, so most are in reasonable range, something werid is happening the drive the max really high
#also, it's sad the median value is 0, but this was explained by Night!
# I am expecting the light distribution to at least be connected, as in the world, light 
#intensity changes quite continuusly, so the outliers are very suspicicous


hamatop.high <-filter(log, hamatop >150000)
nrow(hamatop.high) #59 werid measurements, let's check their stats

summary(hamatop.high)
#ok, all produced by nodeid 40, voltage within normal, humidity normal, temperature reasonable, 

#hamabot and hamatop make no sense should at least be related, but some are zero.  Unless there was
#something that did not allow light to penetrate near the bottom of the mote where hamabot was measured but only allowed the top, which is unreasonable to assume, this cannot be correct, need to at least drop these two values

node.40 <- filter(log, nodeid==40)
#we discover, mote 40 actually has other recordings, let's see how reasonable they are 

ggplot(node.40)+geom_point(aes(x=epoch, y=hamatop)) #ok, looks like at about epoch =50 it went insane
#not naturally occuring since light should be continous, so something went wrong half way through

#confirm with hamatop measurements
ggplot(node.40)+geom_point(aes(x=epoch, y=hamabot))
#hamabot measurements are pretty useless here, pretty much 0 and then sometimes measures stuff

# since mote 40's other sensors seem to be working fine, I will replace it's hamatop measurements (all of htem just to be safe with NA)
#same with it's hamabot measurements

log[log["nodeid"] == 40, ]["hamatop"] <- NA

log[log["nodeid"] == 40, ]["hamabot"] <- NA

filter(log, nodeid==40)

#check there are no other problems with hamatop distribution:

ggplot(log)+geom_point(aes(x=epoch, y=hamatop)) #looks great!
summary(log["hamatop"]) #contains all my newly created NAs again, lots of 0s,but of course because of night!

######################################################################################

#hamabot (bottom: ambient PAR)

summary(log["hamabot"]) #all within normal range
#let's look at the distribution, looks continuous enough, nothing suspicious anymore.  
ggplot(log)+geom_point(aes(x=epoch, y=hamabot))
#moving on!
######################################################################################
#voltage

summary(log["voltage"])
#also within reasonable range as specificed by paper.  
ggplot(log)+geom_point(aes(x=epoch, y=voltage))#totally reaosnable looking, it ebs and flows
#however, for some, there are a CONSTANT reading close to 0...let's look at which these are:

low.voltage <- filter(log, voltage <1)
#this is caused by a bunch of nodes, let's group by them and see which nodeids are responsible

summary(low.voltage)
unique(low.voltage["nodeid"])
#ok, so we have a couple of culprits: 128, 134, 135, 141, 142, 143, 145

#not only is it low, it is constant, which may mean just the voltage readings are off
node.128 <- filter(log, nodeid==128)
summary(node.128)
nrow(node.128) #838 rows!
#one of the parents is 655535, wonder if hte problems with that guy bled through the network to 128...

node.134 <- filter(log, nodeid==134)
summary(node.134)
nrow(node.134) #8282 rows! everthing else is normal (also plotted), also 655535 parent...

node.135 <- filter(log, nodeid==135)
summary(node.135)
nrow(node.135) #3881
#also 655535 parent...this guy seems to be draining everyone. can check is people not connected has no problem


node.141 <- filter(log, nodeid==141)
summary(node.141)
nrow(node.141) #8282

node.142 <- filter(log, nodeid==142)
summary(node.142)
nrow(node.142) #2410

node.143 <- filter(log, nodeid==143)
summary(node.143)
nrow(node.143) #2261

node.145 <- filter(log, nodeid==145)
summary(node.145)
nrow(node.145) #2601

#all of the above are normal, all of them has 65535.0  as a parent...
#Check if parent of other nodes

node.144 <- filter(log, nodeid==144)
summary(node.144)
nrow(node.144) #2601
#yes but no problem, so who know!  But we keep, and just make the coltage reading NA.... or rather keep note and don't think it is werid.  

ggplot(low.voltage)+geom_point(aes(x=epoch, y = humid_temp)) #pretty normal
ggplot(low.voltage)+geom_point(aes(x=epoch, y = hamatop))#pretty normal (well, some discontinuous, so maybe compare with another node)

ggplot(low.voltage)+geom_point(aes(x=epoch, y = hamabot))
ggplot(low.voltage)+geom_point(aes(x=epoch, y = humidity)) #totally reasonable looking

#we keep 128, voltage sensor seems to be off, but everything else is fine.  

# but let's look at voltage with other things...
#humidity
ggplot(log)+geom_point(aes(x=humidity, y=voltage, color=hamatop)) #not too much corelation, looks pretty normal

#hamatop
#hamabot
ggplot(log)+geom_point(aes(x=hamabot, y=voltage))
                       
#ok, nothing too alarming, definitely no reason to throw away.  
#(aside from the low voltage readings I will now change to NA for all of them.  The main reason
# is not that they are low, but that they are constant, so that signals to me that the readings are just off
#it won't hurt to just set them to NA for now.  


log[log["nodeid"] == 128, ]["voltage"] <- NA
log[log["nodeid"] == 134, ]["voltage"] <- NA
log[log["nodeid"] == 135, ]["voltage"] <- NA
log[log["nodeid"] == 141, ]["voltage"] <- NA
log[log["nodeid"] == 142, ]["voltage"] <- NA
log[log["nodeid"] == 143, ]["voltage"] <- NA
log[log["nodeid"] == 145, ]["voltage"] <- NA


######################################################################################
######################################################################################
######################################################################################
######################################################################################

#net
######################################################################################
#humidity
ggplot(net)+geom_point(aes(x=epoch, y=humidity))
#the data looks pretty continuosu, thought some wildly different readings, dropping below 0, not sure if this is calibration eror, let's check which nodes can explain the odd pattersn


#it sucks to delete them eventually, but looking at 

odd.humid <- filter(net, humidity <0)
group_by(odd.humid, nodeid)%>%summarise(n=n())

#ok, three nodeid's are to blame, let's check them out: 78, 123, and 141
node.78 <- filter(net, nodeid==78)
summary(node.78)

filter(locs, ID ==78)  #this is our highest point, it may have been damaged due to water.  Unfortunate to throw it away, however there are other points in the same over 60 meter range.  Also
#log dataset has it, so we are not at a terrible loss

filter(log, nodeid==78)

ggplot(node.78)+geom_point(aes(x=epoch, y=humidity)) #screwing up after some time
ggplot(node.78)+geom_point(aes(x=epoch, y=humid_temp)) #screwing up after some time
ggplot(node.78)+geom_point(aes(x=epoch, y=hamatop))
ggplot(node.78)+geom_point(aes(x=epoch, y=hamabot))

#so there are lots of other points, so let's just delete this

net <- net[net['nodeid'] != 78, ]

#so we have it drops below 0, which is not supposed to happen, paper gives a range, that is above 16
#other points obey this, so probably this is what happens when voltage low? 

ggplot(node.78)+geom_point(aes(x=humidity, y=voltage)) #voltage remained on reasonable levels

#ok, just delete this outlier then, the measurement does not make much sense


node.123 <-filter(net, nodeid==123)
summary(node.123)
ggplot(node.123)+geom_point(aes(x=epoch, y=humidity))
 #seems to go crazy after epoch 5000, also need to delet, no way humidity is fluctuating that much over each epoch
 #infact, check if other readings are ok...
ggplot(node.123)+geom_point(aes(x=epoch, y=voltage))#shot up after 50000 epoch
ggplot(node.123)+geom_point(aes(x=epoch, y=humid_temp)) #bad after 50000 epoch (like no way it is over 40 degrees so a litttle before 5000 we cut)
ggplot(node.123)+geom_point(aes(x=epoch, y=hamatop)) #normal
ggplot(node.123)+geom_point(aes(x=epoch, y=hamabot)) #normal

#delete this point as well


filter(locs, ID ==123) #lot of other motes there
filter(log, nodeid==123)
 #covered also by log

net <- net[net['nodeid'] != 123, ]
#finally

node.141 <-filter(net, nodeid==141)
summary(node.141)
ggplot(node.141)+geom_point(aes(x=epoch, y=humidity))
#same kinda problems as 123, after some time, it went crazy!

ggplot(node.141)+geom_point(aes(x=epoch, y=voltage)) #constant at 1023 volts...sensor wrong
ggplot(node.141)+geom_point(aes(x=epoch, y=humid_temp)) #bad after 90000 epoch (like no way it is over 40 degrees so a litttle before 5000 we cut)
ggplot(node.141)+geom_point(aes(x=epoch, y=hamatop)) #went a bit hight after 90000, not as bad as temperature
ggplot(node.141)+geom_point(aes(x=epoch, y=hamabot)) #same as above.  

#delete this one as well
filter(locs, ID==141)
net <- net[net['nodeid'] != 141, ]


#alright, just to be safe, let's check the summary for log's humidity data

summary(net["humidity"]) #looks quite normal

ggplot(net)+geom_point(aes(x=epoch, y=humidity)) #got rid of most of the outliers, there are still some
#that are really high, let's see what is to blame for this: 

high.humid <- filter(net, humidity >110)
nrow(high.humid)

#culprits: 145 and 118, 145 had low voltage readings in the other log set...

node.145 <- filter(net, nodeid==145)
summary(node.145)
#voltage is off the charts, humid temp readings don't make much sense...

ggplot(node.145)+geom_point(aes(x=epoch, y=humid_temp))

# I don't lack points, I will just drop this one:  The other log data recorded this much better

net <- net[net['nodeid'] != 145, ]

#Finally, let's look at 118
node.118 <- filter(net, nodeid==118)
summary(node.118)
ggplot(node.118)+geom_point(aes(x=epoch, y=humid_temp)) #reasonable
ggplot(node.145)+geom_point(aes(x=epoch, y=humidity)) #can't justify removing this just yet

ggplot(net)+geom_point(aes(x=epoch, y=humidity))  #most outliers gone...


######################################################################################
#humid_temp

ggplot(net)+geom_point(aes(x=epoch, y=humid_temp))
#alright, there are some werid over 40 outliers, according to the readings, there shoudl be nothing over 50


filter(net, humid_temp >50)
#only node 3 to blame

node.3 <- filter(net, nodeid==3)
summary(node.3)
nrow(node.3)

ggplot(node.3)+geom_point(aes(x=epoch, y=humid_temp)) #node three was making sense till after 3500, so let's just delete it, since 
#it's hard to judge at which point it was not.  It's unfortuantley since there are 3 motes, but looking at locs

filter(locs, ID ==3)
#it was at a height of 50, SW interior
ggplot(locs)+geom_point(aes(x=Height, y=ID, color=Direc))
#there is still one more mote in the same direction at that height, so we are not jeaprodizing much


net <- net[net['nodeid'] != 3, ]

#try again: 
ggplot(net)+geom_point(aes(x=epoch, y=humid_temp)) #we are good!

######################################################################################
#hamatop (top: incident PAR)

ggplot(net)+geom_point(aes(x=epoch, y=hamatop))
#looks great!

######################################################################################
#hamabot (bottom: ambient PAR)

ggplot(net)+geom_point(aes(x=epoch, y=hamabot))
#looks reasonable!
######################################################################################
#voltage

ggplot(net)+geom_point(aes(x=epoch, y=voltage))

#ok, so one guy has constant voltage reading once again!

voltage.be.cra <- filter(net, voltage >1000)

nrow(voltage.be.cra)
unique(voltage.be.cra["nodeid"])
#culprits: 134, 135

node.134 <- filter(net, nodeid ==134)
ggplot(node.134)+geom_point(aes(x=epoch, y=humidity))
ggplot(node.134)+geom_point(aes(x=epoch, y=humid_temp))
ggplot(node.134)+geom_point(aes(x=epoch, y=humidity))
ggplot(node.134)+geom_point(aes(x=epoch, y=humidity))

#we checked before, everything is normal, this voltage is just giving a constant reading, so it's probably just not reading correctly.  

#we will do as before and set it as NA

net[net["nodeid"] == 134, ]["voltage"] <- NA
net[net["nodeid"] == 135, ]["voltage"] <- NA



ggplot(net)+geom_point(aes(x=humidity, y=voltage, color=hamatop)) #not too much corelation, looks pretty normal

#hamatop
#hamabot
ggplot(net)+geom_point(aes(x=hamabot, y=voltage))

#normal looking and interesting!
#done!
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
#a priori, their outlier rejection is wrong.  What if the outliers happened because
#voltage is correlated with humidity (for instance, higher humidity caused the voltage to drop below 2.4)
#In this case, we are throwing away good data, and a significant part that would otherwise leave us with biased 
#lower hmidity

#let's now look at readings from voltages lower than 2.4


library("ggplot2")
their.outlier.reject <- filter(log, voltage <2.4)

their.reject <- summary(select(their.outlier.reject, humidity, humid_temp, hamatop, hamabot))
save(their.reject, file="theirrejectstats")
ggplot(their.outlier.reject)+geom_point(aes(x = epoch, y = humidity))
ggplot(their.outlier.reject)+geom_point(aes(x = epoch, y = humid_temp))
ggplot(their.outlier.reject)+geom_point(aes(x = epoch, y = hamatop))
ggplot(their.outlier.reject)+geom_point(aes(x = epoch, y = hamabot))

ggplot(log)+geom_point(aes(x=voltage, y = humidity))+geom_vline(xintercept = 2.4, colour = "red")
ggplot(log)+geom_point(aes(x=voltage, y = humid_temp))+geom_vline(xintercept = 2.4, colour = "red")
ggplot(log)+geom_point(aes(x=voltage, y = hamatop))+geom_vline(xintercept = 2.4, colour = "red")
ggplot(log)+geom_point(aes(x=voltage, y = hamabot))+geom_vline(xintercept = 2.4, colour = "red")
#definitely does not confirm their criteron that once voltage falls below 2.4, there are outliers
#once we got rid of the obvious outliers in the method above, things below and above their criterion all makes sense.  
#at least there is no cause for alarm just yet, 

ggplot(log)+geom_point(aes(x=nodeid, y = voltage))
#this graph shows that each nodeid experienced voltages within the reasonable range
ggplot(log)+geom_point(aes(x=epoch, y = voltage))
#this shows that the voltage going below 2.4 and above 3 all happened very early on, and that if 
#we delete this data, that would bias our data to elminate information from these time periods
 


################################################################################################

################################################################################################

################################################################################################

#Getting rid of MULTIPLES

#first, how many in net and log are repeated measurements? 

################################################################################################


#NET

net.multiplicity <- group_by(net, nodeid, epoch) %>% summarise(n=n()) %>%
  ungroup() %>% group_by(n) %>% summarise(total=n())

#hm, there are definitely repeat values, let's sort this out in net first, then in log
#make a new column that counts the multiplicity of nodeid, epoch

net.m <- group_by(net, nodeid, epoch) %>% mutate(count=n())
net.m <- tbl_df(net.m)
net.doubles <- filter(net.m, count==2)
net.triples <- filter(net.m, count==3)
net.quadruples <-filter(net.m, count==4)


#let's confirm the recordings done when there are repeated loggings are actually entering the same 
#variables...
####################################################
#DOUBLES, temperature
net.doubles <- group_by(net.doubles, nodeid, epoch) %>% mutate(temp.normalize.diff = (max(humid_temp)-min(humid_temp))/max(humid_temp))

filter(select(net.doubles, nodeid, epoch, temp.normalize.diff), temp.normalize.diff !=0)
#good to know, the temperature differences are 0 expect nodeid 74 , just to be safe, we also create temp.diff column
net.doubles <- group_by(net.doubles, nodeid, epoch) %>% mutate(temp.diff = (max(humid_temp)-min(humid_temp)))
filter(select(net.doubles, nodeid, epoch, temp.diff), temp.diff !=0)
#ok, node 74 is to blame at epoch 9441.  and 7 degrees is quite large difference

node.74.epoch.9441 <-filter(net.doubles, nodeid ==74, epoch == 9441)
node.74.epoch.9441

#ok, I'm going to check where node 74 is in locs and replace temperature value with somethng close by

filter(locs, ID == 74)
#1 74     54    NE    3 interior
filter(locs, Height >= 53 & Height <56) #unfortunately nothing was facing the same direction, so I am uncomfortable borrowing values from these
#let's check in log

filter(log, nodeid==74, epoch ==9441)
#ok, this datapoint is not in log, so I don't want to throw the other datapoints aways, but I will just throw away both temperature values...
filter(net, nodeid==74, epoch <9500 &epoch > 9400)
#check for remaining 3 variables, then do the same for triples and quadruples, then you will have no repeats
#ok, that was a good move, temperature was rather continuous, untill we hit the epoch = 9441 point, where we get the 19 degrees, so it's safe to throw that away

#manually change it to the other value
filter(net.doubles, result_time == "2004-05-30 18:52:27.818757")
net.doubles[net.doubles["result_time"] == "2004-05-30 18:52:27.818757", ]["humid_temp"] <- 27.1522
#otherwise, all these doubles temperature we can just condense
filter(net.doubles, nodeid==74 & humid_temp >27) #great it worked!

####################################################
#DOUBLES, humidity

net.doubles <- group_by(net.doubles, nodeid, epoch) %>% mutate(humid.normalize.diff = (max(humidity)-min(humidity))/max(humidity))

filter(select(net.doubles, nodeid, epoch, humid.normalize.diff), humid.normalize.diff !=0)
 #great!  nothing wrong here!

####################################################
#DOUBLES, hamatop

net.doubles <- group_by(net.doubles, nodeid, epoch) %>% mutate(hamatop.normalize.diff = (max(hamatop)-min(hamatop))/max(hamatop))
filter(select(net.doubles, nodeid, epoch, hamatop.normalize.diff), hamatop.normalize.diff !=0)

#oh great, nodeid 74 epoch 9441 has caused more problems!

select(filter(net.doubles, nodeid ==74 & epoch ==9441), epoch, hamatop, hamabot)

#oh great, we are getting all the outliers!

#ok, let's check for continuity again, as the clue to delete the one...though just by range one can see


select(filter(net, nodeid==74, epoch >9430 & epoch < 9500), epoch, hamatop, hamabot)

select(filter(net, nodeid==74, epoch >9430 & epoch < 9500), hamatop, hamabot)
ggplot(filter(net, nodeid==74))+geom_point(aes(x=epoch, y =hamatop))

#perhaps the log data can help us explain...
ggplot(filter(log, nodeid==74))+geom_point(aes(x=epoch, y=hamabot))

#I want to keep both values, but since hamatop screwed up in one measurement, I will just drop the one with the werid hamatop measurement since the fluctuations seem to happen afterwards (though it is hard to judge for this one as my choice was different later on)
net.doubles[net.doubles["result_time"] == "2004-05-30 18:52:27.818757", ]["hamabot"]<-571.429

net.doubles[net.doubles["result_time"] == "2004-05-30 18:52:27.818757", ]["hamatop"]<- 102857.00

#now there should be no doubles here, double check: 
net.doubles <- group_by(net.doubles, nodeid, epoch) %>% mutate(hamatop.normalize.diff = (max(hamatop)-min(hamatop))/max(hamatop))
filter(select(net.doubles, nodeid, epoch, hamatop.normalize.diff), hamatop.normalize.diff !=0)
select(filter(net.doubles, nodeid ==74 & epoch ==9441), epoch, hamatop, hamabot)

#otherwise, all these doubles temperature we can just condense

filter(log, nodeid ==74, epoch >9400) #nothing to validate with net...
#well, showing the same behaviour

#ok, the 8000 measurement just seems to be off, and is discontinuous (for hamatop)
#for hamabot, 

ggplot(filter(net, nodeid==74))+geom_point(aes(x=epoch, y =hamabot))
 #it looks like this guy is covered several times but gets some intense sunglight, not sure what is going on

####################################################
#DOUBLES, hamabot

net.doubles <- group_by(net.doubles, nodeid, epoch) %>% mutate(hamabot.normalize.diff = (max(hamabot)-min(hamabot))/max(hamabot))
filter(select(net.doubles, nodeid, epoch, hamabot.normalize.diff), hamabot.normalize.diff !=0)

#great, no problems here



####################################################
#DOUBLES, voltage


net.doubles <- group_by(net.doubles, nodeid, epoch) %>% mutate(voltage.normalize.diff = (max(voltage)-min(voltage))/max(voltage))
filter(select(net.doubles, nodeid, epoch, voltage.normalize.diff), voltage.normalize.diff !=0)

#no problems!

####################################################
####################################################

#do it for TRIPLES
####################################################
#temperature (TRIPLES)
net.triples <- group_by(net.triples, nodeid, epoch) %>% mutate(temp.normalize.diff = (max(humid_temp)-min(humid_temp))/max(humid_temp))

filter(select(net.triples, nodeid, epoch, temp.normalize.diff), temp.normalize.diff !=0)

#no problem 
####################################################
#humidity (TRIPLES)
net.triples <- group_by(net.triples, nodeid, epoch) %>% mutate(humid.normalize.diff = (max(humidity)-min(humidity))/max(humidity))

filter(select(net.triples, nodeid, epoch, humid.normalize.diff), humid.normalize.diff !=0)

#no problem 
####################################################
#hamabot (TRIPLES)
net.triples <- group_by(net.triples, nodeid, epoch) %>% mutate(hamabot.normalize.diff = (max(hamabot)-min(hamabot))/max(hamabot))

filter(select(net.triples, nodeid, epoch, hamabot.normalize.diff), hamabot.normalize.diff !=0)

#no problem 
####################################################
#hamatop (TRIPLES)
net.triples <- group_by(net.triples, nodeid, epoch) %>% mutate(hamatop.normalize.diff = (max(hamatop)-min(hamatop))/max(hamatop))

filter(select(net.triples, nodeid, epoch, hamatop.normalize.diff), hamatop.normalize.diff !=0)

#no problem 

####################################################
#voltage

net.triples <- group_by(net.triples, nodeid, epoch) %>% mutate(voltage.normalize.diff = (max(voltage)-min(voltage))/max(voltage))
filter(select(net.triples, nodeid, epoch, voltage.normalize.diff), voltage.normalize.diff !=0)

#great, all the triples are just data logged 3 times, no problem!
####################################################
####################################################
####################################################
####################################################
#QUADRUPLES (NET)

#temperature (QUADRUPLES)
net.quadruples <- group_by(net.quadruples, nodeid, epoch) %>% mutate(temp.normalize.diff = (max(humid_temp)-min(humid_temp))/max(humid_temp))

filter(select(net.quadruples, nodeid, epoch, temp.normalize.diff), temp.normalize.diff !=0)

filter(net, nodeid==138)
ggplot(filter(net, nodeid==138), aes(x=epoch, y = humid_temp))+geom_point()
#HUGE problem 
####################################################
#humidity (quadruples)
net.quadruples <- group_by(net.quadruples, nodeid, epoch) %>% mutate(humid.normalize.diff = (max(humidity)-min(humidity))/max(humidity))

filter(select(net.quadruples, nodeid, epoch, humid.normalize.diff), humid.normalize.diff !=0)

#no problem 
####################################################
#hamabot (quadruples)
net.quadruples <- group_by(net.quadruples, nodeid, epoch) %>% mutate(hamabot.normalize.diff = (max(hamabot)-min(hamabot))/max(hamabot))

filter(select(net.quadruples, nodeid, epoch, hamabot.normalize.diff), hamabot.normalize.diff !=0)

#no problem 
####################################################
#hamatop (quadruples)
net.quadruples <- group_by(net.quadruples, nodeid, epoch) %>% mutate(hamatop.normalize.diff = (max(hamatop)-min(hamatop))/max(hamatop))

filter(select(net.quadruples, nodeid, epoch, hamatop.normalize.diff), hamatop.normalize.diff !=0)

#no problem 

####################################################
#voltage

net.quadruples <- group_by(net.quadruples, nodeid, epoch) %>% mutate(voltage.normalize.diff = (max(voltage)-min(voltage))/max(voltage))
filter(select(net.quadruples, nodeid, epoch, voltage.normalize.diff), voltage.normalize.diff !=0)

#great, all the quadruples are just data logged 4 times, no problem!

########################################################################################################
########################################################################################################
########################################################################################################

#LOG (MULTIIPLES)


log.multiplicity <- group_by(log, nodeid, epoch) %>% summarise(n=n()) %>%
  ungroup() %>% group_by(n) %>% summarise(total=n())
log.multiplicity <- tbl_df(log.multiplicity)

#luckily, there are just some doubles in log

log.m <- group_by(log, nodeid, epoch) %>% mutate(count=n())
log.m <- tbl_df(log.m)
log.doubles <- filter(log.m, count==2)

####################################################

#DOUBLES, humidity 

log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(humidity.normalize.diff = (max(humidity)-min(humidity))/max(humidity))
example1 <- filter(select(log.doubles, nodeid, epoch, humidity.normalize.diff), humidity.normalize.diff !=0)
summary(example1)
#all don't differ by much so I will just average the values.  (the spread of the different is so minimal)
log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(humidity = mean(humidity))

####################################################
#DOUBLES, temperature

log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(humid_temp.normalize.diff = (max(humid_temp)-min(humid_temp))/max(humid_temp))
filter(select(log.doubles, nodeid, epoch, humid_temp.normalize.diff, humid_temp), humid_temp.normalize.diff !=0)

#even though there is a lot different, they all differ not by much, I will just take the average of all of them
summary(log.doubles["humid_temp.normalize.diff"])
#humid_temp.normalize.diff
#Min.   :0.000e+00        
#1st Qu.:0.000e+00        
#Median :0.000e+00        
#Mean   :4.786e-05        
#3rd Qu.:0.000e+00        
#Max.   :4.974e-02  

log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(humid_temp = mean(humid_temp))
select(log.doubles, humid_temp)
#great, we are done here!


####################################################
#DOUBLES, hamatop


log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(hamatop.normalize.diff = (max(hamatop)-min(hamatop))/max(hamatop))
filter(select(log.doubles, nodeid, epoch, hamatop.normalize.diff, hamatop), hamatop.normalize.diff !=0)

#8 problematic points, quite problematic.  

select(filter(log.doubles, nodeid == 105 & epoch == 7074), epoch, nodeid, humid_temp, hamatop, hamabot, humidity)
#  epoch nodeid humid_temp  hamatop hamabot humidity
#1  7074    105      8.101  508.772       0  101.696
#2  7074    105      8.101 2026.200       0  101.696

#interesting, this is exactly the same problem as nodeid 74...

ggplot(filter(log, nodeid==105))+geom_point(aes(x=epoch, y = hamatop))+xlim(7000,7500)



select(filter(log.doubles, nodeid == 129 & epoch == 7074), epoch, nodeid, humid_temp, hamatop, hamabot, humidity)
ggplot(filter(log, nodeid==129))+geom_point(aes(x=epoch, y = hamatop))+xlim(7000,7500)
#probably some leave covering then wind and not covering...#same problem as node 105
# I'm just going to average them  (they are happening at the same time, so probably just around noon)

select(filter(log.doubles, nodeid == 134 & epoch == 7074), epoch, nodeid, humid_temp, hamatop, hamabot, humidity)
ggplot(filter(log, nodeid==134))+geom_point(aes(x=epoch, y = hamatop))+xlim(7000,7500)
#exactly same problem...


#135 is a different time: 

select(filter(log.doubles, nodeid == 135 & epoch == 3870), epoch, nodeid, humid_temp, hamatop, hamabot, humidity)
node.135.erratic <- ggplot(filter(log, nodeid==134))+geom_point(aes(x=epoch, y = hamatop))+xlim(3600,4000)

#going to use as an example in the write up

ggsave(node.135.erratic, file="node135example.png")


#but by graphing, it shares the same problem ar the previous two.  So I will average.  

log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(hamatop = mean(hamatop))

#now double check we have no more problems:  
log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(hamatop.normalize.diff = (max(hamatop)-min(hamatop))/max(hamatop))
filter(select(log.doubles, nodeid, epoch, hamatop.normalize.diff, hamatop), hamatop.normalize.diff !=0)

#great it worked!  

####################################################
#DOUBLES, hamabot

log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(hamabot.normalize.diff = (max(hamabot)-min(hamabot))/max(hamabot))
filter(select(log.doubles, nodeid, epoch, hamabot.normalize.diff, hamabot), hamabot.normalize.diff !=0)


#32 different values, mostly because of 0s....if it is not night, 0 makes no sense
ggplot(filter(log, nodeid==4))+geom_point(aes(x=epoch, y=hamabot))

#ok, gonna take max as 0 is not underrepresented for them all

log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(hamabot = max(hamabot))

#double check that worked
log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(hamabot.normalize.diff = (max(hamabot)-min(hamabot))/max(hamabot))
filter(select(log.doubles, nodeid, epoch, hamabot.normalize.diff, hamabot), hamabot.normalize.diff !=0)

#great it worked!  



####################################################
#DOUBLES, voltage

log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(voltage.normalize.diff = (max(voltage)-min(voltage))/max(voltage))
filter(select(log.doubles, nodeid, epoch, voltage.normalize.diff), voltage.normalize.diff !=0)

#ok, there are lots different, but...
summary(log.doubles["voltage.normalize.diff"])

#voltage.normalize.diff
#Min.   :0.000000      
#1st Qu.:0.000000      
#Median :0.000000      
#Mean   :0.000064      
#3rd Qu.:0.000000      
#Max.   :0.037557      
#NA's   :12 

#so just take average!

log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(voltage = mean(voltage))
#doublecheck


log.doubles <- group_by(log.doubles, nodeid, epoch) %>% mutate(voltage.normalize.diff = (max(voltage)-min(voltage))/max(voltage))
filter(select(log.doubles, nodeid, epoch, voltage.normalize.diff), voltage.normalize.diff !=0)

#great!!!!


####################################################
#now we need to almalgamate it back to net and log without the multiple entries

#drop all duplicates from your multiples: 
#log

log.no.doubles <- group_by(log.doubles, epoch, nodeid) %>% filter(row_number()==1)
log.no.doubles <- tbl_df(log.no.doubles)



log.no.doubles.multiplicity <- group_by(log.no.doubles, nodeid, epoch) %>% summarise(n=n()) %>%
  ungroup() %>% group_by(n) %>% summarise(total=n())

#that worked, no doubles!


#net (had doubles, triples and quadruples)

net.no.doubles <- group_by(net.doubles, epoch, nodeid) %>% filter(row_number()==1)
net.no.doubles <- tbl_df(net.no.doubles)
nrow(net.no.doubles) == nrow(unique(net.no.doubles))

nrow(net.triples)
net.no.triples <-group_by(net.triples, epoch, nodeid) %>% filter(row_number()==1)

#sanity checks
nrow(net.no.triples)==678/3 #TRUE!

net.no.quadruples <- group_by(net.quadruples, epoch, nodeid) %>% filter(row_number()==1)
nrow(net.quadruples)
net.no.quadruples #worked!

#ok, now I need to amalgamate 

net.multiplicity <- group_by(net, nodeid, epoch) %>% mutate(count=n())
log.multiplicity <- group_by(log, nodeid, epoch) %>% mutate(count=n())

#First we need to drop all other columns (for averaging the diferent humidity...etc values)
net.no.quadruples <- select(net.no.quadruples, result_time:count)
net.no.triples <- select(net.no.triples, result_time:count)
net.no.doubles <- select(net.no.doubles, result_time:count)
log.no.doubles <- select(log.no.doubles, result_time:count)
#so the original net and log files still have all of the duplicates, I will drop them.  then append my newly trimed no.multiples dfs

net <- filter(net.multiplicity, count==1)
log <- filter(log.multiplicity, count==1)

net <- rbind(net.no.quadruples, net) #nrow(net)+nrow(net.no.quadruples)==nrow(test) #that worked!
#before:  nrow(net)
#[1] 80095
net <- rbind(net.no.doubles, net)
net <- rbind(net.no.triples, net)
nrow(net) ==80095+nrow(net.no.doubles)+nrow(net.no.triples)+nrow(net.no.quadruples) #TRUE!!

nrow(log) #275624
nrow(log.no.doubles) #8250

log <- rbind(log, log.no.doubles)
nrow(log)==275624+nrow(log.no.doubles) #TRUE

#our net and log files are now amalgamated with no repeated measurements  
########################################################################################################

########################################################################################################

########################################################################################################
#now we do the inner joins and try to combine both datasets.  


net$row.id <- 1:nrow(net)
log$row.id <- 1:nrow(log)

# How many overall matches are there?
node.epoch.matches <- inner_join(select(net, nodeid, epoch, row.id),
                                 select(log, nodeid, epoch, row.id),
                                 by=c("nodeid", "epoch"))
# Why doesn't count_distinct work?
nrow(unique(node.epoch.matches[c("nodeid", "epoch")])) / nrow(net) #0.7112299
nrow(unique(node.epoch.matches[c("nodeid", "epoch")])) / nrow(log)# 0.2356644

#before I can merge the two dataframes, I should compare if their data matches up, and if they do, calibrate it
#hence first do an inner join, then rbind over the complemenet of the inner join with each dataset

intersect.log.net <- inner_join(net,log, by=c("nodeid", "epoch"))


################################################################################################


#CALIBRATION !

##########################################
#humidity

intersect.log.net <- mutate(intersect.log.net, humid.diff= humidity.x-humidity.y)
select(intersect.log.net, humid.diff, humidity.x, humidity.y)
select(filter(intersect.log.net, humid.diff != 0), humid.diff)
#differences are so marginal (max 0.02) that I will just average to get real humidity


colnames(intersect.log.net)


intersect.log.net <- mutate(intersect.log.net, humidity.x = mean(humidity.x, humidity.y))
#we can drop the humidity.diff and humidity.y now, and rename humidity.x as humidity

intersect.log.net <- select(intersect.log.net, -(humid.diff))

intersect.log.net <- select(intersect.log.net, -(humidity.y))

colnames(intersect.log.net)[colnames(intersect.log.net)=="humidity.x"] <- "humidity"

colnames(intersect.log.net)


##########################################
#hamatop


intersect.log.net <- mutate(intersect.log.net, top.diff= hamatop.x-hamatop.y)
select(intersect.log.net, top.diff, hamatop.x, hamatop.y)
select(filter(intersect.log.net, top.diff != 0), top.diff)
#  epoch nodeid top.diff
#1  7074    105 -758.714
#2  7074    134   56.696
library(ggplot2)
ggplot(filter(net, nodeid==105))+geom_point(aes(x=epoch, y=hamatop))+xlim(6500, 7500)
#yea, so giving crazy spikes, not sure if i should average or drop...

ggplot(filter(net, nodeid==134))+geom_point(aes(x=epoch, y=hamatop))+xlim(6500, 7500)

#same time that a problem occured, same measurements, I will just average then.  
intersect.log.net <- mutate(intersect.log.net, hamatop.x = mean(hamatop.x, hamatop.y))
intersect.log.net <- select(intersect.log.net, -(top.diff))
intersect.log.net <- select(intersect.log.net, -(hamatop.y))
colnames(intersect.log.net)[colnames(intersect.log.net)=="hamatop.x"] <- "hamatop"

##########################################
#hamabot


intersect.log.net <- mutate(intersect.log.net, bot.diff= hamabot.x-hamabot.y)
select(intersect.log.net, bot.diff, hamabot.x, hamabot.y)
filter(intersect.log.net, bot.diff != 0)

#they are all the same so

intersect.log.net <- select(intersect.log.net, -(bot.diff))
intersect.log.net <- select(intersect.log.net, -(hamabot.y))
colnames(intersect.log.net)[colnames(intersect.log.net)=="hamabot.x"] <- "hamabot"

##########################################
#temperature
intersect.log.net <- mutate(intersect.log.net, temp.diff= humid_temp.x-humid_temp.y)
select(intersect.log.net, temp.diff, humid_temp.x, humid_temp.y)
select(filter(intersect.log.net, temp.diff != 0), temp.diff)

#all but 4 are the same, and the 4 differ by very little (max difference is less than 1 percent)

intersect.log.net <- mutate(intersect.log.net, humid_temp.x = mean(humid_temp.x, humid_temp.y))
intersect.log.net <- select(intersect.log.net, -(temp.diff)) 
intersect.log.net <- select(intersect.log.net, -(humid_temp.y))
colnames(intersect.log.net)[colnames(intersect.log.net)=="humid_temp.x"] <- "humid_temp"
##########################################
#voltage

#this is the tricky one, as they are all different, but let's plot this

ggplot(intersect.log.net)+geom_point(aes(x=voltage.y, y= voltage.x))

regression <- summary(lm(voltage.x ~ voltage.y, intersect.log.net))
regression.df <- lm(voltage.x ~ voltage.y, intersect.log.net)

#great, we got our line!

voltage.calibration <- ggplot(intersect.log.net)+geom_point(aes(x=voltage.y, y= voltage.x))+geom_abline(aes(intercept=442.41, slope= -82.34 ),color="blue")
#worked!
save(voltage.calibration, file = "voltcalibration.png")
#so now we can translate, reasonably...I'm going to make a relatively arbitrary choice, givein 
#the error goes both ways, to translate back to the net units of voltage.  

#first, make the intersect.df just take the log voltage (i.e voltage.y)
#later when we mix together the two datasets, we need to translate the net voltage.  


select(intersect.log.net, voltage.x, voltage.y)
#they are so reasonably close to linear that I will do no error correction
intersect.log.net <- mutate(interset.log.net, voltage.x = voltage.y)

#drop/rename other irrelevant colnames

colnames(intersect.log.net)
colnames(intersect.log.net)[colnames(intersect.log.net)=="voltage.x"] <- "voltage"
colnames(intersect.log.net)[colnames(intersect.log.net)=="result_time.x"] <- "result_time"
colnames(intersect.log.net)[colnames(intersect.log.net)=="parent.x"] <- "parent"
colnames(intersect.log.net)[colnames(intersect.log.net)=="depth.x"] <- "depth"
colnames(intersect.log.net)[colnames(intersect.log.net)=="humid_adj.x"] <- "humid_adj"
colnames(intersect.log.net)[colnames(intersect.log.net)=="humid_adj.x"] <- "humid_adj"

colnames(net)
colnames(log)


intersect.cleaned <- select(intersect.log.net, result_time:hamabot)
net.cleaned <- select(net, result_time:hamabot)
log.cleaned <- select(log, result_time:hamabot)

#now they have the same columns we care about and we can begin to join them.  

#I will just drop in net.cleaned, log.cleaned, their intersecting entries, then convert the voltage in net
#finally I will just r bind them all.  


nrow(net.cleaned) #94061
nrow(log.cleaned) #283874
nrow(intersect.cleaned) #66899

log.node.epoch.matches <- left_join(select(log, nodeid, epoch, row.id),
                                    select(net, nodeid, epoch, row.id),
                                    by=c("nodeid", "epoch"))
test <- left_join(log.cleaned, intersect.cleaned, by=c("nodeid", "epoch") )
test <-tbl_df(test)
summary(test)
test
summary(intersect.cleaned)
sum(!is.numeric(intersect.cleaned$hamabot))

#so if in the merged column, the hamabot.y has NA values, it's because it came from the complement of the intersection in log

#check

log.complement <- filter(test, is.na(hamabot.y))
nrow(log.complement)
nrow(log.cleaned)-nrow(intersect.cleaned)==nrow(log.complement)
#great, so we know to keep those.  

summary(log.complement)

colnames(log.complement)

#rename to the reasonable stuff, delete the .y columns
log.complement <- select(log.complement, result_time.x:hamabot.x)
colnames(log.complement)
log.complement <- rename(log.complement, c("result_time.x"="result_time", "parent.x"="parent", "voltage.x"= "voltage", "depth.x"="depth", "humidity.x"= "humidity", "humid_temp.x"= "humid_temp", "humid_adj.x"="humid_adj", "hamatop.x"="hamatop", "hamabot.x"="hamabot"))
colnames(log.complement)

#now do the same for the net set

log.node.epoch.matches <- left_join(select(log, nodeid, epoch, row.id),
                                    select(net, nodeid, epoch, row.id),
                                    by=c("nodeid", "epoch"))
net.test <- left_join(net.cleaned, intersect.cleaned, by=c("nodeid", "epoch") )
net.test <-tbl_df(net.test)
summary(net.test)
nrow(net.test)
nrow(intersect.cleaned)
summary(intersect.cleaned)
sum(!is.numeric(intersect.cleaned$hamabot))
sum(is.na(net.test$hamabot.y))==nrow(net.test)-nrow(intersect.cleaned)
#ok, confirmed, all of the NAs for hamabot.y are coming from the complement of the interesect set

net.complement <- filter(net.test, is.na(hamabot.y))
net.complement <- select(net.complement, result_time.x:hamabot.x)
nrow(net.complement)
net.complement <- rename(net.complement, c("result_time.x"="result_time", "parent.x"="parent", "voltage.x"= "voltage", "depth.x"="depth", "humidity.x"= "humidity", "humid_temp.x"= "humid_temp", "humid_adj.x"="humid_adj", "hamatop.x"="hamatop", "hamabot.x"="hamabot"))

colnames(net.complement)
#################################################################
#translate the voltage uniteds of net.complement:
#################################################################

net.complement <-tbl_df(net.complement)
net.complement
summary(net.complement$voltage)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 198.0   215.0   220.0   221.4   225.0   307.0    1467 

#intercept=442.41, slope= -82.34 
#so net.voltage = 442.41 -82.34 log.voltage  
#so log.voltage = (442.41- net.voltage)/82.34

#so we need 

net.complement <- mutate(net.complement, voltage= (442.41- voltage)/82.340)
summary(net.complement$voltage)

#all is fixed! 

#################################################################

#now it's time to finally rbind them all together to my finalized clean dataset!

?rbind()

net.log.complement <- rbind(net.complement, log.complement)
nrow(net.log.complement)
nrow(net.complement)+nrow(log.complement)==nrow(net.log.complement) #yes it worked

nrow(intersect.cleaned)
cleaned.dataset <- rbind(net.log.complement, intersect.cleaned)
nrow(cleaned.dataset)==nrow(intersect.cleaned)+nrow(net.complement)+nrow(log.complement)

cleaned.dataset <- tbl_df(cleaned.dataset)
summary(cleaned.dataset)
cleaned.dataset

unique(cleaned.dataset)
#final sanity checks for whether it worked, do we have anymore multiple problems? 

final.multiplicity <- group_by(cleaned.dataset, epoch, nodeid) %>% summarise(n=n()) %>% ungroup() %>% group_by(n) %>% summarise(total=n())
final.multiplicity

#all unique values!  


#################################################################

nrow(cleaned.dataset)
saveRDS(cleaned.dataset, file="final_cleaned.rds")
saveRDS(log.complement, file="log_complement.rds")
saveRDS(intersect.cleaned, file="intersect_cleaned")
saveRDS(net.complement, file="net_complement")


testing_df <- readRDS('final_cleaned.rds')
testing_df <- tbl_df(testing_df)
testing_df
################################################################################################




#locs
locs
#there are two trees we are looking at, 

ggplot(locs)+geom_point(aes(x=ID, y = Height))
# no correlation, so nodeid was not placed with any relation to height.  

ggplot(locs)+geom_point(aes(x=Height, y = Direc, color = Tree))
#mostly put at SW and WSW, not quite uniformly  but throughtout the entire tree


#alright, this data will be useful if we can merge this with the net and log data.  


#other comments: result time was not correlated with epoch, it must be download time. So no taking it into account  

