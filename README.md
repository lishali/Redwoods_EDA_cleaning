Introduction
==============

The Redwoods dataset was collected to get a high resolution idea of the microclimate around a redwood tree over time. The
project was motivated by recent developments in measurement equipment, in particular in wireless motes, that allowed for
a large amount of detailed data to be collected.

The Data
==============

Data from two redwood trees (interior and exterior) was collected over 44 days in early summer, when the most dynamic
microclimatic variation was expected. Recordings were taken by 33 one inch motes every 5 minutes starting from a height
of 15 meters to 70 meters on both tree, captured mostly on the west side, and spaced 0.1-1 meters from the trunk. These
regions was chosen to concentrate on the foliage region, so climate measurements are more buffered against environmental
effects, and thus were expected to be more an indication of the microclimate created by the trees.
Each mote had 4 sensors, one for temperature, humidity, ambient and direct photosynthetically active solar radiation (hamatop
and hamabot respectively). The design of the mote seemed reasonably thought out to measure these quantities properly.
For instance the ambient PAR sensor was sheltered but had wide reach, and mote sensors were calibrated to ensure measurements
between motes agreed within reasonable levels. More details about the dataset can be found in **background_readings**.

Data Collection
==============

The data was collected via wireless download every 5 minutes, comprising the net dataset, and was recorded locally on a 512
kB chip (enough for the duration of the measurements, unless the chip was partially full from testing), comprising the log
dataset. This turned out to be useful redundancy, since during the data cleaning, one can check that the intersection of their
recorded values was 60% of the net data and only 20% (approx.) of log data. This raw data can all be found in the **Data** folder. 

Data Cleaning
==============

The data cleaning is best summarized in two parts. I first systematically checked the hamatop, hamabot, temperature,
humidity and voltage measurements of both net and log datasets for abnormalities, plotting against epoch as a guide. In the
second part, I combined net, log and locs data, and verified the all dataset had no extra information. Details and code are
documented in the Lab1dataclean file (with a outline of where to find each section by code line, and includes analysis of the
Redwood paper’s outlier rejection). 

===============

Details for the datacleaning, EDA and lots of plots can be found in the [Analysis_report](Analysis_report.pdf). 

===============
The code for data analysis is found in the **Data_analysis_code** files.  


