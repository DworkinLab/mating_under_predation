# Mating Under Predation R code and issues

dpylr recommended by Ian
```
install.packages("dplyr")
library(dplyr)
```

```
copulation <- read.csv("Mature.csv",h=T)
courtship <- read.csv("Immature.csv",h=T)
```
Bring in data from working directory (getwd())

```
barplot(copulation$Cop_latency, names.arg = copulation$Fly_ID)
barplot(copulation$Cop_Duration, names.arg = copulation$Fly_ID)
```
Quick view of both in copulation


```
summary(courtship)
```
need each fly with specific ID
```
courtship$Fly_ID <- with(courtship, paste0(courtship$Box,courtship$Date,courtship$Replicate, courtship$Vial_number))
courtship$Fly_ID
```
Need to keep times above 900 seconds for observation (15 minutes)

Make start time in seconds: currently in HH:MM:SS

Easiest method = trial end minus court duration
```
courtship$startTimeSeconds <- (courtship$trial_latency_behav_end - courtship$court_duration)
courtship$startTimeSeconds
```
One value is negative, not sure why? changed to time 0 (delay in program starting vs. behaviour input)
```
courtship$startTimeSeconds[1339]
courtship$startTimeSeconds[1339] = 0
```

```
by_FlyID <- group_by(courtship$Fly_ID)
class(courtship$Fly_ID)
```
Error: says applied to character of class "character", which it is....
Needed library("dplyr") at start

```
by_FlyID <- group_by(courtship,Fly_ID)
by_FlyID
summary(by_FlyID)
```
Need values for less than 15 minutes (900 seconds)
- 1st: nothing that started after 900 seconds
```
lessBy_flyID <- subset(by_FlyID, startTimeSeconds < 900)
lessBy_flyID
summary(lessBy_flyID)
```





