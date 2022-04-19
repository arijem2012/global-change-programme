head(mtcars)
tail(mtcars)
plot(density(mtcars$hp))
hist(mtcars$hp)
library(nycflights13)
library(tidyverse)
nycflights13::flights
view(flights)
library(dplyr)
((flights$carrier))
filter(flights, month==2, day==23)
filter(flights, carrier==B6)
select(flights, carrier)
distinct(flights, carrier)
view(filter(flights, carrier=="UA"))
view(filter(flights,carrier=="UA", origin=="EWR", dest=="IAH"))
jan25 <- filter(flights, origin=="LGA", dest=="ATL")
view(jan25)
distinct(jan25,carrier)
#frequency(distinct(jan25,carrier))
sub1 <- select(flights, -(tailnum:air_time))
view(sub1)
r1 <- rename(flights, airtime=air_time)
view(head(r1))
#5.2 exercise
delaymorethan2hours <- filter(flights, arr_delay>=120)
view(delaymorethan2hours)
view(filter(flights, dest=="HOU"| dest=="IAH"))
view(filter(flights, carrier %in% c("UA", "AA", "DL")))
view(filter(flights, month %in% c(7,8,9)))
view(filter(flights, month %in% 7:9)) 
view(filter(flights, arr_delay>=120 | dep_delay<=0))
view(filter(flights, dep_delay>=60| dep_delay-arr_delay>30))
view(filter(flights, dep_time==2400 | dep_time<=600 )) #inclusive given timing
view(filter(flights, between(dep_time, 000, 600))) #exclusive given timing
view(filter(flights, is.na(dep_time)))
summary(flights)
0/0
#5.3 exercise
view(arrange(flights, desc(air_time)))
without_missing_values <- is.na(flights)
summary(without_missing_values)
summary(flights)
view(without_missing_values)
view(arrange(flights, desc(dep_time)))
view(arrange(flights, dep_time))
view(arrange(flights, desc(is.na(dep_time)), dep_time))
view(arrange(flights,desc(dep_delay)))
view(arrange(flights,dep_delay))
view(arrange(flights, desc()))
speed <- (flights$distance/flights$air_time)
view(speed)
new_flights <- cbind(flights,speed)
view(new_flights)
view(arrange(new_flights,desc(speed)))
view(arrange(new_flights, desc(distance)))
view(arrange(new_flights,distance))
#5.4 exercise
view(select(new_flights, dep_time, dep_delay, arr_time, arr_delay))
view(select(new_flights, dep_time:arr_delay))
new1 <- select(new_flights, -(year:day))
view(new1)
new2 <- select(new1, -(carrier:speed))
view(new2)
new3 <- select(new_flights, -((year:day)|(carrier:speed)))
view(new3)
view(select(new_flights,any_of(c("dep_time","dep_delay","arr_time","arr_delay"))))
view(select(new_flights,all_of(c("dep_time","dep_delay","arr_time","arr_delay"))))
view(select(new_flights, starts_with("dep_"), starts_with("arr_")))
viewselect(new_flights, matches("dep"))
vignette("dplyr")
v1 <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
view(new_flights, !!!v1) # Bang bang bang operator
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
view(select(new_flights, any_of(vars)))
view(select(new_flights, !!!vars))
#5.5 exercise
view(1:3+1:10)
?Trig
#5.6 exercise
by_day <- group_by(new_flights, year, month, day)
view(summarise(by_day, mean_distance=mean(distance, na.rm=TRUE)))
distan1 <- summarise(by_day, mean_distance=mean(distance, na.rm=TRUE))
view(select(distan1,desc(mean_distance)))
by_dest <- group_by(new_flights, dest)
delay <- summarise(by_dest, count=n(), dist=mean(distance, na.rm=TRUE), delay=mean(arr_delay, na.rm=TRUE))
delay <- filter(delay, count>30, dest!='HNL')
ggplot(delay, mapping=aes(x=dist, y=delay)) + geom_point(aes(size=count), alpha=1/3) + geom_smooth(se=FALSE)
delay1 <- new_flights%>%
  group_by(dest)%>%
  summarise(count=n(), dist=mean(distance,na.rm=TRUE), delay1=mean(arr_delay, na.rm=TRUE))%>%
  filter(count>20, dest!="HNL")

view(pro1 <- (new_flights%>%
  group_by(dest)%>%
  summarise(mean_dist=mean(distance, na.rm=TRUE)))%>%
  arrange(desc(mean_dist)))

not_cancelled <- new_flights%>%
  filter(!is.na(dep_delay) | !is.na(arr_delay))
view(not_cancelled)

view(not_cancelled%>%
  group_by(year, month, day)%>%
  summarise(mean_delay=mean(dep_delay, na.rm=TRUE))%>%
    arrange(desc(mean_delay)))

max(new_flights$distance)
sd(new_flights$distance)
n_distinct(new_flights$dest)

view(not_cancelled%>%
  group_by(year, month, day)%>%
  summarise(first_flight=min(dep_time),
            last_flight=max(dep_time)))

sum(is.na(new_flights$distance))
sum(!is.na(new_flights$distance))
view(not_cancelled%>%
  group_by(dest)%>%
  summarise(dcarrier=n_distinct(carrier))%>%
  arrange(desc(dcarrier)))
view(march25 <- filter(not_cancelled, month==3, day==25))
view(not_cancelled%>%
  group_by(month==3, day==25)%>%
  summarise(early_dep=sum(dep_time<500))%>%
  arrange(desc(early_dep)))
view(filter(not_cancelled, month==3, day==25, dep_time<500))

view(not_cancelled%>%
  group_by(year, month, day)%>%
  summarise(delay_morethan_onehour=mean(arr_delay>60)))
view(not_cancelled)

daily <- group_by(new_flights, year, month, day)
view(per_day <- summarise(daily, flights=n()))
per_month <- summarise(per_day, flights=sum(flights))
view(per_year <- summarise(per_month, flights=sum(flights)))

not_cancelled%>%
  group_by(dest)%>%
  summarise(n=n())

not_cancelled%>%
  group_by(dest)%>%
  summarise(n=length(dest))

not_cancelled%>%
  group_by(tailnum)%>%
  tally()
install.packages("tinytex")
tinytex::install_tinytex()

Arijit <- c(1,2,3,4,5)
Sujit <- c(6,7,8,9,10)
Arijit
college <- data.frame(Arijit, Sujit)
college
hist(Arijit)
barplot(Sujit)
?barplot

#Exercise 7 practice
library(tidyverse)
library(ggplot2)
library(dplyr)
view(diamonds)
view(is.na(diamonds))
summary(diamonds)
mean_price_of_diamonds <- diamonds%>%group_by(cut)%>%summarise(mean_price=mean(price))
ggplot(diamonds,aes(x=cut) ) + geom_bar()
ggplot(diamonds, colour="clarity") + geom_bar(mapping=aes(x=clarity))
ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
diamonds%>%count(cut)
small_set <- diamonds%>%filter(carat<3)
ggplot(small_set, aes(x=carat, colour=cut))+geom_freqpoly(binwidth=0.1)
count(filter(diamonds, carat==1|carat==2))
view(diamonds%>%group_by(cut)%>%summarise(mean_depth=mean(depth)))
count(filter(diamonds,carat==0.99))
count(filter(diamonds, carat==1))
count(filter(diamonds, color=="E"))
1+2
2+5
5/2
library(usethis)
7+5
