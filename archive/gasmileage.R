library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(scales)

fsave <- TRUE

df.cav <- read.table("data/cavalier.csv",sep=",",header=TRUE,fill=TRUE)
df.cor <- read.table("data/corolla.csv",sep=",",header=TRUE,fill=TRUE)
df.rav <- read.table("data/rav4.csv",sep=",",header=TRUE,fill=TRUE)

df.cav$car <- "cav"
df.cor$car <- "cor"
df.rav$car <- "rav"

byday <- function(df) {
    d <- lapply(seq_len(nrow(df)-1), function(i) {
        day <- seq.Date(from=as.Date(df$Date[i],format="%m/%d/%Y"), to=as.Date(df$Date[i+1],format="%m/%d/%Y"),
        by="day")
        n <- ifelse(as.numeric(max(day)-min(day))>0,as.numeric(max(day)-min(day)),0)
        miles <- df$miles[i+1]/(n+1)
        #print(paste(i,"from:",df$Date[i],"to:",df$Date[i+1],"n:",n))
        d <- data.frame("date"=day,"miles"=miles)
    })
    return(do.call(rbind,d))
}

# Expand by day and then by month to smooth the data
df.cav2 <- byday(df.cav) %>% mutate(month=month(date),year=year(date)) %>% group_by(month,year) %>% summarise(miles=sum(miles)) %>% mutate(car="cav")
df.cor2 <- byday(df.cor) %>% mutate(month=month(date),year=year(date)) %>% group_by(month,year) %>% summarise(miles=sum(miles)) %>% mutate(car="cor")
df.rav2 <- byday(df.rav) %>% mutate(month=month(date),year=year(date)) %>% group_by(month,year) %>% summarise(miles=sum(miles)) %>% mutate(car="rav")
df.cars <- rbind(df.cav2,df.cor2,df.rav2) %>% mutate(date=as.Date(paste(year,month,"1",sep="-")))
dmy <- seq.Date(from=min(df.cars$date),to=max(df.cars$date),by="month")
dmy.cav1 <- data.frame("date"=dmy,"car"="cav","month"=month(dmy),"year"=year(dmy),"miles"=0)
dmy.cor1 <- data.frame("date"=dmy,"car"="cor","month"=month(dmy),"year"=year(dmy),"miles"=0)
dmy.rav1 <- data.frame("date"=dmy,"car"="rav","month"=month(dmy),"year"=year(dmy),"miles"=0)
df.cars3 <- rbind(as.data.frame(df.cars),dmy.cav1,dmy.cor1,dmy.rav1) %>% group_by(month,year,car) %>% summarise(miles=sum(miles)) %>% mutate(date=as.Date(paste(year,month,"1",sep="-")))
df.cars3$car <- factor(df.cars3$car, levels=c("rav","cor","cav"))

df.car <- rbind(df.cav,df.cor,df.rav)

df.car$date <- as.Date(df.car$Date, format="%m/%d/%Y")
df.car$month <- month(df.car$date)
df.car$year <- year(df.car$date)
df.car$mo <- df.car$month
#df.car$mo <- ifelse((df.car$month %% 2)==0, df.car$month,df.car$month+1)
df.car2 <- df.car %>% select(date,car,mo,year,miles)

# Create dummy data to fill gaps with zeros
byDate <- seq.Date(from=as.Date("2005-01-01"),to=as.Date("2020-02-01"),by="month")
dmy.cav <- data.frame("date"=byDate,"car"="cav","mo"=month(byDate),"year"=year(byDate),"miles"=0)
dmy.cor <- data.frame("date"=byDate,"car"="cor","mo"=month(byDate),"year"=year(byDate),"miles"=0)
dmy.rav <- data.frame("date"=byDate,"car"="rav","mo"=month(byDate),"year"=year(byDate),"miles"=0)
df.car3 <- rbind(df.car2,dmy.cav,dmy.cor,dmy.rav)


df.grp <- df.car3 %>% group_by(mo,year,car) %>% summarise(miles=sum(miles))
df.grp$date <- paste(df.grp$year,df.grp$mo,"1",sep="-")
df.grp$date2 <- as.Date(df.grp$date)
df.grp$car <- factor(df.grp$car, levels=c("rav","cor","cav"))

#df.grp$mo <- ifelse((df.grp$month %% 2)==0, df.grp$month,df.grp$month+1)




#
# Theme
#
theme_nh <- theme(panel.background=element_rect(fill="#fff1e5"),
plot.background=element_rect(fill="#fff1e5"),
axis.line.y=element_blank(),
axis.line.x=element_line(color="grey40",size=0.2),
panel.grid.minor = element_blank(),
panel.grid.major.y=element_line(color="grey70",size=0.2),
panel.grid.major.x=element_line(color="grey70",size=0.2),
axis.ticks=element_blank(),
axis.text.y=element_text(size=10,color="grey40",family="Optima"),
axis.text.x=element_text(size=10,color="grey40",family="Optima"),
axis.title.x=element_text(size=9,color="grey40",family="Optima"),
axis.title.y=element_text(size=9,color="grey40",family="Optima"),
plot.title=element_text(size=16,color="grey15",family="Optima",face="bold"),
plot.subtitle=element_text(size=12,color="grey40",family="Optima"),
plot.caption=element_text(size=6,color="grey50",hjust=0.0),
plot.margin=unit(c(1.5,0.4,0.2,0.2),unit="cm"),
legend.position="none")




p1 <- ggplot(df.grp, aes(x=date2,y=miles,group=car,color=car)) +
geom_point() +
geom_line()


p.miles <- ggplot(df.grp, aes(x=date2,y=miles,group=car,fill=car)) +
geom_area(position="stack",color="grey35",lwd=0.2,alpha=0.6) +
scale_y_continuous(label=comma) +
annotate("text",x=as.Date("2008-01-01"),y=3750,label="Cavalier",family="Optima",color="blue",size=5) +
labs(x="",y="Miles per month") +
theme_nh


p.miles2 <- ggplot(df.grp, aes(x=date2,y=miles,group=car,fill=car)) +
#geom_area(position="stack",color="grey25",lwd=0.2) +
stat_smooth(geom="area", method="loess",position="stack",span=0.15,alpha=0.5) +
scale_y_continuous(label=comma) +
annotate("text",x=as.Date("2008-01-01"),y=3750,label="Cavalier",family="Optima",color="blue",size=5) +
labs(x="",y="Miles per month") +
theme_nh


####################
# This is the one
#  Blues
#  60% Lightness #4974e9
#  77% Lightness #96aff2 *R default
#
#  Greens
#  40% Lightness #3b9339
#  62% Lightness #74c973 *R default
#
#  Reds
#  60% Lightness #f34e3f
#  77% Lightness #f8988f *R default
p.miles3 <- ggplot(df.cars3, aes(x=date,y=miles,group=car,fill=car)) +
geom_area(position="stack",color="grey35",lwd=0.2,alpha=0.6) +
scale_y_continuous(limits=c(0,5000),breaks=c(1000,2000,3000,4000,5000),label=comma) +
annotate("text",x=as.Date("2005-10-01"),y=4200,label="Cavalier",family="Optima",color="#4974e9",size=5,alpha=0.8) +
annotate("text",x=as.Date("2010-10-01"),y=3250,label="Corolla",family="Optima",color="#3b9339",size=5,alpha=0.8) +
annotate("text",x=as.Date("2018-10-01"),y=2300,label="Rav4",family="Optima",color="#f34e3f",size=5,alpha=0.7) +
#
annotate("text",x=as.Date("2007-02-01"),y=4000,label="Home for\nthe holidays",family="Optima",color="grey50",size=3,alpha=0.8) +
annotate("text",x=as.Date("2009-05-01"),y=4300,label="Road trip!",family="Optima",color="grey50",size=3,alpha=0.8,hjust=0.5) +
#annotate("text",x=as.Date("2012-05-01"),y=2000,label="Hmm, what\nhappened to\nthat stack of\nreceipts?",family="Optima",color="grey50",size=3,alpha=0.8,hjust=0.5) +
annotate("text",x=as.Date("2012-05-01"),y=1850,label="Lost a giant\nstack of\nreceipts",family="Optima",color="grey50",size=3,alpha=0.8,hjust=0.5) +
annotate("text",x=as.Date("2007-08-15"),y=500,label="Long trips to\nfaraway lands",family="Optima",color="grey40",size=3,alpha=0.8,hjust=0.5) +
annotate("text",x=as.Date("2016-10-01"),y=2400,label="So long\nCavalier...",family="Optima",color="grey50",size=3,alpha=0.8,hjust=0.5) +
annotate("text",x=as.Date("2019-07-01"),y=3400,label="Road trip!",family="Optima",color="grey50",size=3,alpha=0.8,hjust=0.5) +
#
geom_segment(x=as.Date("2007-01-01"),xend=as.Date("2006-07-25"),y=600,yend=200,size=0.2,color="grey65",alpha=0.5) +
geom_segment(x=as.Date("2008-04-01"),xend=as.Date("2008-11-01"),y=600,yend=200,size=0.2,color="grey65",alpha=0.5) +
geom_segment(x=as.Date("2007-01-01"),xend=as.Date("2006-12-25"),y=3600,yend=3200,size=0.2,color="grey80",alpha=0.8) +
geom_segment(x=as.Date("2006-07-01"),xend=as.Date("2005-12-25"),y=3600,yend=3200,size=0.2,color="grey80",alpha=0.8) +
geom_segment(x=as.Date("2007-08-01"),xend=as.Date("2007-12-01"),y=3600,yend=3200,size=0.2,color="grey80",alpha=0.8) +
geom_segment(x=as.Date("2016-10-01"),xend=as.Date("2016-10-01"),y=2050,yend=200,size=0.2,color="grey80",alpha=0.8) +
geom_segment(x=as.Date("2012-03-15"),xend=as.Date("2012-03-15"),y=1200,yend=200,size=0.2,color="grey80",alpha=0.8) +
#
labs(x="",y="Miles per month",title="A trip down memory lane with... gas receipts",subtitle="How I justified obsessively hoarding gas receipts and tracking mileage for the last 15 years",caption="Created by N. Hoteling | April 2020") +
theme_nh


p3 <- ggplot(df.grp, aes(x=date2,y=miles,group=car,color=car,fill=car)) +
geom_bar(position="stack",stat="identity") +#,color="grey99",lwd=0.2,width=30) +
scale_y_continuous(label=comma) +
labs(x="",y="Miles per month") +
theme_nh

