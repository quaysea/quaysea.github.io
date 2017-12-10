---
title: "bst 260"
author: "Kelechi"
date: "November 17, 2017"
output: html_document
---

```{r setup, include=FALSE}

suppressMessages(library ("tidyverse"))
hbdata<-read.csv("C:/Users/Kay/Dropbox/BST-260 project/preg data.csv", header=TRUE)
sedata<-hbdata%>%select(country, year, survey, house, cluster,dhsid, Anemia.level,hb,anemic, During.pregancy..given.or.bought.iron.tablets.syrup, Wealth.index, Total.children.ever.born, Current.age...respondent, Age.5.year.groups, Type.of.place.of.residence, Highest.educational.level, Source.of.drinking.water, Age.at.1st.birth...18..18.25...25, Type.of.toilet.facility, Number.of.living.children,   Hemoglobin.level.adjusted.by.altitude..g.dl...1.decimal., Number.of.living.children, Body.mass.index.for.respondent, Sample.weight)

library(dplyr)
usedata<-sedata%>%
  mutate(myear= case_when(
    between(year, 2000, 2005) ~ "2000-2005",
    between(year, 2006, 2010)  ~ "2006-2010",
    between(year, 2011, 2015) ~ "2011-2015"
  ))


usedata<-sedata%>%
  mutate(myear= case_when(
    between(year, 2000, 2010) ~ "2000-2010",
    between(year, 2011, 2015) ~ "2011-2015"
  ))



  usedata%>%
  filter(hb<200&hb>30)%>%
  group_by(myear)%>%
  ungroup()%>%
  ggplot(aes(country, hb)) +
  geom_boxplot() +
  coord_flip() +
  xlab("")+
  facet_wrap(~myear)
  
  usedata%>%
  filter(hb<200&hb>30)%>%
  group_by(myear)%>%
  ungroup()%>%
  ggplot(aes(hb,country)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("")

  facet_wrap(~myear)


usedata%>%
  filter(hb<200&hb>30)%>%
  group_by(country)%>%
  ungroup()%>%
  ggplot(aes(year, hb)) +
   geom_boxplot() +
   facet_wrap(~country)

usedata%>%
  filter(hb<200&hb>30)%>%
  group_by(myear)%>%
  ungroup()%>%
  ggplot(aes(country, hb)) +
  geom_boxplot() +
  coord_flip() +
  xlab("")+
  facet_wrap(~myear)


library(broom)
library(robustlmm)
reg<-usedata%>%lmer(hb~country+(1|cluster)+(1|year)+Current.age...respondent+Highest.educational.level+Wealth.index, data=., REML=FALSE)


Reg<-lm(Anemia.level~Current.age...respondent,country|cluster,1|year,Highest.educational.level, data=usedata)
summary(reg)
hist(usedata$hb)ti


## R Markdown
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(plotly)

worldMap <- map_data("world")
head( worldMap )





## Including Plots
contname<-read.csv("C:/Users/Kay/Desktop/list-african-countries-dependent-territory-286j.csv")
contdata<-as.data.frame(usedata$country)
colnames(contdata)="region"

newdata<-worldMap%>%filter(region%in%contname$country)%>%mutate(country=region)
eix<-left_join(neweruse, newdata,by="country")
newuse<-left_join(usedata, eix, by="country")
neweruse<-usedata%>%group_by(country)%>%summarise(hbs=mean(hb, na.rm=TRUE))

africa<-ggplot() + geom_polygon(data = newdata, aes(x=long, y = lat, group = group, fill=region), color="black") + 
  coord_fixed(1)

africa+
  guides(fill=FALSE)


ca_base <- ggplot(data = newdata, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1) + 
  geom_polygon(color = "black", fill = "gray")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
  )

elbow_room1 <- ca_base + 
      geom_polygon(data = eix, aes(fill = hbs), color = "white") +
      geom_polygon(color = "red", fill = NA) +
      theme_bw() +
      ditch_the_axes

elbow_room1

```

## children_data


```{r cars}
suppressMessages(library ("tidyverse"))
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(plotly)
library(lme4)
library(broom)
library(robustlmm)
library(stringr)
library(tibble)
library(lattice)
library(latticeExtra)
library(knitr)
library(kablExtra)
childata<-read.csv("C:/Users/kay/Dropbox/BST-260 project/chl data.csv")
#child<-read.sas7bdat("C:/Users/Kay/Dropbox/BST-260 project/chl.sas7bdat")

#grouping survey years in 3 groups and in 2 groups
childuse<-childata%>%
  mutate(myear= case_when(
    between(year, 2000, 2005) ~ "2000-2005",
    between(year, 2006, 2010)  ~ "2006-2010",
    between(year, 2011, 2016) ~ "2011-2016"
  ))

childuse<-childata%>%
  mutate(myear= case_when(
    between(year, 2000, 2010) ~ "2000-2010",
    between(year, 2011, 2016) ~ "2011-2016"
  ))

t<-childata%>%group_by(region, country)%>%summarise(meanhb=mean(hb, na.rm=TRUE))#checking list of countries
data.frame(t)
line<-childuse%>%
  filter(!is.na(hb<200&hb>30))%>%
  group_by(year)%>%
  summarise(hbs=median(hb, na.rm=TRUE))%>%
  #summarise(hbs=sum(hb<100)/length(hb))%>%
  ungroup()%>%
  ggplot(aes(year, hbs)) +
 geom_point()+
  geom_smooth()
  geom_line()


```

```{r}
#Box plots of hb values by country in year groups with logarithmic conversion of of the hb
library(ggthemes)
library(gridExtra)

childuse%>%
  filter(hb<200&hb>30)%>%
  group_by(myear)%>% 
  mutate(country = reorder(country, hb)) %>%
  ungroup()%>%
  ggplot() +
  geom_boxplot(aes(country, hb)) +
  scale_y_log10() +
  coord_flip()+ 
  theme_fivethirtyeight()+
  facet_wrap(~myear)
  
  childuse%>%
  filter(hb<200&hb>30)%>%
  group_by(myear)%>%
  ungroup()%>%
  ggplot() +
  geom_boxplot(aes(country, hb,fill = factor(myear))) +
  scale_y_log10() + 
     coord_flip()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  facet_wrap(~myear)



childuse%>%
  filter(hb<200&hb>30)%>%
  group_by(myear)%>%
  ungroup()%>%
  ggplot(aes(country, hb))+ 
  geom_jitter(width = 0.1, alpha = 0.2) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme
    scale_y_log10() +
  theme_economist_white()+
    facet_wrap(~myear)

```


```{r}
#bar chart showing percentage of children anemic by country
snapshot<-  childuse%>%
  filter(!is.na(hb<200&hb>30))%>%
  group_by(country)%>%
  summarise(hbs=sum(hb<100)/length(hb))%>%
  mutate(country = reorder(country, hbs)) %>%
  ungroup()%>%
  ggplot(aes(country, hbs*100)) +
 geom_bar(stat="identity") +
  coord_flip() +
  xlab("")

bar_percent<-childuse%>%
  filter(!is.na(hb<200&hb>30))%>%
  group_by(myear, country)%>%
  summarise(hbs=sum(hb<100)/length(hb))%>%
  mutate(country = reorder(country, hbs)) %>%
  ungroup()%>%
  ggplot(aes(country, hbs*100)) +
 geom_bar(stat="identity") +
  coord_flip() +
  xlab("")+
  facet_wrap(~myear)
```


```{r}
#Maps showing average hb per country
worldMap <- map_data("world")
contname<-read.csv("C:/Users/Kelec/Desktop/list-african-countries-dependent-territory-286j.csv")#getting country names
Africa<-worldMap%>%filter(region%in%contname$country)%>%mutate(country=region)#subsetting the world map to only african countries using the vector of african countries created above

neweruse<-childuse%>%group_by(country,myear)%>%mutate(hb=as.numeric(hb))%>%summarise(hbs=mean(hb, na.rm=TRUE))
newuse<-right_join(Africa, neweruse, by="country")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
  )
ca_base <- ggplot(data = Africa, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1) + 
  geom_polygon(color = "black", fill = "gray")

elbow_room1 <- ca_base + 
      geom_polygon(data = newuse, aes(fill = hbs), color = "white") +
      geom_polygon(color = "red", fill = NA)+
  theme_classic()+
  facet_wrap(~myear)+
  geom_text(aes(label=country), size = 3)+
  ditch_the_axes

ggplotly(elbow_room1) %>% config(displayModeBar = FALSE) %>% config( showlink = FALSE)


neweruse<-childuse%>%group_by(country)%>%mutate(hb=as.numeric(hb))%>%summarise(hbs=mean(hb, na.rm=TRUE))
newuse<-right_join(Africa, neweruse, by="country")
ca_base <- ggplot(data = Africa, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1) + 
  geom_polygon(color = "black", fill = "gray")

elbow_room1 <- ca_base + 
      geom_polygon(data = newuse, aes(fill = hbs), color = "white") +
      geom_polygon(color = "red", fill = NA)+
  theme_classic()+
  ditch_the_axes

k<-ggplotly(elbow_room1) %>% config(displayModeBar = FALSE) %>% config( showlink = FALSE)

```

```{r}
#Regression
childregress<-childuse%>%
  mutate(year=as.factor(year), 
         myear=as.factor(myear), 
         country=as.factor(country),
         totalwt=sum(as.numeric(svyweight), na.rm=TRUE),
         weight=svyweight/totalwt,
        klust=paste(country, cluster, year, sep=" "),
        klust=as.factor(klust))


reg<-lmer(hb ~ country + (1 | klust)+age + wealthindex + has_bednet + livechl + Number.of.children.5.and.under, 
          data=childregress, 
          weights=weight, 
          REML=FALSE)

parta=tidy(reg)
  
partb<-confint(reg, 
               parm =c("age","wealthindex","has_bednet","livechl","Number.of.children.5.and.under"), 
               level=0.95)
partc<-data.frame(partb, fix.empty.names = TRUE)


part<-data.frame(parta[31:35,1:4], partc[,1:2], row.names = NULL)

kable(part, 
       caption = "Estimates and confidence intervals")%>%kable_styling()
```

