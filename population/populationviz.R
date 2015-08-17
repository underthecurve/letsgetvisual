library("foreign")
library("ggplot2")
library("RColorBrewer")
library("WDI")
library("countrycode")
library("grid")
library("ggthemes")
getwd()
setwd("/Users/christinezhang/Desktop/visual/population")

## Descriptive - is the 2015 revision of 2050 projections higher or lower than previous (2012) projection? How does this differ by country / income level?
 
# get country income classifications from world development indicators
wdi <- WDI(country="all", indicator = "SP.POP.TOTL", extra = T, start = 2014, end = 2014)

wdi$uncode <- countrycode(wdi$iso3c, "iso3c", "un", warn = T)
names(wdi)
wdi <- wdi[complete.cases(wdi[ ,12]), ]
wdi <- wdi[, c(10, 12)]

# load in UN population projections data (pop2012: projections from 2012 report; pop2015: projections 2015 report)

un <- read.dta("un_data.dta")
head(un)
un <- un[un$year <= 2050, ]
un <- merge(un, wdi, by.x = "Countrycode", by.y = "uncode", all.x = T)

un.2050 <- un[un$year == 2050, ]

un.2050$diff <- un.2050$pop2015 - un.2050$pop2012

sum(un.2050[un.2050$Majorarearegioncountryora=="WORLD",]$diff) # World has slightly higher 2015 estimate of 2050 population, but not super different

sum(un.2050[un.2050$Majorarearegioncountryora!="WORLD",]$diff < 0) # 116 countries/areas for which 2015 revisions give lower 2050 estimates

sum(un.2050[un.2050$Majorarearegioncountryora!="WORLD",]$diff > 0) # 117 countries/areasfor which 2015 revisions give higher 2050 estimates


# 43 high-income countries have downward revisions (only 24 have upward revisions); 78 developing (low & middle income) countries have upward revisions (only 64 have upward revisions)
tapply(un.2050$diff < 0,un.2050$income, sum) 
tapply(un.2050$diff > 0,un.2050$income, sum) 

## Which countries have the most discrepancy between 2012 & 2015 forecasts over the 2015-2050 period?

# regress pop2015 on pop2012 for each country & extract the R-squared stats - the ones with the lowest R2 are the ones for which the projections from the 2012 report diverge the most from the projections from the 2015 report, on average

countries <- unique(un$Majorarearegioncountryora)
R2vector <- vector(mode="numeric", length=length(countries))

for (i in 1:length(countries)) {
  mod <- lm(pop2015 ~ pop2012, data = un[un$Majorarearegioncountryora == countries[i], ])
  R2vector[i] <- summary(mod)$r.squared
}

R2vector <- as.data.frame(R2vector)
R2.data <- cbind(countries, R2vector)

# take a look: which ones are they?
R2.data 
# okay, but some of the ones with the smallest R2 are also pretty small countries (e.g., Turks and Caicos Islands = 0.095)

R2.data.merge <- merge(R2.data, un, by.x = "countries", by.y = "Majorarearegioncountryora")
head(R2.data.merge)
str(R2.data.merge)
R2.data.merge$countries <- as.character(R2.data.merge$countries)

R2.data.order <- R2.data.merge[order(R2.data.merge$R2vector), ]
R2.data.order$countries <- ifelse(R2.data.order$countries == "R\xe9union", "Reunion",
                                  ifelse(R2.data.order$countries == "C\xf4te d'Ivoire",
                                         "Cote D'Ivoire", R2.data.order$countries))

R2.data.order$countries.f <- factor(R2.data.order$countries, levels = unique(R2.data.order$countries))
levels(R2.data.order$countries.f)

# This is a nice plot, but freeing the y scales is confusing
ggplot(R2.data.order[R2.data.order$countries %in% head(unique(R2.data.order[R2.data.order$pop2015 > 5000,]$countries), 20),], aes(x = year, group = countries)) +
  geom_line(aes(y = pop2012), color = "red") +
  geom_line(aes(y = pop2015), color = "blue") +
  facet_wrap(~countries.f, scales = "free_y")

# Index the population estimates such that 2015 = 1.
# this gives the proportional increase compared with 2015 & makes things more comparable
dat.2015 <- R2.data.order[R2.data.order$year == 2015, ]
dat.merge <- merge(R2.data.order, dat.2015, by = "countries")
names(dat.merge)

dat.merge$index2015 <- dat.merge$pop2015.x / dat.merge$pop2015.y
dat.merge$index2012 <- dat.merge$pop2012.x / dat.merge$pop2012.y

dat.merge <- dat.merge[order(dat.merge$R2vector.x), ]

ggplot(dat.merge[dat.merge$countries == "China" | dat.merge$countries == "India" , ], aes(x = year.x, group = countries)) +
  geom_line(aes(y = index2012*100), color = "#4bb2e0", size = 1) +
  geom_line(aes(y = index2015*100), color = "#0018a8", size = 1) +
  facet_wrap(~countries.f.x) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme_fivethirtyeight() +
  geom_hline(yintercept = 90, color = 'gray20') +
  ggtitle("China & India's forecasts have changed slightly in the last 3 yrs\n") +scale_x_continuous(labels = c("2020", "'30", "'40", "'50"), breaks = c(2020, 2030, 2040, 2050)) +
  theme(panel.margin = unit(2, "lines"), strip.background = element_blank(), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"), panel.grid.major = element_line(color = 'gray90', size = .3), strip.text.x = element_text( size = 14), axis.text = element_text(color = 'gray47'))

ggsave("china_ind.pdf", width=8)

ggplot(dat.merge[dat.merge$countries %in% head(unique(dat.merge[dat.merge$pop2015.x > 5000,]$countries), 10) & dat.merge$countries != "China" & dat.merge$countries != "Republic of Korea" | dat.merge$countries == "Viet Nam" , ], aes(x = year.x, group = countries)) +
  geom_line(aes(y = index2012*100), color = "#4bb2e0", size = 1) +
  geom_line(aes(y = index2015*100), color = "#0018a8", size = 1) +
  facet_wrap(~countries.f.x) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme_fivethirtyeight() +
  geom_hline(yintercept = 90, color = 'gray20') +
  ggtitle("Population forecasts are revised every 3 years:\nWho's changed the most?\n") +
  scale_x_continuous(labels = c("2020", "'30", "'40", "'50"), breaks = c(2020, 2030, 2040, 2050)) +
  theme(panel.margin = unit(2, "lines"), strip.background = element_blank(), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"), panel.grid.major = element_line(color = 'gray90', size = .3), strip.text.x = element_text( size = 14), axis.text = element_text(color = 'gray47'))

ggsave("pop.pdf", width=8)
