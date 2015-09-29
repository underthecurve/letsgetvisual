library('WDI')
library('ggplot2')
library('dplyr')
library('foreign')
library('reshape2')
library('zoo')
library('ggthemes')
library('scales')

## BLS data - monthly seasonally adjusted
# http://data.bls.gov/timeseries/LNS14000000
month <- read.csv("bls_month.csv")
month <- melt(month, id.vars = 'Year')
month <- month[month$Year >= 2008, ]
month$date <- paste(month$variable, month$Year, sep = "-")
month$date <- as.Date(as.yearmon(month$date, "%b-%Y"))
month <- rename(month, unempl.monthly = value)

# same as POTUS's https://twitter.com/POTUS/status/643883096689913856
ggplot(month, aes(x = date, y = unempl.monthly)) +
  geom_line() 

## WDI data 
# http://data.worldbank.org/indicator/SL.UEM.TOTL.NE.ZS

unempl <- WDI(country = c('US'), indicator = c('SL.UEM.TOTL.NE.ZS'), start = 2008, end = 2014)

unempl <- rename(unempl, unempl.yearly = SL.UEM.TOTL.NE.ZS)
unempl$unempl.yearly[unempl$year == 2014] <- 6.2 
unempl$unempl.yearly[unempl$year == 2013] <- 7.4
# per OECD: http://stats.oecd.org//Index.aspx?QueryId=68483

unempl$Year<-as.Date(paste0(unempl$year,'-01-01'))

ggplot(unempl, aes(x = Year, y = unempl.yearly)) +
  geom_line() 

## plotting both
ggplot() +
  geom_line(data = month, aes(x = date, y = unempl.monthly/100), color = '#ca0020', alpha = .75, size = 1) +
  geom_point(data = month, aes(x = date, y = unempl.monthly/100, color = 'monthly rate'), size = 3) +
  geom_line(data = unempl, aes(x = Year, y = unempl.yearly/100), color = '#0571b0', alpha = .75, size = 1) +
  geom_point(data = unempl, aes(x = Year, y = unempl.yearly/100, color = 'yearly rate'), size = 3) +
  scale_color_manual(values = c('#ca0020', '#0571b0')) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.ticks = element_blank(), panel.grid.major = element_line(color = 'grey'), legend.title = element_blank(), axis.text = element_text(size = 20), plot.title = element_text(size = 22), legend.text = element_text(size = 20), legend.position = c(0.85,0.85)) +
  ggtitle("Trends in U.S. unemployment:\nmonthly v. yearly estimates\n") 

ggsave("plot_unemp.png", width = 12, height = 8)

  