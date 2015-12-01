library(ggplot2)
library(RColorBrewer)
library(foreign)
library(grid)
library(reshape2)
library(DescTools)

setwd("~/Desktop/visual/twitter/data")
list.files()

dat <- read.csv("followers.csv")
dat.m <- melt(dat)
dat.m$account <- gsub("X.", "@", dat.m$variable) # make twitter handles begin with "@"

head(dat.m)
str(dat.m)
levels(dat.m$date) # this is messy. is there a more efficient way to get R to understnad dates?
dat.m$date.order <- factor(dat.m$date, levels =c('7 Oct', '8 Oct', '9 Oct', '10 Oct', '11 Oct', '12 Oct', '13 Oct', '14 Oct','4 Nov', '5 Nov', '6 Nov', '7 Nov', '8 Nov', '9 Nov', '10 Nov', '11 Nov')) 

# week variable labels 'average' week v. MozFest week

# hard to see since nicky has so many followers! 
ggplot(dat.m, aes(x = date.order, y = value, group = account)) +
  geom_line(aes(color = week)) +
  facet_wrap(account~week) +
  theme_bw() +
  theme(panel.margin = unit(2, "lines"), strip.background = element_blank(), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"), panel.grid.major = element_line(color = 'gray90', size = .3), strip.text.x = element_text( size = 14), axis.text = element_text(color = 'gray47'), legend.position='none')

# this is ugly but gives a better picture
ggplot() +
  geom_point(data = dat.m, aes(x = date.order, y = value, group = account, color = week)) +
  geom_line(data = dat.m[dat.m$week == 'average', ], aes(x = date.order, y = value, group = account )) +
  geom_line(data = dat.m[dat.m$week == 'MozFest', ], aes(x = date.order, y = value, group = account )) +
  theme_bw() +
  theme(panel.margin = unit(2, "lines"), strip.background = element_blank(), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"), panel.grid.major = element_line(color = 'gray90', size = .3), strip.text.x = element_text( size = 14), axis.text = element_text(color = 'gray47'))

# cool but way too misleading to put plots next to each other with different y-scales
ggplot() +
  geom_point(data = dat.m, aes(x = date.order, y = value, group = account, color = week)) +
  geom_line(data = dat.m[dat.m$week == 'average', ], aes(x = date.order, y = value, group = account )) +
  geom_line(data = dat.m[dat.m$week == 'MozFest', ], aes(x = date.order, y = value, group = account )) +
  theme_bw() +
  facet_wrap(~account, scales = 'free_y') +
  theme(panel.margin = unit(2, "lines"), strip.background = element_blank(), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"), panel.grid.major = element_line(color = 'gray90', size = .3), strip.text.x = element_text( size = 14), axis.text = element_text(color = 'gray47'))

# let's try the indexing idea (as with the population viz here: https://github.com/underthecurve/letsgetvisual/blob/master/population/populationviz.R)

# again, this code is probably inefficient but it works ... 
dat.average <- dat.m[dat.m$week == 'average' & dat.m$date =="7 Oct", ]
dat.merge <- merge(dat.m, dat.average, by = "account")
head(dat.merge)

dat.moz <- dat.m[dat.m$week == 'MozFest' & dat.m$date =="4 Nov", ]
dat.merge2 <- merge(dat.merge, dat.moz, by = "account")
head(dat.merge2)

dat.merge2$indexavg <- dat.merge2$value.x / dat.merge2$value.y
dat.merge2$indexmoz <- dat.merge2$value.x / dat.merge2$value

dat.merge2$index <- ifelse(dat.merge2$week.x == 'average', dat.merge2$indexavg, dat.merge2$indexmoz)

# much better!
ggplot() +
  geom_point(data = dat.merge2, aes(x = date.order.x, y = index, group = account, color = week.x)) +
  geom_line(data = dat.merge2[dat.merge2$week.x == 'average', ], aes(x = date.order.x, y = index, group = account)) +
  geom_line(data = dat.merge2[dat.merge2$week.x == 'MozFest', ], aes(x = date.order.x, y = index, group = account)) +
  theme_bw() +
  facet_wrap(~account) +
  theme(panel.margin = unit(2, "lines"), strip.background = element_blank(), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"), panel.grid.major = element_line(color = 'gray90', size = .3), strip.text.x = element_text( size = 14), axis.text = element_text(color = 'gray47'))

# a better view (imo)

dat.merge2$account.order <- factor(dat.merge2$account,levels = rev(dat.merge2$account[order(dat.merge2$indexmoz)]),ordered = TRUE)

dat.merge2$week2 <- ifelse(dat.merge2$week.x == 'average', 'average week', 'MozFest week')

ggplot() +
  geom_point(data = dat.merge2, aes(x = date.order.x, y = index, group = account,  color = account.order)) +
  geom_line(data = dat.merge2[dat.merge2$week.x == 'average', ], aes(x = date.order.x, y = index, group = account, color = account.order)) +
  geom_line(data = dat.merge2[dat.merge2$week.x == 'MozFest', ], aes(x = date.order.x, y = index, group = account, color = account.order)) +
  labs(x = "", y = "") +
  theme_fivethirtyeight() +
  scale_color_economist() +
  scale_y_continuous(limits = c(1, 1.3)) +
  theme(panel.margin = unit(2, "lines"), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"), panel.grid.major = element_line(color = 'gray90', size = .3), axis.text = element_text(color = 'gray47'), legend.title = element_blank(), legend.position = 'right', legend.background = element_blank(),legend.key = element_blank()) +
  guides(color=guide_legend(nrow=7)) +
  ggtitle('Growth in Twitter followers of Knight-Mozilla fellows\navg week vs. MozFest week\n')


ggsave('followers.pdf', width = 13, height = 7)

# view of raw #s 

ggplot() +
  geom_point(data = dat.merge2, aes(x = date.order.x, y = value.x, group = account, color = week2)) +
  geom_line(data = dat.merge2[dat.merge2$week.x == 'average', ], aes(x = date.order.x, y = value.x, group = account, color = week2)) +
  geom_line(data = dat.merge2[dat.merge2$week.x == 'MozFest', ], aes(x = date.order.x, y = value.x, group = account, color = week2)) +
  theme_fivethirtyeight() +
  scale_color_economist() +
  scale_x_discrete(breaks = c('7 Oct', '14 Oct', '4 Nov', '11 Nov'), labels = c('7\nOct', '14\nOct', '   4\n     Nov', '11\nNov')) +
  facet_wrap(~account, scales = 'free', ncol = 1) +
  theme(panel.margin = unit(1, "lines"), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"), panel.border = element_rect(linetype = 'solid',color = 'gray47'), panel.grid.major = element_blank(), strip.text.x = element_text(size = 11), axis.text = element_text(color = 'gray47', size = 8), legend.title = element_blank(),legend.background = element_blank(),legend.key = element_blank(), legend.position = 'top')

ggsave('followers_2.png', height = 12, width = 4)


