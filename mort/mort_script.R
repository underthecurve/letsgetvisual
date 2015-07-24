library(ggplot2)
library(RColorBrewer)
library(WDI)

WDIsearch("mortality")

mort <- WDI(country = 'all', indicator = c('SH.DYN.MORT'), start = 1990, end = 2013, extra = T)
table(mort$income)
#View(mort[mort$country == 'High income: OECD', ] ) #OECD avg = 5.129518 in 2013
mort<- mort[ mort$income == 'High income: OECD', ]

mort$mark <- ifelse(mort$iso3c == 'USA', 1,
                         0
)

mort$mark <- as.factor(mort$mark)

ggplot(mort, aes(x = year, y = SH.DYN.MORT, group = country)) +
  geom_line(aes(color = mark, size = mark)) +
  scale_color_manual(values = c('grey', 'blue')) +
  scale_x_continuous(breaks = c(1990, 2013)) +
  scale_size_manual(values = c(.8,1)) +
  labs(x = "", y = "") +
  theme(legend.position = 'none', panel.background = element_blank(), panel.grid = element_blank(), plot.title = element_text(size = 11))

ggsave('mort_trend.png', height = 5, width = 3)

mort$country.1 <-factor(mort$country, levels=mort[order(mort$SH.DYN.MORT), "country"])


ggplot(mort[mort$year == 2013 & !is.na(mort$country), ], aes(x = country.1, y = SH.DYN.MORT)) +
  geom_bar(aes(fill = mark), stat = 'identity', width = 0.8) +
  scale_fill_manual(values = c('grey', 'blue')) +
  scale_size_manual(values = c(.8,1)) +
  labs(x = "", y = "") +
  theme(legend.position = 'none', panel.background = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size = 11)) +
  coord_flip() + 
  geom_hline(yintercept = 5.129518, color = 'red')

ggsave('mort_2013.png', height = 5, width = 3)

