#########################################
## Code that makes an error plot in the paper
##


library(tidyverse)
library(ggvoronoi)
library(ggthemes)

load("ncdcBigDataWithForecast.RData")

##### FT Wayne, IN
special.loc <- "USW00014827"

ncdc.special <- ncdc.forecast %>%
  filter(ID == special.loc,
         Date >= as.Date("2016-04-01"),
         Date <= as.Date("2016-04-15")) %>%
  mutate(ErrorLasso = TMAXF - Day1LassoForecastedTMaxF,
         ErrorHull = TMAXF - Day1HullForecastedTMaxF)

ncdc.special.tall <- ncdc.special %>%
  dplyr::select(Date, TMAXF, Day1LassoForecastedTMaxF, Day1HullForecastedTMaxF) %>%
  pivot_longer(-Date, names_to="Series") %>%
  mutate(Series = factor(Series, levels=c("TMAXF", "Day1LassoForecastedTMaxF", "Day1HullForecastedTMaxF"),
                         labels=c("Observed", "LASSO Based Forecast", "Hull Based Forecast")) )

p.error.ts <- ggplot(ncdc.special.tall) + 
  geom_line(aes(x=Date, y=value, linetype=Series, color=Series), size=1) +
  labs(y="Daily High Temperature (F)", x="April") + 
  scale_linetype("") + 
  scale_color_manual("", values=c("gray40", "black", "gray20")) + 
  scale_x_date(date_breaks="day", date_labels="%d") + 
  theme_bw() +
  theme(legend.position = c(0.25, 0.87),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
p.error.ts


ggsave(plot=p.error.ts, filename="paper/fortWayneErrorTS.jpg", width=9, height=5.5, dpi=300)
