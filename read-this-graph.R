library(tidyverse)
library(urbnthemes)
library(lubridate)

set_urbn_defaults(style = "print")

cps <- read_csv("data/cps-horate_2000_2019-04-01.csv")

sample <- cps %>% 
  filter(year >= 2015) %>% 
  mutate(horatemax_line = horate_high[date == max(.$date)],
         horatemin_line = horate_low[date == max(.$date)])


howtoread <- ggplot(data = sample, mapping = aes(date, horate)) +
  geom_ribbon(aes(ymin = horate_low, ymax = horate_high, fill = region),
              alpha = .5,
              show.legend = TRUE) +
  scale_fill_manual(values = "#6f6f6f",
                    labels = "90% confidence interval") +
  geom_line(aes(color = region)) +
  scale_color_manual(values = "#1696d2",
                     labels = "Homeownership rate") +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  geom_line(data = sample, mapping = aes(date, horatemax_line),
            linetype = 2) +
  annotate(geom = "text", x = ymd("2016-05-01"), y = mean(sample$horatemax_line) + 0.0015,
           label = "This is the upper bound of the 90% confidence interval",
           color = "#1696d2")+
  geom_line(data = sample, mapping = aes(date, horatemin_line),
            linetype = 2) +
  annotate(geom = "text", x = ymd("2017-12-01"), y = mean(sample$horatemin_line) - 0.0015,
           label = "This is the lower bound of the 90% confidence interval",
           color = "#1696d2") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(.62, .63, .64, .65, .66),
                     limits = c(.62, .66)) +
  scale_x_date(breaks = "1 year",
               labels = scales::date_format("%Y"),
               expand = c(0, 0)) +
  labs(x = NULL, y = NULL)

p <- urbn_plot(urbn_title("How To Read This Graph"),
          get_legend(howtoread),
          urbn_y_title("Homeownership rate"),
          remove_legend(howtoread),
          urbn_source("Current Population Survey/Housing Vacancy Survey"),
          urbn_logo_text(),
          heights = c(0.1, 0.05, 0.05, 1, 0.05, 0.05))

ggsave("plots/read-this-graph.png",
       plot = p,
       height = 5, width = 6.5)

