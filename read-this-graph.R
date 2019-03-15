library(tidyverse)
library(urbnthemes)

source("01a_load-data.R")

sample <- cps %>% 
  filter(year >= 2015) %>% 
  mutate(horatemax_line = max(sample$horate_high),
         horatemin_line = max(sample$horate_low))


ggplot(data = sample, mapping = aes(date, horate)) +
  geom_ribbon(aes(ymin = horate_low, ymax = horate_high),
              fill = "#6f6f6f", alpha = .5) +
  geom_line() +
  geom_line(data = sample, mapping = aes(date, horatemax_line),
            linetype = 2) +
  geom_line(data = sample, mapping = aes(date, horatemin_line),
            linetype = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(.62, .63, .64, .65, .66),
                     limits = c(.62, .66)) +
  scale_x_date(breaks = "1 year",
               labels = scales::date_format("%Y")) +
  labs(x = NULL, y = "Homeownership rate")

ggsave("plots/read-this-graph.png")

