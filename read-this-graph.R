library(tidyverse)
library(urbnthemes)
library(lubridate)

set_urbn_defaults(style = "print")

cps <- read_csv("data/cps-horate_2000_2019-10-01.csv") %>% 
  mutate(year_label = ifelse(quarter == "Q1",
                             year,
                             ""))

sample <- cps %>% 
  filter(year >= 2015) %>% 
  mutate(horatemax_line = horate_high[date == max(.$date)],
         horatemin_line = horate_low[date == max(.$date)])

rate <- sample %>% 
  filter(date == max(.$date)) %>% 
  pull(horate)

breaks <- sample %>% 
  pull(date)

labels <- sample %>% 
  pull(year_label)

howtoread <- ggplot(data = sample, mapping = aes(date, horate)) +
  geom_ribbon(aes(ymin = horatemin_line, ymax = horatemax_line),
              alpha = 0.23,
              fill = "#6f6f6f") +
  annotate(geom = "rect",
           xmin = ymd("2019-10-01"), xmax = ymd("2020-09-01"),
           ymin = 0.619, ymax = 0.66,
           fill = "white", color = NA) +
  geom_ribbon(aes(ymin = horate_low, ymax = horate_high, fill = region),
              alpha = .35,
              show.legend = TRUE) +
  scale_fill_manual(values = "#1696d2",
                    labels = "90% confidence interval") +
  geom_line(aes(color = region),
            size = 1.2) +
  scale_color_manual(values = "#1696d2",
                     labels = "Homeownership rate") +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  geom_line(data = sample, mapping = aes(date, horatemax_line),
            linetype = 2, color = "#6f6f6f", alpha = 0.5) +
  annotate(geom = "text", x = ymd("2016-04-15"), y = mean(sample$horatemax_line) + 0.0015,
           label = "Upper bound confidence interval for Q4 2019",
           color = "#6f6f6f",
           size = 3)+
  geom_line(data = sample, mapping = aes(date, horatemin_line),
            linetype = 2, color = "#6f6f6f", alpha = 0.5) +
  annotate(geom = "text", x = ymd("2016-04-15"), y = mean(sample$horatemin_line) - 0.0015,
           label = "Lower bound confidence interval for Q4 2019",
           color = "#6f6f6f",
           size = 3) +
  geom_point(data = filter(sample, date == max(sample$date)),
             mapping = aes(x = date, y = horatemax_line),
             color = "black", alpha = 0.5, size = 2) +
  geom_point(data = filter(sample, date == max(sample$date)),
             mapping = aes(x = date, y = horatemin_line),
             color = "black", alpha = 0.5, size = 2) +
  geom_point(data = filter(sample, date == max(sample$date)),
             mapping = aes(x = date, y = horate),
             color = "#1696d2", size = 2) +
  annotate(geom = "text", x = ymd("2019-10-14"),
           y = rate,
           label = "The value for\nthe 4th quarter of\n 2019 is in this \n range",
           size = 3,
           hjust = 0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(.62, .63, .64, .65, .66),
                     limits = c(.617, .66),
                     expand = c(0,0)) +
  scale_x_date(breaks = breaks,
               labels = labels,
               expand = c(0, 0),
               limits = c(ymd("2015-01-01"), ymd("2020-09-01"))) +
  labs(x = NULL, y = NULL)

p <- urbn_plot(urbn_title("US Homeownership Rate, Housing Vacancy Survey"),
          get_legend(howtoread),
          urbn_y_title("Homeownership rate"),
          remove_legend(howtoread),
          urbn_source("US Census Bureau's Housing Vacancy Survey."),
          urbn_note("Data for Housing Vacancy Survey are quarterly."),
          urbn_logo_text(),
          heights = c(0.1, 0.05, 0.05, 1, 0.035, 0.035, 0.035))

ggsave("plots/read-this-graph.png",
       plot = p,
       height = 5, width = 6.5)

