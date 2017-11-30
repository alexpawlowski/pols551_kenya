library(tidyverse)
mode_share <- readr::read_csv('transit_data/mode_share_kenya.csv')
source('theme_fig.R')


mode_share.plot <- mode_share %>%
  group_by(Location) %>%
  #filter(Location == "Kenyan cities") %>% %>%
  mutate(Mode = factor(Mode,
                       levels = c(
                         "Walk", "Matatu", "Bus", "Car", "Moto", "Bicycle", "Boda boda", "Others"
                       ))) %>%
  ggplot(data = ., aes(y = Share.Pct, x = Location, group = Mode)) +
  scale_color_brewer(palette = 'Paired') +
  scale_fill_brewer(palette = 'Paired') +
  geom_col(aes(color = Mode, fill = Mode),
           alpha = 0.9,
           position = 'fill') +
  # scale_x_continuous(breaks = seq(0,15,by = 3),
  #                    limits = c(0, 16),
  #                    expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2),
                     labels = function(x) x*100,
                     #limits = c(0,110),
                     expand = c(0,0)) +
  labs(
    y = 'Mode Share (%)',
    x = NULL,
    title = 'Mode Share in Nairobi compared to all Kenyan cities',
    caption = 'Data from Kenya Urbanization Review - World Bank (2016)' #http://documents.worldbank.org/curated/en/639231468043512906/pdf/AUS8099-WP-P148360-PUBLIC-KE-Urbanization-ACS.pdf
  ) +
  coord_flip() +
  theme_fig()
mode_share.plot


ggsave(mode_share.plot, filename = 'mode_share.plot.svg', width = 6, height = 3)

