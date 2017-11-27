library(tidyverse)
mode_share <- read_csv('transit_data/mode_share_kenya.csv')
source('theme_fig.R')

mode_share.plot <- mode_share %>%
  group_by(Location) %>%
  #filter(Location == "Kenyan cities") %>%
  mutate(Mode = factor(Mode, levels = rev(unique(.$Mode[order(.$Share.Pct)])))) %>%
  ggplot(data = ., aes(y = Share.Pct, x = Location, group = Mode)) +
  scale_color_brewer(palette = 'Paired') +
  scale_fill_brewer(palette = 'Paired') +
  geom_col(aes(color = Mode, fill = Mode),
           alpha = 0.9,
           position = 'stack') +
  # scale_x_continuous(breaks = seq(0,15,by = 3),
  #                    limits = c(0, 16),
  #                    expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,100, by = 20),
                     #limits = c(0,110),
                     expand = c(0,0)) +
  coord_flip() +
  theme_fig()
mode_share.plot


ggsave(mode_share.plot, filename = 'mode_share.plot.svg', width = 5, height = 3)

