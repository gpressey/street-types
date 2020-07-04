# Graham Pressey
# July 4, 2020
# Creates maps of the most common street types in every municipality in Canada

library(tidyverse)
library(janitor)
library(sf)
library(extrafont)

loadfonts(device = "win")
theme_set(theme_minimal(base_size = 24))

canada <- sf::st_read(dsn = here::here("data", "canada", "lrnf000r18a_e.shp")) %>% 
  clean_names()

c <- canada %>%
  filter(
    !str_detect(rank,"1|2|3"),
    !str_detect(type,"HWY|FWY"),
    !is.na(type)) %>% 
  mutate(
    pr = str_remove(prname_l, " / .*"),
    type = tolower(type)
  )

csd <- unique(c$csduid_l)

# WARNING: produces approximately 4700 image files, ~1-2 hours total run time

map(csd, function(csd){
  shp <- c %>% 
    filter(csduid_l == csd)
  
  top <- shp %>% 
    group_by(name, type) %>% 
    tally() %>% 
    group_by(type) %>% 
    tally() %>% 
    arrange(-n)
  
  #print(top)
  
  shp <- shp %>% 
    mutate(
      suffix_top = 
        factor(type, levels = top$type) %>% 
        fct_other(keep = top$type[1:12])
    ) %>% 
    filter(suffix_top != "Other")
  
  csd_name <- unique(shp$csdname_l)
  pr_name <- unique(shp$pr)
  
  p <- shp %>% 
    ggplot() + 
    geom_sf(aes(colour = suffix_top)) + 
    facet_wrap(~ suffix_top) +
    labs(
      title = paste(csd_name, " (", pr_name, ")", sep = ""),
      subtitle = "Most common street types"
      ) + 
    theme(
      text = element_text(family = "Tw Cen MT"),
      plot.title.position = "plot",
      axis.line = element_blank(), 
      panel.grid = element_blank(), 
      axis.text = element_blank()) + 
    scale_color_viridis_d(begin = 0.2, end = 0.5,direction = -1, guide = F)
  
  filename = here::here("charts", paste0(csd, ".png"))
  
  ggsave(plot = p, filename = filename, width = 16, height = 12, units = "in")
})
