# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(patchwork)
library(showtext)

# Get the data -----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-31')

bird_baths <- tuesdata$bird_baths


## Color palette from https://coolors.co/f94144-f3722c-f8961e-f9844a-f9c74f-90be6d-43aa8b-4d908e-577590-277da1
col10 <- c("#F94144", "#F3722C", "#F8961E", "#F9844A", "#F9C74F", "#90BE6D", "#43AA8B", "#4D908E", "#577590", "#277DA1")

bg_col <- "#F6F6F4"
text_col <- c("#30323D", "#5B5F71", "#B74F6F")


## Fonts
font_add_google(name = "Changa", family = "changa")
font_add_google(name = "Cairo", family = "cairo")
showtext_auto()


# Preprocess data --------------------------------------------------------------
## Keep only observations labeled Rural/Urban
bird_baths_flt <- bird_baths %>% 
  filter(!is.na(urban_rural))


## Find most 'migrating' rural/urban bird species
bird_baths_sum <- bird_baths_flt %>% 
  with_groups(c(bird_type, survey_year, urban_rural), ~ summarize(.x, total_bird_count = sum(bird_count))) %>% 
  with_groups(c(bird_type, urban_rural), ~ mutate(.x, bird_count_sd = sd(total_bird_count))) %>% 
  arrange(desc(bird_count_sd)) %>% 
  pivot_wider(names_from = survey_year, values_from = total_bird_count) %>% 
  ## Add color of line segments
  mutate(line_col = if_else(`2014` > `2015`, "#94D2BD", "#E9D8A6"))

## Select top 10 most 'migrating' rural bird species
top10_rural <- bird_baths_sum %>% 
  filter(urban_rural == "Rural") %>% 
  head(10) %>% 
  mutate(dot_col = col10)

## Select top 10 most 'migrating' urban bird species
top10_urban <- bird_baths_sum %>% 
  filter(urban_rural == "Urban") %>% 
  head(10) %>% 
  mutate(dot_col = col10)



## Add text highlighting color if bird species present in both rural and urban areas
top10_rural <- top10_rural %>%
  mutate(bird_name_col = if_else(bird_type %in% top10_urban$bird_type, text_col[3], text_col[1]))

top10_urban <- top10_urban %>%
  mutate(bird_name_col = if_else(bird_type %in% top10_rural$bird_type, text_col[3], text_col[1]))



# Make plots -------------------------------------------------------------------

y_min <- min(c(top10_rural$`2014`, top10_rural$`2015`, top10_urban$`2014`, top10_urban$`2015`))
y_max <- max(c(top10_rural$`2014`, top10_rural$`2015`, top10_urban$`2014`, top10_urban$`2015`))



add_birds_theme <- function(p){
  p +
    ylim(y_min, y_max) +
    labs(x = "", y = "") +
    theme_void() +
    theme(
      axis.text.x = element_text(size = 17, color = text_col[1], family = "cairo", face = "bold"),
      plot.title = element_text(family = "changa", color = text_col[1], size = 25)) 
}


plot_urban <- ggplot(top10_urban) +
  geom_segment(aes(x = 1, xend = 2, y = `2014`, yend = `2015`), color = top10_urban$line_col, size = 0.4) +
  geom_point(aes(x = 1, y = `2014`), size = 2.5, color = top10_urban$dot_col) +
  geom_point(aes(x = 2, y = `2015`), size = 2.5, color = top10_urban$dot_col) +
  geom_text_repel(
    aes(x = 1, y = `2014`, label = bird_type), 
    size = 6,
    family = "cairo",
    color = top10_urban$bird_name_col,
    min.segment.length = 0,
    segment.alpha = 0.5,
    segment.size = 0.3,
    nudge_x = -0.15,
    direction = "y",
    hjust = 1,
    point.padding = 0.5) +
  scale_x_continuous(
    breaks = c(1,2), labels = c("Winter 2014", "Summer 2015"),
    expand = expansion(add = c(0.9, 0.1))) + 
  labs(title = "Urban area") 

plot_rural <- ggplot(top10_rural) +
  geom_segment(aes(x = 1, xend = 2, y = `2014`, yend = `2015`), color = top10_rural$line_col, size = 0.4) +
  geom_point(aes(x = 1, y = `2014`), size = 2.5, color = top10_rural$dot_col) +
  geom_point(aes(x = 2, y = `2015`), size = 2.5, color = top10_rural$dot_col) +
  geom_text_repel(
    aes(x = 2, y = `2015`, label = bird_type), 
    size = 6,
    family = "cairo",
    color = top10_rural$bird_name_col,
    min.segment.length = 0,
    segment.alpha = 0.5,
    segment.size = 0.3,
    # segment.curvature = -0.1,
    # segment.ncp = 3,
    # segment.angle = 20,
    nudge_x = 0.15,
    direction = "y",
    hjust = 0,
    point.padding = 0.5) +
  scale_x_continuous(
    breaks = c(1,2), labels = c("Winter 2014", "Summer 2015"),
    expand = expansion(add = c(0.1, 0.9))) + 
  labs(title = "Rural area") 
  

plot_urban <- add_birds_theme(plot_urban) +
  theme(plot.title = element_text(hjust = 0.75))

plot_rural <- add_birds_theme(plot_rural) +
  theme(plot.title = element_text(hjust = 0.25))
  



## Y-axis 
ladder <- tibble(y = seq(10, 130, by = 20))

ladder_plot <- ggplot(ladder) +
  geom_segment(aes(x = 0, xend = 0.25, y = y, yend = y), color = text_col[1]) +
  geom_segment(aes(x = 0.75, xend = 1, y = y, yend = y), color = text_col[1]) +
  geom_text(aes(x = 0.5, y = y, label = y), size = 7, color = text_col[1], family = "changa") +
  ylim(y_min, y_max) +
  labs(x = "", y = "") +
  theme_void()


## Layout scheme
layout <- "
AAABCCC
AAABCCC
AAABCCC
"


## Combine all into one plot
plot_urban + ladder_plot + plot_rural +
  plot_annotation(
    title = "Seasonal changes in bird occurrence at bird baths in Australia",
    # subtitle = "Birds visiting bird baths recorded over austral winter and summer periods of 2014-2015. Bird species with prominent seasonal behaviour of taking baths are shown. \nLeft section shows bird visits of bird baths located in urban areas. Right section shows bird visits of bird baths located in rural areas. Bird species common in both urban and rural areas are highlighted with pink color.",
    subtitle = "Birds visiting bird baths recorded over austral winter and summer periods of 2014-2015. Bird species with prominent seasonal behaviour of taking baths are shown.",
    caption = "Anna Valyaeva | Data: Cleary et al, 2016",
    theme = theme(
      plot.title = element_text(size = 30, color = text_col[1], family = "changa"),
      plot.subtitle = element_text(size = 15, color = text_col[1], family = "cairo"),
      plot.caption = element_text(hjust = 0.5, size = 12, color = text_col[2], family = "cairo"),
      plot.background = element_rect(fill = bg_col))) +
  plot_layout(design = layout)


ggsave("bird_baths.png", dpi = 300, width = 14, height = 8, units = "cm")


