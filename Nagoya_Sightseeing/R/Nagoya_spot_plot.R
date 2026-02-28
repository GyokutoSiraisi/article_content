
# 準備 ----------------------------------------------------------------------

## パッケージの読み込み
pacman::p_load(tidyverse,
               ggrepel,
               arrow,
               sf)

## データの読み込み
spot <-
  arrow::read_csv_arrow("data/tourist_spot_Nagoya.csv") %>% 
  drop_na() %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE)
aichi_shape <-
  read_sf("data/Aichi_district", options = "ENCODING=SHIFT-JIS")
nagoya_shape <-
  aichi_shape %>%
  filter(N03_003 == "名古屋市") %>% 
  st_union()



# 地図をプロット -----------------------------------------------------------------

windowsFonts("Meiryo" = windowsFont("Meiryo"))

## 愛知県
p1 <-
  ggplot() +
  geom_sf(data = aichi_shape, fill = "#0BBF5B", color = NA) +
  geom_sf(data = nagoya_shape, fill = "#7acc89", color = NA, linewidth = 1.2) +
  geom_sf(data = spot, color = "#013220", size = 2, shape = 16) +
  geom_label_repel(
    data = spot %>% mutate(Name = ifelse(nagoya == 1, "", Name)), 
    aes(x = lng, y = lat, label = Name),
    size = 3,
    color = "black",
    box.padding = 0.5, # ラベルと点の距離
    family = "Meiryo") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F2F2F2"))
p1
ggsave("output/aichi_spot.png",
       plot = p1,
       dpi = 300,
       width = 900*3,
       height = 500*3,
       units = "px")

## 名古屋市
p2 <-
  ggplot() +
  geom_sf(data = aichi_shape %>% filter(N03_003 == "名古屋市"),
          fill = "#0BBF5B", color = "#7acc89") +
  geom_sf(data = spot %>% filter(nagoya == 1),
          color = "#013220", size = 2, shape = 16) +
  geom_label_repel(
    data = spot %>% filter(nagoya == 1), 
    aes(x = lng, y = lat, label = Name),
    size = 3,
    color = "black",
    box.padding = 0.5, # ラベルと点の距離
    family = "Meiryo") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F2F2F2"))
p2
ggsave("output/nagoya_spot.png",
       plot = p2,
       dpi = 300,
       width = 1200 * 3,
       height = 550 * 3,
       units = "px")




