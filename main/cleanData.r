## Buffering a GeoJSON file in R

# read geojson file and use tmap package to visualize it
library(geojsonio)
library(tmap)
library(tmaptools)
library(sf)
library(dplyr)
library(showtext)
library(glue)
font_add("Canger", "/Library/Fonts/仓耳今楷01-W04.ttf")
font_families()
showtext_auto()  # 全局自动使用
showtext_auto(FALSE) # 不需要就关闭

setwd("/Users/sousekilyu/Documents/GitHub/PekingCollege")
# Specify the path to your GeoJSON file
geojson_file <- "data/北京普通高等学校（分校区）.geojson"

# Read the GeoJSON file and transfer its prj to 4326
data <- st_read(geojson_file) %>%
  rename(Campus = "校区") %>%
  mutate(collegeName = paste0(Title, ',', Campus))
data <- st_transform(data, crs = 4326)

# Set tmap options to check and fix geometry
tmap_options(check.and.fix = TRUE)

# Visualize the data
qtm(data)

# Install and load the lwgeom package
if (!require(lwgeom)) {
  install.packages("lwgeom")
}
library(lwgeom)

# Make the data valid
data <- st_make_valid(data) 

# Now try to create the buffer of 1km

data_buffer <- st_buffer(data, dist = 500)

# Visualize the data_buffer
qtm(data_buffer)

## Read data of Beijing reaction facilities

# read Beijing poi csv data and transfer it to sf data
beijing_poi <- read.csv("/Users/sousekilyu/Documents/Meta Data/高德poi/2022高德poi/北京POI数据/北京市POI数据.csv")
# rename col names
colnames(beijing_poi) <- c("name",
                           "type1",
                           "type2",
                           "longitude",
                           "latitude",
                           "province",
                           "city",
                           "district")
beijing_poi_sf <- st_as_sf(beijing_poi,
                           coords = c("longitude", "latitude"),
                           crs = 4326)

# filter
beijing_poi_sf_csm <- beijing_poi_sf %>%
  filter(type1 %in% c('购物消费', '餐饮美食', '休闲娱乐', '生活服务', '运动健身'),
         !(type2 %in% c('中介', '公共事业', '信息咨询中心', '公厕', '其他', '家居建材', '彩票销售'))) %>%
  mutate(type1 =  factor(type1, levels = c('购物消费', '餐饮美食', '休闲娱乐', '生活服务', '运动健身'))) %>%
  unique() %>%
  mutate(id = 1:nrow(.))

beijing_poi_sf_metro <- beijing_poi_sf %>%
  filter((type2 %in% c('公交站', '地铁'))) %>%
  mutate(type2 =  factor(type2, levels = c('公交站', '地铁'))) %>%
  unique() %>%
  mutate(id = 1:nrow(.))
# use tm_dots function to visualise it and set color in c("#fb8072", "#8dd3c7", "#ffffb3", "#bebada", "#80b1d3")
tm_shape(beijing_poi_sf_csm) + 
  tm_dots(col = "type1", 
          size = 0.01, 
          palette = c("#fb8072", "#8dd3c7", "#ffffb3", "#bebada", "#80b1d3")) +
  tm_layout(
    fontfamily = 'Canger',
    frame = T,
    panel.label.size = 1,
    legend.show = TRUE,
    asp = 3/4
  )

## Merge Beijing College data and Facilities data

# pick up beijing_poi_sf_csm which is in the buffer of data_buffer, remaining c(Title, Campus, `所在区`, `办学水平`, gemoetry) of data_buffer and all columns of beijing_poi_sf_csm
beijing_poi_sf_csm_in_buffer <- st_join(beijing_poi_sf_csm, data_buffer, join = st_within)
beijing_poi_sf_csm_in <- st_join(beijing_poi_sf_csm, data, join = st_within)
beijing_poi_sf_metro_in_buffer <- st_join(beijing_poi_sf_metro, data_buffer, join = st_within)


# group by beijing_poi_sf_csm_in_buffer$collegeName, summarise the number of unique id 
# and sort by the number of unique id
freq_beijing_poi_sf_csm_in_buffer <- beijing_poi_sf_csm_in_buffer %>%
  group_by(Title, Campus) %>%
  summarise(n = n_distinct(id)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(!is.na(Title))

freq_beijing_poi_sf_csm_in <- beijing_poi_sf_csm_in %>%
  group_by(Title, Campus) %>%
  summarise(n = n_distinct(id)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(!is.na(Title))

freq_beijing_poi_sf_metro_in_buffer <- beijing_poi_sf_metro_in_buffer %>%
  group_by(Title, Campus) %>%
  summarise(n = n_distinct(id)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(!is.na(Title))
  
## plot reaction
sample_poi_sum <- freq_beijing_poi_sf_csm_in_buffer %>%
  filter(grepl('清华大学|北京大学', Title), grepl('本部|燕园Campus', Campus)) %>%
  filter(n > 0) %>%
  select(Title, Campus) %>%
  mutate(collegeName = paste0(Title, ',', Campus))

sample_college <- data %>%
  filter(Title %in% sample_poi_sum$Title, Campus %in% sample_poi_sum$Campus) %>%
  select(Title, Campus, `所在区`, `办学水平`, geometry) %>%
  mutate(collegeName = paste0(Title, ',', Campus))
sample_college_buffer <- data_buffer %>%
  filter(Title %in% sample_poi_sum$Title, Campus %in% sample_poi_sum$Campus) %>%
  select(Title, Campus, `所在区`, `办学水平`, geometry) %>%
  mutate(collegeName = paste0(Title, ',', Campus))
sample_poi <- beijing_poi_sf_csm_in_buffer %>%
  filter(Title %in% sample_poi_sum$Title, Campus  %in% sample_poi_sum$Campus) %>%
  select(name, type1, type2, id, geometry, Title, Campus) %>%
  mutate(collegeName = paste0(Title, ',', Campus))

plot <- tm_shape(sample_college_buffer) + 
  tm_borders() +
  tm_shape(sample_college) + 
  tm_borders() +
  tm_shape(sample_poi) + 
  tm_dots(col = "type1", 
          size = 0.01, 
          palette = c("#fb8072", "#8dd3c7", "#ffffb3", "#bebada", "#80b1d3")) +
  #tm_text("collegeName", size = 0.5) +
  tm_layout(
    fontfamily = 'Canger',
    frame = T,
    panel.label.size = 1,
    legend.show = TRUE,
    asp = 3/4
  )
print(plot)
