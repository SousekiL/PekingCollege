# in Campus

# out Campus

# out Campus metro
sample_poi_sum <- freq_beijing_poi_sf_metro_in_buffer %>%
  filter(grepl('清华大学|北京大学', Title), grepl('本部|燕园校区', Campus)) %>%
  filter(n > 0) %>%
  select(Title, Campus) %>%
  mutate(collegeName = paste0(Title, ',', Campus))
sample_poi <- beijing_poi_sf_metro_in_buffer %>%
  filter(Title %in% sample_poi_sum$Title, Campus  %in% sample_poi_sum$Campus) %>%
  select(name, type1, type2, id, geometry, Title, Campus) %>%
  mutate(collegeName = paste0(Title, ',', Campus))

sample_college <- data %>%
  filter(Title %in% sample_poi_sum$Title, Campus %in% sample_poi_sum$Campus) %>%
  select(Title, Campus, `所在区`, `办学水平`, geometry) %>%
  mutate(collegeName = paste0(Title, ',', Campus))
sample_college_buffer <- data_buffer %>%
  filter(Title %in% sample_poi_sum$Title, Campus %in% sample_poi_sum$Campus) %>%
  select(Title, Campus, `所在区`, `办学水平`, geometry) %>%
  mutate(collegeName = paste0(Title, ',', Campus))

plot <- tm_shape(sample_college_buffer) + 
  tm_borders() +
  tm_facets(by = c('Title', 'Campus')) +
  tm_shape(sample_college) + 
  tm_borders() +
  tm_facets(by = c('Title', 'Campus')) +
  tm_shape(sample_poi) + 
  tm_dots(col = "type2", 
          size = 0.01, 
          palette = c("#fb8072", "#8dd3c7")) +
  tm_text("name", size = 1) +
  tm_layout(
    fontfamily = 'Canger',
    frame = T,
    panel.label.size = 1,
    legend.show = TRUE,
    legend.text.size = 1,
    asp = 3/4
  )
print(plot)
