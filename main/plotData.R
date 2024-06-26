source('/Users/sousekilyu/Documents/GitHub/PekingCollege/main/cleanData.r')

getSampleData <- function(freq_data, poi_data, data_buffer, rank) {
  .sample_poi_sum <- freq_data %>%
    filter(n > 0) %>%
    arrange(desc(n)) %>%
    `[`(1:rank, )
  assign("sample_poi_sum", .sample_poi_sum, envir = .GlobalEnv)
  
  .sample_poi <- poi_data %>%
    filter(collegeName %in% sample_poi_sum$collegeName) %>%
    select(name, type1, type2, id, geometry, Title, Campus, collegeName)
  assign("sample_poi", .sample_poi, envir = .GlobalEnv)
  
  .sample_college <- data %>%
    filter(collegeName %in% sample_poi_sum$collegeName) %>%
    select(Title, Campus, `所在区`, `办学水平`, geometry, collegeName)
  assign("sample_college", .sample_college, envir = .GlobalEnv)
  
  .sample_college_buffer <- data_buffer %>%
    filter(collegeName %in% sample_poi_sum$collegeName) %>%
    select(Title, Campus, `所在区`, `办学水平`, geometry, collegeName)
  assign("sample_college_buffer", .sample_college_buffer, envir = .GlobalEnv)
}

# in Campus
tmp_name = "吃喝玩乐(校园内)"
pal = c("#fb8072", "#8dd3c7", "#ffffb3", "#bebada", "#80b1d3")
lab = c('购物消费', '餐饮美食', '休闲娱乐', '生活服务', '运动健身')
getSampleData(freq_beijing_poi_sf_csm_in,
              beijing_poi_sf_csm_in,
              data_buffer,
              10)

plot <- tm_shape(sample_college) +
  tm_borders(lwd = 2, col = "white") +
  tm_facets(by = 'collegeName', ncol = 3, nrow = 4) +
  tm_shape(sample_poi) +
  tm_dots(
    col = "type1",
    title = "吃喝玩乐",
    border.lwd = NA,
    size = 0.1,
    alpha = .7,
    palette = pal,
    legend.show = FALSE
  ) +
  tm_add_legend(
    type = "fill",
    col = pal,
    labels = lab,
    title = "吃喝玩乐"
  ) +
  tm_facets(by = 'collegeName', ncol = 3, nrow = 4) +
  tm_shape(bj_highway) +
  tm_lines(col = "gray60",
           alpha = 0.7,
           lwd = 1) +
  #tm_text("name", size = 1) +
  tm_layout(
    fontfamily = 'Canger',
    main.title = glue('{tmp_name}'),
    main.title.size = 2.5,
    main.title.position = 0,
    frame = T,
    panel.label.size = 2,
    #legend.show = TRUE,
    legend.title.size = 2,
    legend.text.size = 2,
    legend.outside = T,
    legend.outside.position = "right",
    legend.outside.size = .15,
    bg.color = "black",
    scale = 0.75
  )
print(plot)

tmap_save(
  plot,
  glue('plot/{tmp_name}.png'),
  dpi = 300,
  width = 3000,
  height = 4000
)
# out Campus
tmp_name = "吃喝玩乐(校园外500米)"
pal = c("#fb8072", "#8dd3c7", "#ffffb3", "#bebada", "#80b1d3")
lab = c('购物消费', '餐饮美食', '休闲娱乐', '生活服务', '运动健身')
getSampleData(freq_beijing_poi_sf_csm_in_buffer,
              beijing_poi_sf_csm_in_buffer,
              data_buffer,
              10)

plot <- tm_shape(sample_college_buffer) +
  tm_borders(lwd = 2,
             col = "gray",
             lty = "dashed") +
  tm_facets(by = 'collegeName', ncol = 3, nrow = 4) +
  tm_shape(sample_college) +
  tm_borders(lwd = 2, col = "white") +
  tm_facets(by = 'collegeName', ncol = 3, nrow = 4) +
  tm_shape(sample_poi) +
  tm_dots(
    col = "type1",
    title = "吃喝玩乐",
    border.lwd = NA,
    size = 0.1,
    alpha = .7,
    palette = pal,
    legend.show = FALSE
  ) +
  tm_add_legend(
    type = "fill",
    col = pal,
    labels = lab,
    title = "吃喝玩乐"
  ) +
  tm_facets(by = 'collegeName', ncol = 3, nrow = 4) +
  tm_shape(bj_highway) +
  tm_lines(col = "gray60",
           alpha = 0.7,
           lwd = 1) +
  #tm_text("name", size = 1) +
  tm_layout(
    fontfamily = 'Canger',
    main.title = glue('{tmp_name}'),
    main.title.size = 2.5,
    main.title.position = 0,
    frame = T,
    panel.label.size = 2,
    #legend.show = TRUE,
    legend.title.size = 2,
    legend.text.size = 2,
    legend.outside = T,
    legend.outside.position = "right",
    legend.outside.size = .15,
    bg.color = "black",
    scale = 0.75
  )
print(plot)

tmap_save(
  plot,
  glue('plot/{tmp_name}.png'),
  dpi = 300,
  width = 3000,
  height = 4000
)

# out Campus 1000
tmp_name = "吃喝玩乐(校园外1000米)"
pal = c("#fb8072", "#8dd3c7", "#ffffb3", "#bebada", "#80b1d3")
lab = c('购物消费', '餐饮美食', '休闲娱乐', '生活服务', '运动健身')
getSampleData(
  freq_beijing_poi_sf_csm_in_buffer1000,
  beijing_poi_sf_csm_in_buffer1000,
  data_buffer1000,
  10
)

plot <- tm_shape(sample_college_buffer) +
  tm_borders(lwd = 2,
             col = "gray",
             lty = "dashed") +
  tm_facets(by = 'collegeName', ncol = 3, nrow = 4) +
  tm_shape(sample_college) +
  tm_borders(lwd = 2, col = "white") +
  tm_facets(by = 'collegeName', ncol = 3, nrow = 4) +
  tm_shape(sample_poi) +
  tm_dots(
    col = "type1",
    title = "吃喝玩乐",
    border.lwd = NA,
    size = 0.1,
    alpha = .7,
    palette = pal,
    legend.show = FALSE
  ) +
  tm_add_legend(
    type = "fill",
    col = pal,
    labels = lab,
    title = "吃喝玩乐"
  ) +
  tm_facets(by = 'collegeName', ncol = 3, nrow = 4) +
  tm_shape(bj_highway) +
  tm_lines(col = "gray60",
           alpha = 0.7,
           lwd = 1) +
  #tm_text("name", size = 1) +
  tm_layout(
    fontfamily = 'Canger',
    main.title = glue('{tmp_name}'),
    main.title.size = 2.5,
    main.title.position = 0,
    frame = T,
    panel.label.size = 2,
    #legend.show = TRUE,
    legend.title.size = 2,
    legend.text.size = 2,
    legend.outside = T,
    legend.outside.position = "right",
    legend.outside.size = .15,
    bg.color = "black",
    scale = 0.75
  )
print(plot)

tmap_save(
  plot,
  glue('plot/{tmp_name}.png'),
  dpi = 300,
  width = 3000,
  height = 4000
)


## icon
