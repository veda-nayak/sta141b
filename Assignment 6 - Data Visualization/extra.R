# Extra


# ==============================================================================
# sd <- SharedData$new(pd[!is.na(pd$county),])
# 
# scatter = sd %>% ggplot(aes(x = sqft, 
#                             y = newPrice, 
#                             color = county, 
#                             label1 = beds, 
#                             label2 = baths,
#                             label3 = yearBuilt)) + 
#   geom_point(size = 3, alpha = .6)  
# labs(x = "Square Footage",
#      y = "Price",
#      color = "County") +
#   theme_bw() 
# 
# map <- leaflet() %>% 
#   addTiles() %>%
#   fitBounds(lng1 = -122, lng2 = -122, lat1 = 38, lat2 = 39.5) %>% 
#   addCircleMarkers(data = sd,
#                    lat = ~lat, 
#                    lng = ~long,
#                    # color = ~pals(days),
#                    popup = ~as.factor(newPrice))
# 
# scatter = pd %>% ggplot(aes(x = sqft, 
#                             y = newPrice, 
#                             color = county, 
#                             label1 = beds, 
#                             label2 = baths,
#                             label3 = yearBuilt, 
#                             label4 = )) + 
#                   geom_point(size = 3, alpha = .6)  
#                   labs(x = "Square Footage",
#                        y = "Price",
#                        color = "County") +
#                     theme_bw()
# 
# scatter_interact = ggplotly(scatter, tooltip = c("label1", 
#                                                  "label2", 
#                                                  "label3")) %>%
#   config(
#     modeBarButtonsToAdd = list('zoomIn2d', 'zoomOut2d'),
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = list('lasso2d', 'select2d')
#   )
# 
# saveWidget(map, "map.html")
# saveWidget(scatter_interact, "scatter.html") 
# saveWidget(scatterFinal, "scatter.html") 
# 

# ==============================================================================
# 
# sd <- SharedData$new(pd[!is.na(pd$county),])
# 
# scatter2 = ggplot(sd, aes(x = sqft,
#                           y = newPrice,
#                           color = county,
#                           text = paste0(
#                             "Beds: ", beds, "\n",
#                             "Baths: ", baths, "\n",
#                             "Year Built: ", yearBuilt, "\n"
#                           ),
#                           customdata = url
#                   )) + 
#   geom_point(size = 3, alpha = .6) + 
# labs(x = "Square Footage",
#      y = "Price",
#      color = "County") +
#   theme_bw() 
# 
# scatter_interact2 = ggplotly(scatter2, tooltip = c("text")) %>%
#   config(
#     modeBarButtonsToAdd = list('zoomIn2d', 'zoomOut2d'),
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = list('lasso2d', 'select2d'),
#     responsive = TRUE
#   )
# 
# scatterFinal = onRender(
#                         scatter_interact2, "
#                       function(el) {
#                         el.on('plotly_click', function(d) {
#                           var url = d.points[0].customdata;
#                           window.open(url);
#                         });
#                       }
#                     "
#                       )
# 
# map <- leaflet(sd) %>% 
#   addTiles() %>%
#   fitBounds(lng1 = -122, lng2 = -122, lat1 = 38, lat2 = 39.5) %>% 
#   addCircleMarkers(data = sd,
#                    lat = ~lat, 
#                    lng = ~long,
#                    # color = ~pals(days),
#                    popup = ~as.factor(newPrice))
# 
# words = "hello world"
# 
# a = div(
#   bscols(widths = c(6, 6),
#          words),
#   bscols(widths = c(6, 6),
#          filter_checkbox("bedroom", "Bedrooms", sd, ~bedrooms, inline = TRUE),
#          filter_slider("newPrice", "Price", sd, ~newPrice, step = 1000)
#   ),
#   bscols(widths = c(6, 6),
#          scatterFinal,
#          map
#   ),
#   datatable(sd, options = list(scrollX = TRUE))
# )
# 
# save_html(a, "linked.html")
# 


# ==============================================================================
# pd$url_link <- paste0('<a href="', pd$url, '" target="_blank">View Listing</a>')
# pd_new <- pd[ ,c(1:3, 26, 5:25)]
# sd <- SharedData$new(pd_new[!is.na(pd$county),])
# 
# scatter <- ggplot(sd, aes(x = sqft, 
#                           y = newPrice, 
#                           color = county, 
#                           label1 = beds, 
#                           label2 = baths,
#                           label3 = yearBuilt, 
#                           label4 = url_link)) + 
#   geom_point(size = 3, alpha = .6) +
#   labs(x = "Square Footage",
#        y = "Price",
#        color = "County") +
#   theme_bw() 
# 
# scatter_interact <- ggplotly(scatter, tooltip = c("label1", 
#                                                   "label2", 
#                                                   "label3", 
#                                                   "label4")) %>%
#   config(
#     modeBarButtonsToAdd = list('zoomIn2d', 'zoomOut2d'),
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = list('lasso2d', 'select2d'), 
#     responsive = TRUE
#   )
# 
# map <- leaflet(sd) %>% 
#   addTiles() %>%
#   fitBounds(lng1 = -122, lng2 = -122, lat1 = 38, lat2 = 39.5) %>% 
#   addCircleMarkers(data = sd,
#                    lat = ~lat, 
#                    lng = ~long,
#                    popup = ~as.factor(newPrice))
# 
# words <- ""
# 
# a <- div(
#   bscols(widths = c(6, 6),
#          words),
#   bscols(widths = c(6, 6),
#          filter_checkbox("bedroom", "Bedrooms", sd, ~bedrooms, inline = TRUE),
#          filter_slider("newPrice", "Price", sd, ~newPrice, step = 1000)
#   ),
#   bscols(widths = c(6, 6),
#          scatter_interact,
#          map
#   ),
#   datatable(sd, 
#             options = list(scrollX = TRUE), 
#                       escape = FALSE)
# )
# 
# save_html(a, "linked.html")

# ==============================================================================

# scatter <- ggplot(sd, aes(x = sqft, 
#                           y = newPrice, 
#                           color = county, 
#                           label1 = beds, 
#                           label2 = baths,
#                           label3 = yearBuilt, 
#                           label4 = url_link)) + 
#   geom_point(size = 3, alpha = .6) +
#   labs(x = "Square Footage",
#        y = "Price",
#        color = "County") +
#   theme_bw() 
# 
# scatter_interact <- ggplotly(scatter, tooltip = c("label1", 
#                                                   "label2", 
#                                                   "label3", 
#                                                   "label4")) %>%
#   config(
#     modeBarButtonsToAdd = list('zoomIn2d', 'zoomOut2d'),
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = list('lasso2d', 'select2d'), 
#     responsive = TRUE
#   )

# ==============================================================================

# map <- leaflet(sd) %>% 
#   addTiles() %>%
#   fitBounds(lng1 = -122.5, lng2 = -121.5, lat1 = 37.5, lat2 = 39.5) %>% 
#   addCircleMarkers(
#     lat = ~lat,
#     lng = ~long,
#     # I wanted to add more info here because clicking on locations on the plot didn't translate to table info poppin up
#     popup = ~paste("Price: $", format(newPrice, big.mark = ","),
#                    "<br>Beds:", beds,
#                    "<br>Baths:", baths,
#                    "<br>Sq Ft:", format(sqft, big.mark = ","),
#                    "<br>County:", county,
#                    "<br>", url_link),
#     radius = 6,
#     fillOpacity = 0.7,
#     clusterOptions = markerClusterOptions(),
#     layerId =  ~row_id
#   )


# scatter_interact = ggplotly(scatter, tooltip = "text") %>%
# config(
#   modeBarButtonsToAdd = list('zoomIn2d', 'zoomOut2d'),
#   displaylogo = FALSE,
#   modeBarButtonsToRemove = list('lasso2d', 'select2d'),
#   responsive = TRUE
# ) %>%
#   highlight(on = "plotly_click", 
#             off = "plotly_doubleclick", 
#             persistent = TRUE)

# plotlyMap = plot_ly(sd, 
#                     x = ~long, 
#                     y = ~lat,
#                     color = ~county,
#                     colors = colorPalette,
#                     text = ~paste("Price: $", format(newPrice, big.mark = ","),
#                                   "<br>Beds:", beds,
#                                   "<br>Baths:", baths,
#                                   "<br>Sq Ft:", format(sqft, big.mark = ",")),
#                     type = "scattermapbox",
#                     marker = list(size = 8)) %>%
#   layout(
#     mapbox = list(
#       style = "open-street-map", 
#       center = list(lon = -122, lat = 38.5),
#       zoom = 6
#     ),
#     showlegend = TRUE) %>%
#   config(
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = list('lasso2d', 'select2d'),
#     responsive = TRUE
#   ) %>%
#   highlight(on = "plotly_click", 
#             off = "plotly_doubleclick", 
#             color = 'black', 
#             persistent = TRUE)