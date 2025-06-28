
dir = "/Users/bayleighbaldwin/Desktop/Veda's R Homework laptop pleasE COME BACK/Veda Nayak_Assignment 6"
setwd(dir)

library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(XML)
library(patchwork)
library(ggiraph)
library(DT)
library(ggforce)
library(crosstalk)


pd = readRDS("PropertyData.rds")

names(pd)

table(pd$lat > 43)
table(pd$lat < 31)

table(pd$long < -125)
table(pd$long > -113)

wrongLat = which(pd$lat < 31)
pd[wrongLat, 19:20] = NA


table(pd$price != pd$price.1)
weirdPrices = which(pd$price != pd$price.1)


pd$newPrice = pd$price
pd[weirdPrices, 25] <- sapply(weirdPrices, function(x) mean(pd[x, 1], pd[x, 21]))

plot(x = pd$sqft, y = pd$newPrice)

pd[which(pd$sqft == 100), ] = NA
pd[which(pd$sqft > 200000), ] = NA
pd[which(pd$sqft > 60000), ] = NA

max(pd[!is.na(pd[,2]), 2])
m1 = which(pd[,2] == max(pd[!is.na(pd[,2]), 2]))
pd[m1, 2] <- NA

table(pd$type.1)
weirdLocations = which(pd$type.1 == 'Coop' | pd$type.1 == 'Farm'| pd$type.1 == 'Land' |pd$type.1 ==  'Multi-Family')
pd[weirdLocations, ]
pd[weirdLocations, ] <- NA

names(pd)
# pd[weirdBeds, c(2, 3, 10, 6)]# based on a quick look these all seem like apartment complexes --> kick them out
# pd[weirdBeds, ] <- NA

# table(pd$type)
# apts = which(pd$type == 'Apartment')
# pd[apts, ] <- NA



table(pd$county)

pd$url_link <- paste0('<a href="', pd$url, '" target="_blank">View Listing</a>')
pd_new <- pd[, c(1:3, 26, 5:25)]
pd_new$row_id <- 1:nrow(pd_new)
pdShared <- pd_new[!is.na(pd_new$county), c(25, 13, 11, 10, 15, 22, 23, 6, 7, 24, 17, 4, 26, 19, 20)]
              
sd <- SharedData$new(pdShared,
                     key = ~row_id)

colorPalette = c("Marin" = "#ff595e", 
                 "Napa" = "#ff924c",
                 "Sacramento" = "#ffca3a",
                 "Solano"= "#8ac926",
                 "Sonoma" = "#EDFF7A", 
                 "Sutter"= "#1982c4",
                 "Yolo" = "#6a4c93",
                 "Yuba" = "#f78ef0"
               )

pastelPalette = c("Marin" = "#ffe6e7", 
                  "Napa" = "#fff0e6",
                  "Sacramento" = "#fff8e6",
                  "Solano"= "#f0f9e6",
                  "Sonoma" = "#fcffe6", 
                  "Sutter"= "#e6f2fc",
                  "Yolo" = "#f0e6f5",
                  "Yuba" = "#fef0fe")


css = tags$head(
  tags$style(HTML("
    body { 
      margin: 0; 
      padding: 20px; 
      overflow-x: auto; 
    }
    h1 {
      text-align: center;
      line-height: 0.6;
    }
    h2 {
      text-align: center;
      line-height: 0.6;
    }
    p{
      line-height: 0.3;
      }
  "))
)

scatter = ggplot(sd, aes(x = sqft, 
                          y = newPrice, 
                          color = county, 
                          text = paste("Beds:", beds, 
                                       "<br>Baths:", baths,
                                       "<br>Year Built:", yearBuilt))) + 
  geom_point(size = 3, alpha = 0.9) +
  labs(x = "Square Footage",
       y = "Price",
       color = "County") +
  scale_color_manual(values = colorPalette)
  theme_bw()

scatter_interact = ggplotly(scatter, tooltip = "text")

plotlyMap = plot_ly(sd, 
                      x = ~long, 
                      y = ~lat,
                      color = ~county,
                      colors = colorPalette,
                      text = ~paste("Price: $", format(newPrice, big.mark = ","),
                                    "<br>Beds:", beds,
                                    "<br>Baths:", baths,
                                    "<br>Sq Ft:", format(sqft, big.mark = ",")),
                      type = "scattermapbox",
                      marker = list(size = 8)) %>%
  layout(
    mapbox = list(
      style = "open-street-map", 
      center = list(lon = -122, lat = 38.5),
      zoom = 6
    ),
    showlegend = TRUE) %>%
  highlight(color = 'black')

a <- div(
  css,
  bscols(widths = c(12),
         h1("Interactive Visualizations"),
         h2("Author: Veda Nayak"),
         p("Clicked and corrsponding residences on the scatter plot, map, and table will show up as follows:"),
         p("   - Scatter Plot: The selected term will be highlighted. Remaing terms will loose opacity."),
         p("   - Map: The selected term will be highlighted in black."),
         p("   - Table: The selected term will become the only one visible."),
         p("If you want to select multiple points, hold shift as you click on them."),
         p("To unselect terms and reset your view, double click in the white space on the graph."),
         p("If you would like to unselect a whole county, click on the county on the legand."),
         p("If you have specifications on what property you are looking for, you can use the filters to narrow down."),
         p("Have a good day!")
         
  ),
  
  bscols(widths = c(2, 5, 5),
         h3("Filters:"),
         filter_checkbox("bedrooms", "Bedrooms", sd, ~beds, inline = TRUE),
         filter_checkbox("baths", "Baths", sd, ~baths, inline = TRUE)
         
  ),
  
  bscols(widths = c(6, 6),
         div(h3("Square Footage vs. Price"), scatter_interact),
         div(h3("Property Locations"), plotlyMap)
  ),
  
  div(
    h3("Property Details"),
    datatable(sd, 
              filter = 'top',
              colnames = c( 'Property',
                            'Price',
                            'Square Feet',
                            'Beds',
                            'Baths',
                            'Garage Capacity',
                            'Last Selling Price',
                            'Last Sold Date',
                            'Street Address',
                            'Address Locality',
                            'County',
                            'Year Built',
                            'Listing Link',
                            'Row ID', 
                            'Longitude',
                            'Latitude'),
              options = list(
                pageLength = 10, 
                scrollX = TRUE
              ), 
              escape = FALSE) %>%
      formatStyle(columns = 1:ncol(pdShared),
                  valueColumns = 'county', 
                  backgroundColor = styleEqual(names(pastelPalette), pastelPalette))
  )
)

save_html(a, "linked.html")

