#' Read clipboard data into R
#'
#' @param postcodes a character vector containing postcodes to map
#' @param minzoom a numeric defining minimum map zoom
#' @param maxzoom a numeric defining maximum map zoom
#' @param include.popup a logical indicating if postcodes should be included as a popup
#'
#' @return a data.frame consisting of data copied to clipboard
#' @export
#'
#' @description A function to easily copy data into R from a spreadsheet.
#'              This should not be used within a reproducible work flow, instead
#'              it should only be used on an ad-hoc basis
#'              (note: I'm not aware of the original author of this function,
#'              it was taken directly from a stack overflow answer)
#'
#' @examples
#' # first copy data to clipboard (i.e. highlight data in spreadsheet and press ctrl + c)
#'
#' # apply function to read data from clipboard into R
#'
#' data = read_from_clipboard()
map_basic = function(postcodes = NULL,
                     minzoom = NULL,
                     maxzoom = NULL,
                     include.popup = TRUE) {

  # run initial setup function

  temp = initial_setup(postcodes)

  postcode.summary = temp[[1]]

  data = temp[[2]]

  centre.map = temp[[3]]

  hpt.map = temp[[4]]

  sw.la.map = temp[[5]]

  # set min and max zoom

  minzoom = ifelse(is.null(minzoom), 0, minzoom)

  maxzoom = ifelse(is.null(maxzoom), 18, maxzoom)

  # apply include.popup argument (to either include/remove popup)

  if(include.popup){

    NULL

  } else {

    data$postcode  = NULL

  }

  # BASIC MAP

  map = leaflet() %>%
    # add open street view map
    addTiles(group = "OpenStreetMap",
             options = tileOptions(minZoom = minzoom, maxZoom = maxzoom)) %>%
    # add centre shape file layer
    addPolygons(data = centre.map,
                fill = TRUE,
                fillOpacity = 0.4,
                weight = 1,
                stroke = TRUE,
                opacity = 1,
                color = "black",
                fillColor = "grey",
                popup = paste("<b><a>", centre.map@data$PHECNM, "</a></b>"),
                group = "PHE Centre layer") %>%
    # add circle markers
    addCircleMarkers(lng=data$x,
                     lat=data$y,
                     popup = data$pcds,
                     #clusterOptions = markerClusterOptions(),
                     stroke = TRUE,
                     opacity = 1,
                     fillOpacity = 1,
                     radius = 3,
                     group = "Points_circle") %>%
    # add layers control
    addLayersControl(overlayGroups = c("Points_circle", "PHE Centre layer"),
                     options = layersControlOptions(collapsed = FALSE))

  return(map)

}

