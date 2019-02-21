#' initial setup pre-mapping
#'
#' @param postcodes a character vector containing postcodes to map
#'
#' @return a list
#'
#' @description A helper function to convert postcodes to coordinates and PHE geographies
#'              and read in shape files
#'
initial_setup = function(postcodes = NULL, local.authority.area = "SW") {

  # check local.authority.area input

  if(tolower(local.authority.area) %in% c("ne", "nw", "yh", "em", "eoe", "wm", "lon", "se", "seal", "sw", "eng")){

    NULL

  } else {

    stop("local.authority.area must be one of the following: ne, nw, yh, em, eoe, wm,
         lon, se, seal, sw, eng")

  }

  # function to
  # 1. convert postcodes to longitude and latitude
  # 2. load shape files
  # 3. return converted data and shape files

  ##############################################################################
  # 1. convert postcodes to longitude and latitude

  # set working directory to same folder that this R script is saved in

  # load postcodes (either from clipboard or specified as an argument)

  if(is.null(postcodes)) {

    # read in data from clipboard

    temp = read_from_clipboard()

    # extract out as postcode

    postcodes = as.character(temp[, 1])

  } else {

    NULL

  }

  # apply get.geog function

  data = rgisws::postcode_lookup(postcodes,
                                 c("oslaua", "hptcd", "phec", "phereg", "lsoa11", "msoa11", "ccg"),
                                 xy = 4326,
                                 return_names = TRUE)

  # summarise number of postcodes with missing latitude/longitude

  postcode.summary = paste0("Postcodes which matched successfully: ", sum(!(is.na(data$x) | is.na(data$y))),
                            "\n",
                            "Postcodes which failed to match: ", sum(is.na(data$x) | is.na(data$y)))

  # remove postcodes which failed to match

  data = data[!(is.na(data$x) | is.na(data$y)), ]


  ##############################################################################
  # 2. load shape files

  # note - shape files are contained as hidden datasets in the package, these are titled:
  # centre.map, hpt.map, em.la.map, eoe.la.map, lon.la.map, ne.la.map, nw.la.map
  # se.la.map, seal.la.map, sw.la.map, wm.la.map, yh.la.map, eng.la.map

  # create order column

  centre.map@data$order = 1:nrow(centre.map@data)

  # append number of cases for each polygon onto each shape file

  centre.map@data = merge(x = centre.map@data, y= data.frame(table(data$phec_nm)),
                          by.x = "PHECNM", by.y = "Var1", all.x = TRUE)

  centre.map@data$Freq[is.na(centre.map@data$Freq)] = 0

  # create shape file label

  centre.map@data$label = paste("<b><a>", centre.map@data$PHECNM, "</a></b>",
                                "<br/> Number of cases:", centre.map@data$Freq)

  # reorder

  centre.map@data = centre.map@data[order(centre.map@data$order), ]


  # create order column

  hpt.map@data$order = 1:nrow(hpt.map@data)

  # append number of cases for each polygon onto each shape file

  hpt.map@data = merge(x = hpt.map@data, y= data.frame(table(data$hptcd_nm)),
                       by.x = "HPTNM", by.y = "Var1", all.x = TRUE)

  hpt.map@data$Freq[is.na(hpt.map@data$Freq)] = 0

  # create shape file label

  hpt.map@data$label = paste("<b><a>", hpt.map@data$HPTNM, "</a></b>",
                             "<br/> Number of cases:", hpt.map@data$Freq)

  # reorder

  hpt.map@data = hpt.map@data[order(hpt.map@data$order), ]

  # grab appropriate local authority layer

  la.map = get(paste0(tolower(local.authority.area), ".la.map"))

  # create order column

  la.map@data$order = 1:nrow(la.map@data)

  # append number of cases for each polygon onto each shape file

  la.map@data = merge(x = la.map@data, y= data.frame(table(data$oslaua_nm)),
                      by.x = "GSS_NM", by.y = "Var1", all.x = TRUE)

  la.map@data$Freq[is.na(la.map@data$Freq)] = 0

  # create shape file label

  la.map@data$label = paste("<b><a>", la.map@data$GSS_NM, "</a></b>",
                            "<br/> Number of cases:", la.map@data$Freq)

  # reorder

  la.map@data = la.map@data[order(la.map@data$order), ]



  ##############################################################################
  # 3. return converted data and shape files

  return(list(postcode.summary, data, centre.map, hpt.map, la.map))

}
