
# TODO: html can't be intermediate file; must go directly to raster.
# Can we just download the html, then convert to raster? So don't need to re-download the html?
# We just need to call it again?

#' @import tidyverse
#' @import googleway
#' @import htmlwidgets
#' @import webshot
#' @import raster
#' @import png
#' @import plotwidgets
#' @import httr
#' @import sf

if(T){
  library(tidyverse)
  library(googleway)
  library(htmlwidgets)
  library(webshot)
  #library(REdaS)
  library(raster)
  library(png)
  library(plotwidgets)
  library(httr)
  library(sf)
  #library(rgeos)
  #library(mapview)
  #library(geosphere)
}

if(F){
  roxygen2::roxygenise("~/Documents/github/googletraffic")
}


#' Get traffic raster from Bing Maps
#'
#' This function returns a raster of traffic levels from Bing Maps.
#'
#' @param latitude Latitude of center of area
#' @param longitude Longitude of center of area
#' @param height Height
#' @param width Width
#' @param zoom Zoom; integer from 0 to 20. For more information, see [here](https://wiki.openstreetmap.org/wiki/Zoom_levels)
#' @param bing_key Bing API key
#' 
#' @return Returns a georeferenced raster file. The file can contain the following values: 1 = no traffic; 2 = light traffic; 3 = moderate traffic; 4 = heavy traffic.
#' @export
bing_traffic <- function(latitude,
                         longitude,
                         height,
                         width,
                         zoom,
                         bing_key){
  
  #### Create URLs to query
  # https://docs.microsoft.com/en-us/bingmaps/rest-services/imagery/get-a-static-map
  style <- paste("me|sc:ffffff;lv:0;lbc:ffffff;loc:000000;bv:0",
                 "trs|sc:ffffff;fc:ffffff;bsc:ffffff;boc:ffffff;lv:0;bv:0",
                 "pl|v:0;bv:0",
                 "pt|v:0;bv:0",
                 "ad|v:0;bv:0",
                 "ar|v:0;bv:0",
                 "wt|sc:ffffff;fc:ffffff;lv:0;bv:0",
                 "rd|sc:ffffff;fc:ffffff;lv:0;lbc:0;bv:0",
                 "str|v:0;bv:0",
                 "np|fc:ffffff;sc:ffffff;lv:0;bv:0",
                 "hg|fc:ffffff;sc:ffffff;lv:0;bv:0",
                 "cah|fc:ffffff;sc:ffffff;lv:0;bv:0",
                 "ard|fc:ffffff;sc:ffffff;lv:0;bv:0",
                 "mr|fc:ffffff;sc:ffffff;lv:0;bv:0",
                 "rl|fc:ffffff;sc:ffffff;lv:0;bv:0;v:0",
                 "transit|v:0;bv:0;fc:ffffff;sc:ffffff",
                 "g|lv:0;sc:ffffff;lc:ffffff;bsc:ffffff;boc:ffffff;bv:0",
                 sep="_")
  
  bing_metadata_url <- paste0("https://dev.virtualearth.net/REST/v1/Imagery/Map/Road/",
                              latitude,",",longitude,"/",zoom,
                              "?mapSize=",height,",",width,
                              "&style=",style,
                              "&mmd=1",
                              "&mapLayer=TrafficFlow&format=png&key=",bing_key)
  
  bing_map_url <- paste0("https://dev.virtualearth.net/REST/v1/Imagery/Map/Road/",
                         latitude,",",longitude,"/",zoom,
                         "?mapSize=",height,",",width,
                         "&style=",style,
                         "&mapLayer=TrafficFlow&format=png&key=",bing_key)
  
  #### Grab bbox from metadata
  md <- bing_metadata_url %>% GET() %>% content(as="text") %>% fromJSON 
  bbox <- md$resourceSets$resources[[1]]$bbox[[1]]
  
  #### Grab map as matrix; values as colors
  response <- httr::GET(bing_map_url)
  rimg <- httr::content(response)
  rimg <- aperm(rimg, c(2, 1, 3))
  rimg <- apply(rimg, 2, rgb)
  
  #### Assign colors
  colors_df <- rimg %>% table() %>% as.data.frame() %>%
    dplyr::rename(hex = ".")
  colors_df$hex <- colors_df$hex %>% as.character()
  
  ## Assign traffic colors based on hsl
  hsl_df <- colors_df$hex %>% 
    col2hsl() %>%
    t() %>%
    as.data.frame() 
  
  colors_df <- bind_cols(colors_df, hsl_df)
  
  colors_df <- colors_df %>%
    mutate(color = case_when(((H == 0) & (S < 0.2)) ~ "background",
                             ((H >= 349) & (H <= 351)) ~ "dark-red",
                             H >= 354 & H <= 355 & S >= 0.85 ~ "red",
                             H >= 31 & H <= 33 & S == 1 ~ "orange",
                             H >= 149 & H <= 152 & S >= 0.8 ~ "green")) 
  
  ## Apply traffic colors to raster
  colors_unique <- colors_df$color %>% unique()
  colors_unique <- colors_unique[!is.na(colors_unique)]
  colors_unique <- colors_unique[!(colors_unique %in% "background")]
  for(color_i in colors_unique){
    color_num <- NA
    if(color_i == "dark-red") color_num <- 4
    if(color_i == "red")      color_num <- 3
    if(color_i == "orange")   color_num <- 2
    if(color_i == "green")    color_num <- 1
    
    rimg[rimg %in% colors_df$hex[colors_df$color %in% color_i]] <- color_num
  }
  
  rimg_num <- matrix(as.numeric(rimg),    
                     ncol = ncol(rimg)) %>% t()
  
  #### Convert to raster
  r <- raster(rimg_num)
  extent(r) <- c(bbox[2], bbox[4],
                 bbox[1], bbox[3])
  
  crs(r) <- CRS("+init=epsg:4326")
  
  return(r)
}

#' Make traffic html from Google
#'
#' This function returns an html of traffic from Google. The `gt_html_to_raster()` can
#' then be used to convert this html into a georeferenced raster file. 
#'
#' @param location Vector of latitude and longitude
#' @param height Height
#' @param width Width
#' @param zoom Zoom; integer from 0 to 20. For more information, see [here](https://wiki.openstreetmap.org/wiki/Zoom_levels)
#' @param filename Path and filename to save file
#' @param google_key Google API key
#' @param save_params Save an .Rds file that contains the parameters (location, height, width and zoom). This file can then be used by the `gt_html_to_raster()` function.
#' 
#' @return Returns an html file of Google traffic
#' @export
gt_make_html <- function(location,
                         height,
                         width,
                         zoom,
                         filename,
                         google_key){
  
  #### Define style; all white background
  # Adapted from: https://snazzymaps.com/style/95/roadie
  style <- '[
    {
        "elementType": "labels",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "elementType": "geometry",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "road",
        "elementType": "geometry",
        "stylers": [
            {
                "visibility": "on"
            },
            {
                "color": "#ffffff"
            }
        ]
    },
    {
        "featureType": "landscape",
        "stylers": [
            {
                "color": "#ffffff"
            },
            {
                "visibility": "on"
            }
        ]
    },
    {}
]'
  
  #### Create map
  gmap <- google_map(key = google_key,
                     location = location,
                     zoom = zoom,
                     height = height,
                     width = width,
                     styles = style,
                     zoom_control = F,
                     map_type_control = F,
                     scale_control = F,
                     fullscreen_control = F,
                     rotate_control = F,
                     street_view_control = F) %>%
    add_traffic() 
  
  saveWidget(gmap, 
             filename, 
             selfcontained = T)
  
  #### Also creates folder; delete that
  unlink(filename %>% str_replace_all(".html$", "_files"), 
         recursive = T)
  
  return(NULL)
}

# det_google_pixel_dist_m <- function(latitude, zoom){
#   # https://wiki.openstreetmap.org/wiki/Zoom_levels
#   pixel_dist_m <- (2*pi*6378137*cos(deg2rad(latitude))/2^zoom)/256
#   
#   return(pixel_dist_m)
# }

#' Determine pixel distance in decimal degrees
#'
#' Based on the zoom level, determine the height/width of each pixel in decimal degrees
#'
#' @param zoom Zoom level
#'
#' @return Returns the pixel height/width in decimal degrees (integer)
det_google_pixel_dist_deg <- function(zoom){
  # https://wiki.openstreetmap.org/wiki/Zoom_levels
  pixel_dist_deg <- 360/(2^zoom)/256
  
  return(pixel_dist_deg)
}

#' Determine the spatial extent of a Google traffic tile
#'
#' Based on the location, height, width, and zoom, determines the spatial extent of the Google traffic tile
#'
#' @param latitude Latitude
#' @param longitude Longitude
#' @param height Height
#' @param width Width
#' @param zoom Zoom level
#'
#' @return Returns an extent object
gt_make_extent <- function(latitude,
                           longitude,
                           height,
                           width,
                           zoom){
  
  pixel_dist_deg <- det_google_pixel_dist_deg(zoom)
  
  point_right    <- longitude + pixel_dist_deg*width/2
  point_left     <- longitude - pixel_dist_deg*width/2
  point_bottom   <- latitude - pixel_dist_deg*height/2
  point_top      <- latitude + pixel_dist_deg*height/2
  
  r_extent <- extent(point_left,
                     point_right,
                     point_bottom,
                     point_top)
  
  return(r_extent)
}

#' Creates grid of points to query Google Traffic 
#'
#' Querying too large of a location may be unfeasible; consequently, it may be necessary to query multiple smaller locations. Based on the location to be queried and the height, width and zoom parameters, determines the points that should be queried.
#'
#' @param polygon `SpatialPolygonsDataframe` the defines region to be queried.
#' @param height Height
#' @param width Width
#' @param zoom Zoom level
#' @param reduce_hw Number of pixels to reduce height/width by. The tiles produced by the function may not exactly overlap. Reducing the height and width ensures overlap to eventually remove any blank space.
#'
#' @return Returns a dataframe with the locations to query and parameters.
#' @export
gt_make_point_grid <- function(polygon,
                               height,
                               width,
                               zoom,
                               reduce_hw = 100){
  
  ## Polygon should be sf object
  if(class(polygon)[1] %in% "SpatialPolygonsDataFrame"){
    polygon <- polygon %>% st_as_sf()
  }
  
  ## Reduce height/width
  # Extents may not perfectly connect. Reducing the height and width aims to create
  # some overlap in the extents, so all the tiles will connect.
  height_use <- height - reduce_hw
  width_use  <- width  - reduce_hw
  
  ## Pixel distance (degrees)
  pixel_dist_deg   <- det_google_pixel_dist_deg(zoom)
  
  ## Make raster and convert to polygon
  r <- raster(ext = extent(polygon), res=c(width_use*pixel_dist_deg,
                                           height_use*pixel_dist_deg))
  
  p <- as(r, "SpatialPolygonsDataFrame") %>% st_as_sf()
  
  ## Only keep polygons (boxes) that intersect with original polygon
  p_inter_tf <- st_intersects(p, polygon, sparse=F) %>% as.vector()
  p_inter <- p[p_inter_tf,]
  
  ## Grab points
  points_df <- p %>% 
    st_centroid() %>%
    st_coordinates() %>%
    as.data.frame() %>%
    dplyr::rename(longitude = X,
                  latitude = Y) %>%
    mutate(id = 1:n(),
           height = height,
           width = width,
           zoom = zoom) 
  
  # points_df <- p_inter %>%
  #   coordinates() %>%
  #   as.data.frame() %>%
  #   dplyr::rename(longitude = V1,
  #                 latitude = V2) %>%
  #   mutate(id = 1:n(),
  #          height = height,
  #          width = width,
  #          zoom = zoom) 
  
  return(points_df)
}

#' Converts Google HTML file to Raster
#' 
#' Converts a Google HTML file into a spatially referenced raster file. 
#'
#' @param polygon `SpatialPolygonsDataframe` the defines region to be queried.
#'
#' ## If `save_params` is set to `FALSE` in `gt_make_html` or `gt_make_htmls_from_grid`, then the following must be specified
#' @param height Height
#' @param width Width
#' @param zoom Zoom level
#'
#' ## Other parameters
#' @param webshot_delay How long to wait for .html file to load. Larger .html files will require more time to fully load.
#' @param save_png The function creates a .png file as an intermediate step. Specify whether the .png file should be kept (default: `FALSE`)
#'
#' @return Returns a georeferenced raster file. The file can contain the following values: 1 = no traffic; 2 = light traffic; 3 = moderate traffic; 4 = heavy traffic.
#' @export
gt_html_to_raster <- function(filename,
                              location = NULL,
                              height = NULL,
                              width = NULL,
                              zoom = NULL,
                              webshot_delay = 10,
                              save_png = F){
  
  ## Grab parameters from dataframe
  if(is.null(location) | is.null(height) | is.null(width) | is.null(zoom)){
    param_df_filename <- filename %>% str_replace_all(".html$", "_params.Rds")
    
    if(file.exists(param_df_filename)){
      param_df <- readRDS(param_df_filename)
      
      location = param_df$location
      height   = param_df$height
      width    = param_df$width
      zoom     = param_df$zoom
      
    } else{
      stop("location, height, width, or zoom not specified and parameter dataframe doesn't exist")
    }
    
  }
  
  #### Make lat/lon
  latitude = location[1]
  longitude = location[2]
  
  #### Convert .html to png
  filename_root <- filename %>% str_replace_all(".html$", "")
  filename_only <- basename(filename_root)
  filename_dir <- filename_root %>% str_replace_all(paste0("/", filename_only), "")
  
  setwd(filename_dir)
  webshot(paste0(filename_only,".html"),
          file = paste0(filename_only,".png"),
          vheight = height,
          vwidth = width,
          cliprect = "viewport",
          delay = webshot_delay,
          zoom = 1)
  
  #### Load as raster and image
  r   <- raster(file.path(filename_dir,  paste0(filename_only, ".png")),1)
  img <- readPNG(file.path(filename_dir, paste0(filename_only, ".png")))
  
  #### Assign traffic colors 
  ## Image to hex
  rimg <- as.raster(img) 
  colors_df <- rimg %>% table() %>% as.data.frame() %>%
    dplyr::rename(hex = ".")
  colors_df$hex <- colors_df$hex %>% as.character()
  
  ## Assign traffic colors based on hsl
  hsl_df <- colors_df$hex %>% 
    col2hsl() %>%
    t() %>%
    as.data.frame() 
  
  colors_df <- bind_cols(colors_df, hsl_df)
  
  colors_df <- colors_df %>%
    mutate(color = case_when(#((H == 0) & (S < 0.2)) ~ "background",
      ((H == 0) & (S >= 0.2)) ~ "dark-red",
      H > 0 & H <= 5 ~ "red",
      H >= 20 & H <= 28 ~ "orange",
      H >= 120 & H <= 130 ~ "green"))
  
  ## Apply traffic colors to raster
  colors_unique <- colors_df$color %>% unique()
  colors_unique <- colors_unique[!is.na(colors_unique)]
  #colors_unique <- colors_unique[!(colors_unique %in% "background")]
  rimg <- matrix(rimg) #%>% raster::t() #%>% base::t()
  for(color_i in colors_unique){
    rimg[rimg %in% colors_df$hex[colors_df$color %in% color_i]] <- color_i
  }
  
  r[] <- NA
  r[rimg %in% "green"]    <- 1
  r[rimg %in% "red"]      <- 2
  r[rimg %in% "orange"]   <- 3
  r[rimg %in% "dark-red"] <- 4
  
  ## Spatially define raster
  extent(r) <- gt_make_extent(latitude,
                              longitude,
                              height,
                              width,
                              zoom)
  
  crs(r) <- CRS("+init=epsg:4326")
  
  ## Delete png
  if(save_png %in% F){
    unlink(file.path(filename_dir, paste0(filename_only,".png")))
  }
  
  return(r)
}

#' Make multiple traffic html from Google based on grid of points
#'
#' This function returns multiple html of traffic from Google based on a grid defined by `gt_make_point_grid`. The `gt_htmls_to_raster()` can then be used to these html files into a georeferenced raster file. 
#'
#' @param location Vector of latitude and longitude
#' @param height Height
#' @param width Width
#' @param zoom Zoom; integer from 0 to 20. For more information, see [here](https://wiki.openstreetmap.org/wiki/Zoom_levels)
#' @param filename Path and filename to save file
#' @param google_key Google API key
#' @param save_params Save an .Rds file that contains the parameters (location, height, width and zoom). This file can then be used by the `gt_html_to_raster()` function.
#' 
#' @return Returns an html file of Google traffic
#' @export
gt_make_htmls_from_grid <- function(grid_param_df,
                                    filename_suffix,
                                    out_dir,
                                    google_key,
                                    save_params = T){
  
  #### Time to add to filename
  # time <- Sys.time() %>% 
  #   as.numeric() %>% 
  #   as.character() %>% 
  #   str_replace_all("[[:punct:]]", "")
  
  #### Make HTML files
  for(id in grid_param_df$id){
    
    points_to_query_i <- points_to_query[points_to_query$id %in% id,]
    
    gt_make_html(location = c(points_to_query_i$latitude, points_to_query_i$longitude),
                 height = points_to_query_i$height,
                 width = points_to_query_i$height,
                 zoom = points_to_query_i$zoom,
                 filename = paste0(id,"_",filename_suffix,".html"),
                 google_key = google_key,
                 save_params = save_params)
    
  }
  
  return("Done!")
}

#' Converts multiple Google HTML files into a raster
#' 
#' Converts a multiple Google HTML files into a single spatially referenced raster file. 
#' 
#' @param html_files Vector of html_files
#' @param webshot_delay How long to wait for .html file to load. Larger .html files will require more time to fully load.
#'
#' ## If `save_params` is set to `FALSE` in `gt_make_htmls_from_grid`, then the following must be specified
#' @param grid_param_df Grid parameter dataframed defined by `gt_make_point_grid()`
#'
#' ## Other parameters
#' @param save_png The function creates a .png file as an intermediate step. Specify whether the .png file should be kept (default: `FALSE`)
#' @param print_progress Whether to print progress to show which file the function is processing (default: `TRUE`)
#' 
#' @return Returns a georeferenced raster file. The file can contain the following values: 1 = no traffic; 2 = light traffic; 3 = moderate traffic; 4 = heavy traffic.
#' @export
gt_htmls_to_raster <- function(html_files,
                               webshot_delay,
                               grid_param_df = NULL,
                               save_png = F,
                               print_progress = T){
  
  r_list <- lapply(html_files, function(file_i){
    if(print_progress){
      print(paste0("Processing: ", file_i))
    }
    
    if(!is.null(grid_param_df)){
      id <- file_i %>% 
        str_replace_all(".*/", "") %>% 
        str_replace_all("_.*", "") %>% 
        as.numeric()
      
      param_i <- grid_param_df[grid_param_df$id %in% id,]
      
      location = c(param_i$latitude, param_i$longitude)
      
    } else{
      
      params_filename <- file_i %>% str_replace_all(".html$", "_params.Rds")
      param_i     <- readRDS(params_filename)
      
      location = param_i$location
    }
    
    height   = param_i$height
    width    = param_i$width
    zoom     = param_i$zoom
    
    gt_html_to_raster(file_i,
                      c(param_i$latitude, param_i$longitude),
                      height = param_i$height,
                      width = param_i$width,
                      zoom = param_i$zoom,
                      webshot_delay = webshot_delay,
                      save_png = save_png)
  })
  
  names(r_list)    <- NULL
  r_list$fun       <- max
  r_list$tolerance <- 1
  
  r_all <- do.call(mosaic, r_list)
  
  return(r_all)
}

#' Make Google Traffic Raster
#' 
#' Make a raster from Google traffic data, where each pixel has one of four values
#' indicating traffic volume (no traffic, light, moderate, and heavy).
#' 
#' @param location Vector of latitude and longitude
#' @param height Height
#' @param width Width
#' @param zoom Zoom level; integer from 0 to 20. For more information, see [here](https://wiki.openstreetmap.org/wiki/Zoom_levels)
#' @param webshot_delay How long to wait for google traffic layer to render. Larger height/widths require longer delay times.
#'
#' @return Returns a georeferenced raster file. The file can contain the following values: 1 = no traffic; 2 = light traffic; 3 = moderate traffic; 4 = heavy traffic.
#' @export
gt_make_raster <- function(location,
                           height,
                           width,
                           zoom,
                           webshot_delay){
  
  ## Filename; as html
  filename_html <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
  
  ## Make html
  gt_make_html(location = location,
               height = height,
               width = width,
               zoom = zoom,
               filename = filename_html,
               google_key = google_key)
  
  ## Make raster
  r <- gt_html_to_raster(filename = filename_html,
                         location = location,
                         height = height,
                         width = width,
                         zoom = zoom,
                         webshot_delay = webshot_delay)
  
  ## Delete html file
  unlink(filename_html)
  
  return(r)
}

#' Make Google Traffic Raster Based on Grid of Coordinates
#' 
#' Make a raster from Google traffic data, where each pixel has one of four values
#' indicating traffic volume (no traffic, light, moderate, and heavy).
#' 
#' @param grid_param_df Grid parameter dataframe produced from `gt_make_point_grid`
#' @param webshot_delay How long to wait for google traffic layer to render. Larger height/widths require longer delay times.
#' @param print_progress Show progress for which tile has been processed.
#'
#' @return Returns a georeferenced raster file. The file can contain the following values: 1 = no traffic; 2 = light traffic; 3 = moderate traffic; 4 = heavy traffic.
#' @export
gt_make_raster_from_grid <- function(grid_param_df,
                                     webshot_delay,
                                     print_progress = T){
  
  ## Make list of rasters
  r_list <- lapply(1:nrow(grid_param_df), function(i){
    
    if(print_progress){
      print(paste0("Processing tile ",i," out of ",nrow(grid_param_df)))
    } 
    
    param_i <- grid_param_df[i,]
    
    r_i <- gt_make_raster(location      = c(param_i$latitude, param_i$longitude),
                          height        = param_i$height,
                          width         = param_i$width,
                          zoom          = param_i$zoom,
                          webshot_delay = webshot_delay)
    
    return(r_i)
  })
  
  ## Mosaic rasters together
  names(r_list)    <- NULL
  r_list$fun       <- max
  r_list$tolerance <- 1
  
  r <- do.call(mosaic, r_list)
  
  return(r)
}

#' Make Google Traffic Raster Based on Polygon
#' 
#' Make a raster from Google traffic data, where each pixel has one of four values
#' indicating traffic volume (no traffic, light, moderate, and heavy).
#' 
#' @param polygon `SpatialPolygonsDataframe` in WGS84 CRS.
#' @param height Height
#' @param width Width
#' @param zoom Zoom level; integer from 0 to 20. For more information, see [here](https://wiki.openstreetmap.org/wiki/Zoom_levels)
#' @param webshot_delay How long to wait for google traffic layer to render. Larger height/widths require longer delay times.
#' @param reduce_hw Number of pixels to reduce height/width by. The tiles produced by the function may not exactly overlap. Reducing the height and width ensures overlap to eventually remove any blank space.
#' @param print_progress Show progress for which tile has been processed.
#'
#' @return Returns a georeferenced raster file. The file can contain the following values: 1 = no traffic; 2 = light traffic; 3 = moderate traffic; 4 = heavy traffic.
#' @export
gt_make_raster_from_polygon <- function(polygon,
                                        height,
                                        width,
                                        zoom,
                                        webshot_delay,
                                        reduce_hw = 0,
                                        print_progress = T){
  
  grid_param_df <- gt_make_point_grid(polygon   = polygon,
                                      height    = height,
                                      width     = width,
                                      zoom      = zoom,
                                      reduce_hw = reduce_hw)
  
  if(print_progress){
    print(paste0("Raster will be created from ",
                 nrow(grid_param_df),
                 " Google traffic tiles."))
  }
  
  r <- gt_make_raster_from_grid(grid_param_df,
                                webshot_delay)
  
  return(r)
}