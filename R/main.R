# TODO
# 1. Google Maps Traffic
# 2. Bing Traffic; more extensive colors

# https://stackoverflow.com/questions/9298326/google-static-maps-with-traffic-and-public-transport-overlays

library(tidyverse)
library(googleway)
library(htmlwidgets)
library(webshot)
library(mapview)
library(REdaS)
library(raster)
library(png)
library(plotwidgets)
library(geosphere)
library(httr)
library(rgeos)

api_keys_df <- read_csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv")

map_key <- api_keys_df %>%
  dplyr::filter(Service == "Google Directions API",
                Account == "ramarty@email.wm.edu") %>%
  pull(Key)

bing_key <- api_keys_df %>%
  dplyr::filter(Service == "Bing Maps",
                Account == "robmarty3@gmail.com") %>%
  pull(Key)


# Make functions ---------------------------------------------------------------
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
#' @return Raster with the following values: 1 = No traffic; 2 = Light traffic; 3 = Moderate traffic; 4 = Heavy traffic
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

make_html <- function(location,
                      height,
                      width,
                      zoom,
                      out_dir,
                      filename_prefix,
                      map_key,
                      time = NULL){
  
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
  
  
  gmap <- google_map(key = map_key,
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
  
  ## Filename
  if(is.null(time)){
    time <- Sys.time() %>% as.numeric() %>% as.character() %>% str_replace_all("[[:punct:]]", "")
  }
  
  ## Save as html
  fname <- paste0(filename_prefix, "utc", time)
  saveWidget(gmap, 
             file.path(out_dir,paste0(fname,".html")), 
             selfcontained = T)
  
  ## Also creates folder; delete that
  unlink(file.path(out_dir, paste0(fname, "_files")), recursive = T)
  
  return(NULL)
}

det_google_pixel_dist_m <- function(latitude, zoom){
  # https://wiki.openstreetmap.org/wiki/Zoom_levels
  pixel_dist_m <- (2*pi*6378137*cos(deg2rad(latitude))/2^zoom)/256
  
  return(pixel_dist_m)
}

det_google_pixel_dist_deg <- function(zoom){
  # https://wiki.openstreetmap.org/wiki/Zoom_levels
  pixel_dist_deg <- 360/(2^zoom)/256
  
  return(pixel_dist_deg)
}

make_extent <- function(latitude,
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

make_point_grid <- function(polygon,
                            height,
                            width,
                            zoom,
                            reduce_hw = 0){
  
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
  
  p <- as(r, "SpatialPolygonsDataFrame")
  
  ## Only keep polygons (boxes) that intersect with original polygon
  p_inter_tf <- gIntersects(p, polygon, byid=T) %>% as.vector()
  p_inter <- p[p_inter_tf,]
  
  ## Grab points
  points_df <- p_inter %>%
    coordinates() %>%
    as.data.frame() %>%
    dplyr::rename(longitude = V1,
                  latitude = V2) %>%
    mutate(id = 1:n(),
           height = height,
           width = width,
           zoom = zoom) 
  
  return(points_df)
}

html_to_raster <- function(filename,
                           latitude,
                           longitude,
                           height,
                           width,
                           zoom,
                           webshot_delay = 10,
                           save_png = F){
  
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
    mutate(color = case_when(((H == 0) & (S < 0.2)) ~ "background",
                             ((H == 0) & (S >= 0.2)) ~ "dark-red",
                             H > 0 & H <= 5 ~ "red",
                             H >= 20 & H <= 28 ~ "orange",
                             H >= 120 & H <= 130 ~ "green"))
  
  ## Apply traffic colors to raster
  colors_unique <- colors_df$color %>% unique()
  colors_unique <- colors_unique[!is.na(colors_unique)]
  colors_unique <- colors_unique[!(colors_unique %in% "background")]
  rimg <- matrix(rimg) #%>% raster::t() #%>% base::t()
  for(color_i in colors_unique){
    rimg[rimg %in% colors_df$hex[colors_df$color %in% color_i]] <- color_i
  }
  
  r[] <- 0
  r[rimg %in% "green"]    <- 1
  r[rimg %in% "red"]      <- 2
  r[rimg %in% "orange"]   <- 3
  r[rimg %in% "dark-red"] <- 4
  
  ## Spatially define raster
  extent(r) <- make_extent(latitude,
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

# Implement functions ----------------------------------------------------------
#### Points to query
nbo <- getData('GADM', country ='KEN', level=1)
nbo <- nbo[nbo$NAME_1 %in% "Nairobi",]

grid_param_df <- make_point_grid(polygon = nbo,
                                 height = 6000,
                                 width = 6000,
                                 zoom = 16,
                                 reduce_hw = 100)

make_htmls_grid <- function(grid_param_df,
                            filename_prefix,
                            out_dir,
                            map_key){
  
  #### Time to add to filename
  time <- Sys.time() %>% 
    as.numeric() %>% 
    as.character() %>% 
    str_replace_all("[[:punct:]]", "")
  
  #### Make HTML files
  for(id in grid_param_df$id){
    
    points_to_query_i <- points_to_query[points_to_query$id %in% id,]
    
    make_html(location = c(points_to_query_i$latitude, points_to_query_i$longitude),
              height = points_to_query_i$height,
              width = points_to_query_i$height,
              zoom = points_to_query_i$zoom,
              out_dir =out_dir,
              filename_prefix = paste0(id,"_",filename_prefix,"_"),
              map_key = map_key,
              time = time)
    
  }
  
  return("Done!")
}

htmls_to_raster <- function(html_files,
                            grid_param_df,
                            webshot_delay,
                            save_png = F,
                            print_progress = T){
  
  r_list <- lapply(html_files, function(file_i){
    if(print_progress){
      print(paste0("Processing: ", file_i))
    }
    
    id <- file_i %>% 
      str_replace_all(".*/", "") %>% 
      str_replace_all("_.*", "") %>% 
      as.numeric()
    
    param_i <- grid_param_df[grid_param_df$id %in% id,]
    
    html_to_raster(file_i,
                   latitude = param_i$latitude,
                   longitude = param_i$longitude,
                   height = param_i$height,
                   width = param_i$width,
                   zoom = param_i$zoom,
                   webshot_delay = webshot_delay,
                   save_png = save_png)
  })
  
  # TODO: Param...
  r_all <- mosaic(r_list[[1]],
                  r_list[[2]],
                  r_list[[3]],
                  r_list[[4]],
                  r_list[[5]],
                  r_list[[6]],
                  fun = max,
                  tolerance = 1)
  
  return(r_all)
  
}

## Multiple rasters, one time period
make_htmls_grid(grid_param_df = grid_param_df,
                filename_prefix = "nbo_gtt",
                out_dir = "~/Desktop/gtt/html",
                map_key = map_key)

html_files <- list.files("~/Desktop/gtt/html", pattern = ".html$", full.names = T)

r <- htmls_to_raster(html_files = html_files,
                     grid_param_df = grid_param_df,
                     webshot_delay = 22)

## Multiple rasters, multiple time periods



#### Make rasters


writeRaster(r_all, "~/Desktop/test.tiff",overwrite=TRUE)
saveRDS(r_all, "~/Desktop/test.Rds")

r_test <- mosaic(r_list[[1]],
                 #r_list[[2]],
                 #r_list[[3]],
                 r_list[[4]],
                 #r_list[[5]],
                 #r_list[[6]],
                 fun = max,
                 tolerance = 1)


#r_test <- r_list[[2]]
library(leaflet)
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r_test),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(r_test, colors = pal, opacity = 0.7) %>%
  addLegend(pal = pal, values = values(r_test),
            title = "Traffic")

