# Test Function

if(F){
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
  
  google_key <- api_keys_df %>%
    dplyr::filter(Service == "Google Directions API",
                  Account == "ramarty@email.wm.edu") %>%
    pull(Key)
  
  bing_key <- api_keys_df %>%
    dplyr::filter(Service == "Bing Maps",
                  Account == "robmarty3@gmail.com") %>%
    pull(Key)
  
  # One raster, one time period --------------------------------------------------
  ## Option 1
  gt_make_html(location = c(-1.286389, 36.817222),
               height = 5000,
               width = 5000,
               zoom = 15,
               filename = "~/Desktop/nbo.html", 
               google_key = google_key)
  
  r <- gt_html_to_raster(filename = "~/Desktop/gtt/html/nbo.html",
                         webshot_delay = 10)
  
  ## Option 2
  gt_make_html(location = c(-1.286389, 36.817222),
               height = 5000,
               width = 5000,
               zoom = 16,
               filename = "~/Desktop/gtt/html/nbo.html", 
               google_key = google_key,
               save_params = F)
  
  r <- gt_html_to_raster(filename = "~/Desktop/gtt/html/nbo.html",
                         location = c(-1.286389, 36.817222),
                         height = 5000,
                         width = 5000,
                         zoom = 16,
                         webshot_delay = 10)
  
  # Multiple rasters, one time period --------------------------------------------
  ## Make grid dataframe
  nbo <- getData('GADM', country ='KEN', level=1, download = F)
  nbo <- nbo[nbo$NAME_1 %in% "Nairobi",]
  
  grid_param_df <- gt_make_point_grid(polygon = nbo,
                                      height = 6000,
                                      width = 6000,
                                      zoom = 16,
                                      reduce_hw = 100)
  
  ## Option 1
  make_htmls_from_grid(grid_param_df = grid_param_df,
                       filename_suffix = "nbo_gtt",
                       out_dir = "~/Desktop/gtt/html",
                       google_key = google_key)
  
  html_files <- list.files("~/Desktop/gtt/html", pattern = ".html$", full.names = T)
  
  r <- gt_htmls_to_raster(html_files = html_files,
                          webshot_delay = 22)
  
  ## Option 2
  make_htmls_from_grid(grid_param_df = grid_param_df,
                       filename_suffix = "nbo_gtt",
                       out_dir = "~/Desktop/gtt/html",
                       google_key = google_key,
                       save_params = F)
  
  html_files <- list.files("~/Desktop/gtt/html", pattern = ".html$", full.names = T)
  
  r <- gt_htmls_to_raster(html_files = html_files,
                          grid_param_df = grid_param_df,
                          webshot_delay = 22)
  
  # Multiple rasters, multiple time periods --------------------------------------
  ## Make grid dataframe
  nbo <- getData('GADM', country ='KEN', level=1)
  nbo <- nbo[nbo$NAME_1 %in% "Nairobi",]
  
  grid_param_df <- gt_make_point_grid(polygon = nbo,
                                      height = 6000,
                                      width = 6000,
                                      zoom = 16,
                                      reduce_hw = 100)
  
  ## Make HTMLs
  library(lubridate)
  for(i in 1:3){
    
    time <- Sys.time() %>% 
      with_tz(tzone = "UTC") %>%
      as.numeric() %>% 
      as.character() %>% 
      str_replace_all("[[:punct:]]", "")
    
    make_htmls_from_grid(grid_param_df = grid_param_df,
                         filename_suffix = paste0("nbo_gt_utc",time),
                         out_dir = "~/Desktop/gtt/html",
                         google_key = google_key,
                         save_params = F)
    
    Sys.sleep(5)
  }
  
  ## Make rasters
  html_files <- "~/Desktop/gtt/html" %>%
    list.files(pattern = ".html$",
               full.names = T) 
  
  time_stamps <- html_files %>% 
    str_replace_all(".html", "") %>%
    str_replace_all(".*_utc", "") %>%
    unique()
  
  for(time_i in time_stamps){
    html_files_time_i <- html_files %>%
      str_subset(time_i)
    
    r <- gt_htmls_to_raster(html_files = html_files_time_i,
                            grid_param_df = grid_param_df,
                            webshot_delay = 22)
    
    saveRDS(r, file.path("~/Desktop", paste0("nbo_gt_utc",time_i,".Rds")))
  }
  
  
  
  
  
  writeRaster(r_all, "~/Desktop/test.tiff",overwrite=TRUE)
  
  
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
  
}