
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com)
#




server <- function(input,output, session){
  
  options(shiny.maxRequestSize=200*1024^2)    # to increase the size of the input

  ############ for mean LEB for female ################
  output$dmean_risk <- renderLeaflet({
    df <- predsamples
    df2 <- predcoord
    m.pred.y.x <- apply(df[,1788:3574], 2, mean)
    dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(m.pred.y.x))
    r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
    sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
    ras_df_female <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'))
    pal1 <- colorNumeric(c("red", "blue"), domain = na.omit(raster::values(ras_df_female)), na.color = "transparent")
    m1 <- leaflet() %>% 
      addTiles() %>% 
      # addRasterImage(ras_df_female, colors = "YlOrRd", opacity = 0.8) %>%
      addRasterImage(ras_df_female, colors = pal1, opacity = 1, layerId = "values") %>%
      mapview::addMouseCoordinates() %>%
      mapview::addImageQuery(ras_df_female, type="mousemove",  layerId = "values", digits = 2) %>%
      leaflet::addLegend(pal = pal1, values = na.omit(raster::values(ras_df_female)), title = "LEB", opacity = 1)
    m1
  })
  
  ################ mean LEB for male
  output$cmean_risk <- renderLeaflet({
    df <- predsamples
    df2 <- predcoord
    m.pred.y.x2 <- apply(df[,1:1787], 2, mean) 
    dat2 <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(m.pred.y.x2))
    r2  <- raster::mask(raster::rasterFromXYZ(dat2), bound)
    sp::proj4string(r2) = sp::CRS("+init=epsg:27700")
    ras_df_male <- raster::projectRaster(r2, crs=sp::CRS('+init=epsg:4326'))
    pal2 <- colorNumeric(c("red", "blue"), domain = na.omit(raster::values(ras_df_male)), na.color = "transparent")
    m2 <- leaflet() %>% 
        addTiles() %>% 
        addRasterImage(ras_df_male, colors = pal2, opacity = 1, layerId = "values") %>%
        mapview::addMouseCoordinates() %>%
        # mapview::addImageQuery(ras_df_male, type="mousemove",  layerId = "values") %>%
        leaflet::addLegend(pal = pal2, values = na.omit(raster::values(ras_df_male)), title = "LEB", opacity = 1)
      m2
  })
  ########## exceedance #########################
  output$dexceed <- renderLeaflet({
    #function to compute exceedance probability
    #function to compute exceedance probability
    df <- predsamples
    df2 <- predcoord
    excd <- sapply(input$thresholds, function(x) apply(df[,1788:3574], 2, function(z) mean(z < x)))
    dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(excd))
    r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
    sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
    df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'),  method="ngb")
    pal3 <- colorBin(c("blue", "red"), domain = na.omit(raster::values(df)), bins =  seq(0, 1, length.out = 6), na.color = "transparent")
    m3 <- leaflet() %>% 
      addTiles() %>% 
      addRasterImage(df, colors = pal3, opacity = 1, layerId = "values", method="ngb") %>%
      mapview::addMouseCoordinates() %>%
      # mapview::addImageQuery(df, type="mousemove", prefix = "Probability", layerId = "values") %>%
      leaflet::addLegend(pal = pal3, values = na.omit(raster::values(df)), title = "probability", opacity = 1)
    m3
  })
  
  output$cexceed <- renderLeaflet({
    #function to compute exceedance probability
    df <- predsamples
    df2 <- predcoord
      excd <- sapply(input$thresholds, function(x) apply(df[, 1:1787], 2, function(z) mean(z < x)))
      dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(excd))
      r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
      sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
      df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'),  method="ngb")
      pal4 <- colorBin(c("blue", "red"), domain = na.omit(raster::values(df)), bins =  seq(0, 1, length.out = 6), na.color = "transparent")
      m4 <- leaflet() %>% 
        addTiles() %>% 
        addRasterImage(df, colors = pal4, opacity = 1, layerId = "values", method="ngb") %>%
        mapview::addMouseCoordinates() %>%
        # mapview::addImageQuery(df, type="mousemove", prefix = "Probability", layerId = "values") %>%
        leaflet::addLegend(pal = pal4, values = na.omit(raster::values(df)), title = "probability", opacity = 1)
      m4
  })
  
  output$dquantile <- renderLeaflet({
    df <- predsamples
    df2 <- predcoord
    quant <- sapply(input$quantile, function(x) apply(df[,1788:3574], 2, function(z) quantile(z, x)))
    dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(quant))
    r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
    sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
    df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'))
    pal5 <- colorNumeric(c("red", "blue"), domain = na.omit(raster::values(df)), na.color = "transparent")
    m5 <- leaflet() %>% 
      addTiles() %>% 
      addRasterImage(df, colors = pal5, opacity = 1) %>%
      mapview::addMouseCoordinates() %>%
      # mapview::addImageQuery(df, type="mousemove", layerId = "values") %>%
      leaflet::addLegend(pal = pal5, values = na.omit(raster::values(df)), title = "LEB", opacity = 1)
    m5
  })
  
  output$cquantile <- renderLeaflet({
    df <- predsamples
    df2 <- predcoord
    quant <- sapply(input$quantile, function(x) apply(df[, 1:1787], 2, function(z) quantile(z, x)))
    dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(quant))
    r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
    sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
    df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'))
    pal6 <- colorNumeric(c("red", "blue"), domain = na.omit(raster::values(df)), na.color = "transparent")
    m6 <- leaflet() %>% 
      addTiles() %>% 
      addRasterImage(df, colors = pal6, opacity = 1) %>%
      mapview::addMouseCoordinates() %>%
      # mapview::addImageQuery(df, type="mousemove", layerId = "values") %>%
      leaflet::addLegend(pal = pal6, values = na.omit(raster::values(df)), title = "LEB", opacity = 1)
    m6
  })
}