
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com)
#




server <- function(input,output, session){
  
  options(shiny.maxRequestSize=200*1024^2)    # to increase the size of the input

  ############ for mean risk ################
  output$dmean_risk <- renderLeaflet({
    df <- predsamples
    df2 <- predcoord
    m.pred.y.x <- apply(df[,1:1787], 2, mean)
    dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(m.pred.y.x))
    r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
    sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
    df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'))
    pal <- colorBin(colorRampPalette(c("blue", "red"))(1000), domain = na.omit(raster::values(df)), bins = 7)
    m1 <- leaflet() %>% 
      addTiles() %>% 
      # addRasterImage(df, colors = "YlOrRd", opacity = 0.8) %>%
      addRasterImage(df, colors = colorRampPalette(c("blue", "red"))(1000), opacity = 0.8, layerId = "values") %>%
      # mapview::addMouseCoordinates() %>%
      # mapview::addImageQuery(df, type="mousemove", prefix = "Probability",  layerId = "values") %>%
      leaflet::addLegend(pal = pal, values = na.omit(raster::values(df)), title = "LEB")
    m1
  })
  
  ################
  output$cmean_risk <- renderLeaflet({
    df <- predsamples
    df2 <- predcoord
    m.pred.y.x <- apply(df[,1788:3574], 2, mean)
    dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(m.pred.y.x))
    r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
    sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
    df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'))
    pal <- colorBin(colorRampPalette(c("blue", "red"))(1000), domain = na.omit(raster::values(df)), bins = 7)
    m2 <- leaflet() %>% 
        addTiles() %>% 
        addRasterImage(df, colors = colorRampPalette(c("blue", "red"))(1000), opacity = 0.8, layerId = "values") %>%
        # mapview::addMouseCoordinates() %>%
        # mapview::addImageQuery(df, type="mousemove", prefix = "Probability",  layerId = "values") %>%
        leaflet::addLegend(pal = pal, values = na.omit(raster::values(df)), title = "LEB")
      m2
  })
  ########## exceedance #########################
  output$dexceed <- renderLeaflet({
    #function to compute exceedance probability
    #function to compute exceedance probability
    df <- predsamples
    df2 <- predcoord
    excd <- sapply(input$thresholds, function(x) apply(df[, 1:1787], 2, function(z) mean(z < x)))
    dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(excd))
    r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
    sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
    df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'))
    pal <- colorBin(colorRampPalette(c("blue", "red"))(1000), domain = na.omit(raster::values(df)), bins =  seq(0, 1, length.out = 6))
    m3 <- leaflet() %>% 
      addTiles() %>% 
      addRasterImage(df, colors = colorRampPalette(c("blue", "red"))(1000), opacity = 0.8, layerId = "values") %>%
      # mapview::addMouseCoordinates() %>%
      # mapview::addImageQuery(df, type="mousemove", layerId = "values") %>%
      leaflet::addLegend(pal = pal, values = na.omit(raster::values(df)), title = "probability")
    m3
  })
  
  output$cexceed <- renderLeaflet({
    #function to compute exceedance probability
    df <- predsamples
    df2 <- predcoord
      excd <- sapply(input$thresholds, function(x) apply(df[,1788:3574], 2, function(z) mean(z < x)))
      dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(excd))
      r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
      sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
      df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'))
      pal <- colorBin(colorRampPalette(c("blue", "red"))(1000), domain = na.omit(raster::values(df)), bins =  seq(0, 1, length.out = 6))
      m4 <- leaflet() %>% 
        addTiles() %>% 
        addRasterImage(df, colors = colorRampPalette(c("blue", "red"))(1000), opacity = 0.8) %>%
        # mapview::addMouseCoordinates() %>%
        # mapview::addImageQuery(df, type="mousemove", layerId = "values") %>%
        leaflet::addLegend(pal = pal, values = na.omit(raster::values(df)), title = "probability")
      m4
  })
  
  output$dquantile <- renderLeaflet({
    df <- predsamples
    df2 <- predcoord
    quant <- sapply(input$quantile, function(x) apply(df[, 1:1787], 2, function(z) quantile(z, x)))
    dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(quant))
    r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
    sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
    df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'))
    pal <- colorNumeric(colorRampPalette(c("blue", "red"))(1000), domain = na.omit(raster::values(df)))
    m5 <- leaflet() %>% 
      addTiles() %>% 
      addRasterImage(df, colors = colorRampPalette(c("blue", "red"))(1000), opacity = 0.8) %>%
      # mapview::addMouseCoordinates() %>%
      # mapview::addImageQuery(df, type="mousemove", layerId = "values") %>%
      leaflet::addLegend(pal = pal, values = na.omit(raster::values(df)), title = "LEB")
    m5
  })
  
  output$cquantile <- renderLeaflet({
    df <- predsamples
    df2 <- predcoord
    quant <- sapply(input$quantile, function(x) apply(df[,1788:3574], 2, function(z) quantile(z, x)))
    dat <- data.frame(x=df2[,1], y=df2[,2], z = as.numeric(quant))
    r1  <- raster::mask(raster::rasterFromXYZ(dat), bound)
    sp::proj4string(r1) = sp::CRS("+init=epsg:27700")
    df <- raster::projectRaster(r1, crs=sp::CRS('+init=epsg:4326'))
    pal <- colorNumeric(colorRampPalette(c("blue", "red"))(1000), domain = na.omit(raster::values(df)))
    m6 <- leaflet() %>% 
      addTiles() %>% 
      addRasterImage(df, colors = colorRampPalette(c("blue", "red"))(1000), opacity = 0.8) %>%
      # mapview::addMouseCoordinates() %>%
      # mapview::addImageQuery(df, type="mousemove", layerId = "values") %>%
      leaflet::addLegend(pal = pal, values = na.omit(raster::values(df)), title = "LEB")
    m6
  })
}