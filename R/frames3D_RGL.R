#' Animate movement on 3D Map
#'
#' This function returns every supported map type that can be used as input to the \code{map_type} argument of \code{\link{frames_spatial}}.
#'
#' @inheritParams m
#' @param out_file character, the output file path, e.g. "/dir/to/". 
#' @param map_service blindtext
#' @param map_type blindtext
#' @param map_token blindtext
#' @param m.crs blindtext
#' @param theta blindtext
#' @param phi blindtext
#' @param sunangle blindtext
#' @param zscale_terrain blindtext
#' @param zscale_movement blindtext
#' @param zoom blindtext 
#' @param out_ext blindtext
#' @param display blindtext
#' @param ... additional arguments to be passed to the render function.
#' 
#' @return A character vector of supported map types
#' 
#' @importFrom RStoolbox rescaleImage
#' @importFrom sp spTransform
#' @importFrom rayshader ray_shade sphere_shade add_overlay add_shadow render_snapshot plot_3d
#' @importFrom rgl axes3d points3d  rgl.close
#' @importFrom av av_encode_video
#' @importFrom gifski gifski
#' @importFrom utils browseURL
#' 
#' @examples 
#' 
#' 
#' @seealso \code{\link{frames_spatial}}
#' @export

frames_3D_RGL <- function(m, out_file, map_service = "mapbox", map_type = "satellite",
                           map_token=map_token, m.crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", 
                           pointsize=5, sunangle = 45, zscale_terrain = 3, zscale_movement=3,...){ 
  
  #create dir
  map_dir <- paste0(out_file, "basemap")
  dir.create(map_dir, showWarnings = FALSE,recursive = TRUE)
  
  ## transform data in CRS ETRS89
  
  m <- sp::spTransform(m, CRSobj = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  ## transform move data in dataframe including time and colour and extract extent
  out("Create Dataframe", type = 1)
  m.df <-
    .m2df(m, path_colours = rainbow(15))
  .stats(n.frames = max(m.df$frame))
  
  ## get terrain and basemap
  out("Download Terrain Map", type = 1)
  r.rgb.terrain <- .getMap(map_service = "mapbox", map_type = "terrain", map_dir = map_dir , gg.ext = st_bbox(m), map_res=1,
                           map_token = map_token, m.crs=m.crs)
  out("Download Basemap", type = 1)
   r.overlay <- .getMap(map_service = map_service, map_type = map_type, map_dir = map_dir , gg.ext = st_bbox(m), map_res=1,
                       map_token = map_token, m.crs=m.crs)
  
  r.overlay <- rescaleImage(r.overlay[[1]],  xmin = 0, xmax = 255, ymin = 0, ymax = 1)
  
  ## calculate elevation as matrix
  
  m.elev <- t(getValues(r.rgb.terrain[[1]], format = "matrix"))
  
  ## create basemap
  
  scene.texture<- m.elev  %>%
    sphere_shade(texture = "imhof4") %>%
    add_overlay(as.array(r.overlay), alphalayer = 0.99) %>%
    add_shadow(ray_shade( m.elev,sunangle = sunangle, maxsearch = 100), max_darken = 0.5) 
  
  ## transform coordinates
  
  r.elev <-  r.rgb.terrain[[1]]
  e <- extent(r.elev)
  
  m.df$lon <- pointDistance(c(e@xmin, e@ymin),
                                    cbind(m.df$x, rep(e@ymin, length(m.df$x))), lonlat = FALSE) /
    res(r.elev)[1] - (e@xmax - e@xmin) / 2 / res(r.elev)[1]
  
  m.df$lat <- pointDistance(c(e@xmin, e@ymin),
                                    cbind(rep(e@xmin, length(m.df$y)), m.df$y), lonlat = FALSE) /
    res(r.elev)[2] - (e@ymax - e@ymin) / 2 / res(r.elev)[2]
  
  m.df$altitude <- extract(r.elev, m.df[, 1:2])
  
  ##calculte number of individuals
  categories <- as.character(unique(m.df$colour))
  nr.Categories <- length(categories)
  
  cat.length <- c()
  
  cat.length <- lapply(1:nr.Categories, function(i){
    length <- c(length(which(m.df$colour == categories[i])))
    cat.length <- c(cat.length,length)
  })
  
  categories.df <- as.data.frame(cbind(as.character(categories),as.numeric(cat.length)))
  categories.df <- categories.df[order(categories.df$V2, decreasing = TRUE),]
  categories.df$V2 <- as.numeric(as.character(categories.df$V2))
  
  ## number of frame = Number of points
  n_frames <- as.numeric(categories.df[1,2])
  
  ## progress bar
  pb <- txtProgressBar(min = 1, max = n_frames, style=3)
  
  ##create empty list
  frames_rgl = list()
  rgl::clear3d()
  
  ##plot background basemap
  plot_3d(
    scene.texture,
    m.elev,
    zscale = zscale_terrain)
  
  ##create frames
  out("Create Frames", type = 1)
  frames_rgl <- lapply(1:140, function(i){
    
      setTxtProgressBar(pb, i)

            #print(paste("Processing index:", i)) # helpful to see how slow/fast
             m.df.temp <- m.df[which(m.df$frame==i),]
             categories <- as.character(unique(m.df.temp$colour))
             nr.Categories <- length(categories)
             
                      points3d(
                        m.df.temp[,9],
                        m.df.temp[,11] / zscale_movement,  
                        -m.df.temp[,10],
                        size = pointsize, col = m.df.temp[,8])
      
             frames_rgl[[i]] <- scene3d()
  })
  
  rgl.close()
  return(frames_rgl)
  
  unlink(map_dir, recursive = TRUE, force = TRUE) 
}
