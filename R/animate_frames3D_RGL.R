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
#' @param ... additional arguments to be passed to the render function.
#' 
#' @return A character vector of supported map types
#' 
#' @importFrom RStoolbox rescaleImage
#' @importFrom rayshader ambient_shade
#' @importFrom rayshader ambient_shade
#' @importFrom av av_encode_video
#' @importFrom av av_encode_video
#' @importFrom gifski gifski
#' @importFrom ggplot2 quo
#' @importFrom lubridate dseconds
#' 
#' @examples 
#' 
#' 
#' @seealso \code{\link{frames_spatial}}
#' @export

animate_3D_RGL <- function(m, out_file, map_service = "mapbox", map_type = "satellite",
                           map_token=map_token, m.crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", 
                           theta=theta, phi=phi, sunangle = 45, zscale_terrain = 3, zscale_movement=3, zoom = 0.80, out_ext = "gif",...){ 
  
  #create dir
  frames_dir <- paste0(out_file, "framesRGL/")
  dir.create(frames_dir, recursive = T)
  
  map_dir <- paste0(out_file, "framesRGL/basemap/")
  dir.create(map_dir, recursive = T)
  
  ## transform data in cRS ETRS89
  
  m <- sp::spTransform(m,
                       CRSobj = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  ## transform move data in dataframe and extract extent
  
  m.df <-
    .m2df(m, path_colours = rainbow(15)) # create data.frame from m with frame time and colour
  .stats(n.frames = max(m.df$frame))
  
  gg.ext <- as.vector(.ext(m.df, m.crs = st_crs(m)))
  
  r.rgb.terrain <- .getMap(map_service = "mapbox", map_type = "terrain", map_dir = map_dir , gg.ext = st_bbox(m), map_res=1,
                           map_token = map_token,
                           m.crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  r.overlay <- .getMap(map_service = map_service, map_type = map_type, map_dir = map_dir , gg.ext = st_bbox(m), map_res=1,
                       map_token = map_token,
                       m.crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  r.overlay <- rescaleImage(r.overlay[[1]],  xmin = 0, xmax = 255, ymin = 0, ymax = 1)
  
  ## calculate eleveation as matrix
  
  m.elev <- t(getValues(r.rgb.terrain[[1]], format = "matrix"))
  
  ## calculate shadowing
  
m.shadow <-
    ray_shade(
      m.elev,
      anglebreaks = seq(30, 40, 10),
      sunangle = sunangle,
      maxsearch = 100
    )
  
  ## create RGB overlay array
  
  array.overlay <- as.array(r.rgb.sat)
  
  ## create texture
  
  scene.texture<- m.elev  %>%
    sphere_shade(texture = "imhof4") %>%
    add_overlay(array.overlay, alphalayer = 0.99) %>%
    add_shadow(m.shadow, max_darken = 0.5) 
  
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
  
  
  path_gif <- file.path(out_file)
  
  categories <- as.character(unique(m.df$colour))
  nr.Categories <- length(categories)
  
  cat.length <- c()
  
  for (i in 1:nr.Categories) {
    length <- c(length(which(m.df$colour == categories[i])))
    cat.length <- c(cat.length,length)
  }
  
  categories.df <- as.data.frame(cbind(as.character(categories),as.numeric(cat.length)))
  categories.df <- categories.df[order(categories.df$V2, decreasing = TRUE),]
  categories.df$V2 <- as.numeric(as.character(categories.df$V2))
  
  ## Number of frame = Number of points
  n_frames <- as.numeric(categories.df[1,2])
  
  img_frames <- file.path(
    frames_dir,
    paste0("film_", formatC(seq_len(n_frames), width = 3, flag = "0"), ".png")
  )
  
  
  rgl::clear3d()
  
  
  # create all frames
  for (i in 155:n_frames) {
    print(i)
    plot_3d(
      scene.texture,
      m.elev,
      zscale = zscale_terrain,
      theta = theta, phi = phi,
      zoom = zoom,
    )
    
    rgl::axes3d(color="grey",edges = "bbox", labels = TRUE, tick = TRUE, nticks = 3,box = FALSE, expand = 1.03,main="Movement")
    
    m.df.temp <- m.df[which(m.df$frame==i),]
    categories <- as.character(unique(m.df.temp$colour))
    nr.Categories <- length(categories)
    
    for (j in 1:nr.Categories){
      
      points3d(
        m.df.temp[m.df.temp$colour == categories[j],][,9],
        m.df.temp[m.df.temp$colour == categories[j],][,11] / zscale_movement,  
        -m.df.temp[m.df.temp$colour == categories[j],][,10],
        size = 5, col = categories[j])
    }
    
    render_snapshot(img_frames[i])
    s <- scene3d()
    
  }
  
  rgl::rgl.close()
  
  files <- list.files(frames_dir, pattern="film_", full.names = T)
  
  tryCatch({
    # animate PNGs
    if(out_ext == "gif"){
      if(n_frames > 800) out("The number of number of frames exceeds 800 and the GIF format is used. This format may not be suitable for animations with a high number of frames, since it causes large file sizes. Consider using a video file format instead.", type = 2)
      gifski(files, gif_file=paste0(frames_dir, "Animate_3D", ".gif"), width = 800, height = 600,
             delay = 1,progress=TRUE)
    }else{
      av::av_encode_video(files, output = paste0(frames_dir, "Animate_3D", ".mp4"), framerate = 24, vfilter = "pad=ceil(iw/2)*2:ceil(ih/2)*2")
    }
  }, error = function(e){
    unlink(frames_dir, recursive = TRUE)
    out(paste0("Error creating animation: ", as.character(e)), type = 3)
  }, finally = unlink(frames_dir, recursive = TRUE))
  
}
