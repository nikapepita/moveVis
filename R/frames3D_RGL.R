#' Create frames of spatial movement maps for animation
#'
#' \code{frames_rgl} creates a list of rgl scenes displaying movement. Each object represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using \code{\link{animate_frames_rgl}}.
#'
#' @inheritParams m
#' @param out_file character, the output file path, e.g. "/dir/to/".
#' @param color color palette for individuals, default: rainbow (15)
#' @param own_terrain TRUE if you want to use your own Terrain Model, default: FALSE
#' @param path_terrain character, file path for the own terrain model.
#' @param map_service choose a mapservice e.g. Mapbox - more informations basemaps::get_maptypes
#' @param map_type choose a maptype e.g. satellite  - more informations basemaps::get_maptypes
#' @param map_token add a map token if it is necassary for the mapservice e.g. mapbox
#' @param m.crs choose crs, default 
#' @param pointsize size of each point, default 10
#' @param sunangle sunangle for 3D Background Image, default 45
#' @param zscale_terrain The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units of 1 meter and the grid values are separated by 10 meters, 'zscale' would be 10. Adjust the zscale down to exaggerate elevation features.Default '10'. 
#' @param zscale_movement define the height of the points, zscale_terrain=zscale_movement - points have same height as basemap
#' @param point TRUE: only points are plotted, FALSE: segments are plotted, Default TRUE 
#' @param ... additional arguments to be passed to the render function.
#' 
#' @return A rgl scene object, which can be plotted with animate_frames_rgl
#' 
#' @importFrom RStoolbox rescaleImage
#' @importFrom sf st_as_sfc st_bbox
#' @importFrom sp spTransform
#' @importFrom rayshader raster_to_matrix ray_shade sphere_shade add_overlay add_shadow plot_3d
#' @importFrom rgl points3d rgl.close segments3d legend3d scene3d clear3d text3d
#' @importFrom utils setTxtProgressBar
#' @importFrom raster pointDistance res
#' @importFrom dplyr count filter
#' @import magrittr
#' @examples 
#' 
#' @seealso \code{\link{frames_spatial}}
#' @export

frames_rgl <- function(m, out_file, color=rainbow(15), own_terrain= FALSE, path_terrain, map_service = "mapbox", map_type = "satellite",
                          map_token=map_token, m.crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", 
                          pointsize=5, sunangle = 45, zscale_terrain = 3, zscale_movement=3, point=TRUE,...){ 
  ##check input
  if(all(!c(inherits(m, "MoveStack"), inherits(m, "Move")))) out("Argument 'm' must be of class 'Move' or 'MoveStack'.", type = 3)
  if(inherits(m, "Move")) m <- moveStack(m)
  
  ##create dir
  map.dir <- paste0(out_file, "basemap/")
  dir.create(map.dir, showWarnings = FALSE,recursive = TRUE)
  
  ## transform data in CRS ETRS89
  m <- sp::spTransform(m, CRSobj = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  ## transform move data in dataframe including time and colour and extract extent
  out("Create Dataframe", type = 1)
  m.df <-
    .m2df(m, path_colours = color)
  .stats(n.frames = max(m.df$frame))
  
  ## download overlay map
  out("Download Overlay Map", type = 1)
  r.overlay <- .getMap(map_service = map_service, map_type = map_type, map_dir = map.dir , gg.ext = st_bbox(m), map_res=1,
                       map_token = map_token,m.crs=m.crs)
  
  #r.overlay <- basemaps::basemap(map_service = map_service, map_type = map_type, map_dir = map.dir , ext = st_bbox(m), map_res=1,
  #                               map_token = map_token,m.crs=m.crs)
  
  ## download terrain map
  r.overlay <- RStoolbox::rescaleImage(r.overlay[[1]],  xmin = 0, xmax = 255, ymin = 0, ymax = 1)

  if(own_terrain==FALSE){
  out("Download Terrain Map", type = 1)
    
    
  r.rgb.terrain <- .getMap(map_service = "mapbox", map_type = "terrain", map_dir = map.dir , gg.ext = st_bbox(m), map_res=1,
                                       map_token = map_token, m.crs=m.crs)
  #r.rgb.terrain <- basemaps::basemap(map_service = "mapbox", map_type = "terrain", map_dir = map.dir , ext = st_bbox(m), map_res=1,
  #                         map_token = map_token, m.crs=m.crs)
  
  }else{
    r.rgb.terrain <- raster(path_terrain)
    ext_crop <- projectRaster(r.overlay, crs = crs(r.rgb.terrain))

    r.rgb.terrain <- r.rgb.terrain %>% 
      crop(ext_crop) %>% 
      projectRaster(crs = m.crs) %>% 
      crop(r.overlay)%>%
      resample(r.overlay, method="bilinear")
  }
  
  
  ## calculate elevation as matrix
  m.elev <- raster_to_matrix(r.rgb.terrain[[1]])
 
  ## create 3d basemap
  
  scene.texture<- m.elev  %>%
    sphere_shade(texture = "imhof4") %>%
    add_overlay(raster::as.array(r.overlay), alphalayer = 0.99) %>%
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
  
  m.df$altitude <- raster::extract(r.elev, m.df[, 1:2])
  
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
  clear3d()
  
  ##plot background basemap
 plot_3d(scene.texture, m.elev,zscale = zscale_terrain)
         
 
  if(point==FALSE){
  ##create frames
  out("Create Frames", type = 1)
  frames_rgl <- lapply(1:n_frames, function(i){
    setTxtProgressBar(pb, i)
    
    m.df.temp <- m.df[which(m.df$frame==i | m.df$frame==i+1),]
    m.df.temp <- m.df.temp[order(m.df.temp$colour),]
    
    categories <- as.character(unique(m.df.temp$colour))
    nr.Categories <- length(categories)
    
    nr <- dplyr::count(m.df.temp, vars = colour)
    
    nr_seg <- nr %>% dplyr::filter(nr$n==2)
    nr_point <- nr %>% dplyr::filter(n==1)

    m.df.seg <- m.df.temp[which(m.df.temp$colour %in% nr_seg$vars),]
    m.df.point <- m.df.temp[which(m.df.temp$colour==nr_point$vars),]
    
    if(!(nrow(m.df.point)==0)) 
    {points3d(
      m.df.point[,9],
      (m.df.point[,11] / zscale_movement)+20,  
      -m.df.point[,10],
      size = pointsize, col = m.df.point[,8])}
    
    if(!(nrow(m.df.seg)==0)) 
    {
      m.df.seg <- split(m.df.seg,m.df.seg$colour)
      
      for (i in 1:length(m.df.seg)){
      lines3d(m.df.seg[[i]][,9],
      (m.df.seg[[i]][,11]/zscale_movement)+20,  
      -m.df.seg[[i]][,10],
      lwd=pointsize, col = m.df.seg[[i]][,8])
        }
      }
    
    frames_rgl[[i]] <- scene3d()  
    frames_rgl[[i]]$dataframe <- m.df.temp
    frames_rgl[[i]]
  
    })
  }else{
    ##create frames
    out("Create Frames", type = 1)
    frames_rgl <- lapply(1:n_frames, function(i){
      
      setTxtProgressBar(pb, i)
      
      m.df.temp <- m.df[which(m.df$frame==i),]
      m.df.temp <- m.df.temp[order(m.df.temp$colour),]
      
      categories <- as.character(unique(m.df.temp$colour))
      nr.Categories <- length(categories)
      
      points3d(
        m.df.temp[,9],
        m.df.temp[,11] / zscale_movement,  
        -m.df.temp[,10],
        size = pointsize, col = m.df.temp[,8])
  
      frames_rgl[[i]] <- scene3d()  
      frames_rgl[[i]]$dataframe <- m.df.temp
      frames_rgl[[i]]
      
    })
    }
  
  rgl.close()
  return(frames_rgl)
  
  unlink(map_dir, recursive = TRUE, force = TRUE) 
}
