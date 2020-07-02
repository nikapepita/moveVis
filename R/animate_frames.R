#' Animate frames
#'
#' \code{animate_frames} creates an animation from a list of frames computed with \code{\link{frames_spatial}}.
#'
#' @inheritParams add_gg
#' @param out_file character, the output file path, e.g. "/dir/to/file.mov". The file extension must correspond to a file format known by the available renderers of the running system. Use \code{\link{suggest_formats}} to get a vector of suggested known file formats.
#' @param fps numeric, the number of frames to be displayed per second. Default is 2.
#' @param width numeric, width of the output animation in pixels.
#' @param height numeric, height of the output animation in pixels.
#' @param res numeric, resolution of the output animation in ppi.
#' @param end_pause numeric, defining how many seconds the last frame of the animation should be hold to add a pause at the the end of the animation. Default is 0 seconds to not add a pause.
#' @param display logical, whether the animation should be displayed after rendering or not.
#' @param overwrite logical, wether to overwrite an existing file, if \code{out_file} is already present.
#' @param ... additional arguments to be passed to the render function.
#'
#' @details An appropriate render function is selected depending on the file extension in \code{out_file}: For \code{.gif} files, \code{gifski::save_gif} is used, for any other (video) format, \code{av::av_capture_graphics} is used.
#'
#' @return None or the default image/video viewer displaying the animation
#' 
#' @importFrom av av_encode_video
#' @importFrom gifski gifski
#' @importFrom ggplot2 quo
#' @importFrom lubridate dseconds
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @examples
#' library(moveVis)
#' library(move)
#' 
#' data("move_data", "basemap_data")
#' # align movement
#' m <- align_move(move_data, res = 4, unit = "mins")
#' 
#' # create spatial frames with frames_spatial:
#' r_list <- basemap_data[[1]]
#' r_times <- basemap_data[[2]]
#' 
#' \dontrun{
#' frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                          fade_raster = TRUE)
#' 
#' # customize
#' frames <- add_colourscale(frames, type = "gradient",
#'                           colours = c("orange", "white", "darkgreen"), legend_title = "NDVI")
#' frames <- add_northarrow(frames, position = "bottomleft")
#' frames <- add_scalebar(frames, colour = "white", position = "bottomright")
#' 
#' frames <- add_progress(frames)
#' frames <- add_timestamps(frames, m, type = "label")
#' 
#' # check available formats
#' suggest_formats()
#' 
#' # animate frames as GIF
#' animate_frames(frames, out_file = tempfile(fileext = ".gif"))
#' 
#' # animate frames as mov
#' animate_frames(frames, out_file = tempfile(fileext = ".gif"))
#' }
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{join_frames}}
#' 
#' @export

animate_frames <- function(frames, out_file, fps = 25, width = 700, height = 700, res = 100, end_pause = 0, display = TRUE, overwrite = FALSE, pointsize=2, point=TRUE, rgl.height=5, mainDir ="c:/Dokumente und Einstellungen/Annika/Desktop/", verbose = TRUE, ...){
  
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See frames_spatial()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  
  if(!is.character(out_file)) out("Argument 'out_file' must be of type 'character'.", type = 3)
  of_split <- strsplit(out_file, "/")[[1]]
  if(length(of_split) > 1) if(isFALSE(dir.exists(paste0(utils::head(of_split, n = -1), collapse = "/")))) out("Target directory of 'out_file' does not exist.", type = 3)
  if(all(file.exists(out_file), !isTRUE(overwrite))) out("Defined output file already exists and overwriting is disabled.", type = 3)
  num.args <- c(fps = fps, width = width, height = height, res = res)
  catch <- sapply(1:length(num.args), function(i) if(!is.numeric(num.args[[i]])) out(paste0("Argument '", names(num.args)[[i]], "' must be of type 'numeric'."), type = 3))
  
  out_ext <- tolower(utils::tail(unlist(strsplit(out_file, "[.]")), n=1))
  out("Rendering animation...")
  if(end_pause > 0){
    n.add <- round(end_pause*fps)
    frames <- append(frames, rep(utils::tail(frames, n = 1), times = n.add))
    out(paste0("Number of frames: ", toString(length(frames)-n.add), " + ", toString(n.add), " to add \u2248 ", toString(dseconds(end_pause)), " of pause at the end"))
  }
  .stats(n.frames = length(frames), fps)
  
  #frames_expr <- expression(moveVis:::.lapply(frames, function(x) quiet(print(x))))
  
  #change to a different folder?!?! to find the pgns easy?
  # create PNGs
  #frames_dir <- paste0(tempdir(), "/moveVis/frames/")
  #dir.create(frames_dir, recursive = T)
  
  
  subDir <- "Output_Frames/"
  frames_dir <- file.path(mainDir, subDir)
  
  dir.create(frames_dir, showWarnings = FALSE)
  setwd(frames_dir)
  
  #if(engine == "ggplot")...
  
  
  if(engine == "rgl"){
    
    ## number of frame = Number of points
    n_frames <- max(frames$move_data$frame)
    
    ## progress bar
    pb <- txtProgressBar(min = 1, max = n_frames, style=3)
    
    clear3d()
    
    ##plot background basemap
    frames$rgl_background()
    frames$rgl_legend()
    
    
    if(point==FALSE){
      ##create frames
      out("Create Frames", type = 1)
      frames_rgl <- lapply(1:n_frames, function(i){
        setTxtProgressBar(pb, i)
        
        m.df.temp <- m.df[which(m.df$frame<=i+1),]
        m.df.temp <- m.df.temp[order(m.df.temp$colour),]
        
        categories <- as.character(unique(m.df.temp$colour))
        nr.Categories <- length(categories)
        
        nr <- dplyr::count(m.df.temp, vars = colour)
        
        nr_seg <- nr %>% dplyr::filter(nr$n>=2)
        nr_point <- nr %>% dplyr::filter(n==1)
        
        m.df.seg <- m.df.temp[which(m.df.temp$colour %in% nr_seg$vars),]
        m.df.point <- m.df.temp[which(m.df.temp$colour==nr_point$vars),]
        
        if(!(nrow(m.df.point)==0)) 
        {points3d(
          m.df.point[,9],
          (m.df.point[,11] / frames$rgl_zscale)+rgl.height,  
          -m.df.point[,10],
          size = pointsize, col = m.df.point[,8])}
        
        if(!(nrow(m.df.seg)==0)) 
        {
          
          if(length(unique(m.df.seg$colour))>1){
            
            m.df.seg <- split(m.df.seg,m.df.seg$colour)
            
            for (j in 1:length(m.df.seg)){
              lines3d(m.df.seg[[j]][,9],
                      (m.df.seg[[j]][,11]/ frames$rgl_zscale)+rgl.height,  
                      -m.df.seg[[j]][,10],
                      lwd=pointsize, col = m.df.seg[[j]][,8])
            }
          }else{
            lines3d(m.df.seg[,9],
                    (m.df.seg[,11]/ frames$rgl_zscale)+rgl.height, 
                    -m.df.seg[,10],
                    lwd=pointsize, col = m.df.seg[,8])
          }
        }
        
      name <- if(i<10){paste0("frame_00",i,".png")
      }else if(i>=10 & i<100){paste0("frame_0",i,".png")
          }else{paste0("frame_",i,".png")}
        
        rgl::rgl.snapshot(name, fmt = "png", top = TRUE )
        
        
        rgl_id <- rgl.ids()
        rgl_id <- rgl_id[rgl_id$type=="linestrip" | rgl_id$type=="point",]
        rgl::rgl.pop(type = "shapes",id = rgl_id$id)
        gc()
        
      })
    }else{
      ##create frames
      out("Create Frames", type = 1)
      frames_rgl <- lapply(1:n_frames, function(i){
        
        setTxtProgressBar(pb, i)
        
        m.df.temp <- m.df[which(m.df$frame<=i),]
        m.df.temp <- m.df.temp[order(m.df.temp$colour),]
        
        categories <- as.character(unique(m.df.temp$colour))
        nr.Categories <- length(categories)
        
        points3d(
          m.df.temp[,9],
          (m.df.temp[,11] /frames$rgl_zscale)+rgl.height,
          -m.df.temp[,10],
          size = pointsize, col = m.df.temp[,8])
        
        name <- if(i<10){paste0("frame_00",i,".png")
        }else if(i>=10 & i<100){paste0("frame_0",i,".png")
        }else{paste0("frame_",i,".png")}
        

        rgl::rgl.snapshot(name, fmt = "png", top = TRUE )
        
        rgl::rgl.pop(type = "shapes")
        gc()
        
      })
    }
    rgl::rgl.close()
  }
  
  tryCatch({
    file <- file.path(frames_dir, "frame_%05d.png")
    frames_files <- list.files(frames_dir, full.names = TRUE)
    
      # animate PNGs
      if(out_ext == "gif"){
        if(length(frames) > 800) out("The number of number of frames exceeds 800 and the GIF format is used. This format may not be suitable for animations with a high number of frames, since it causes large file sizes. Consider using a video file format instead.", type = 2)
        gifski(frames_files, gif_file=paste0("Animate_3D", ".gif"), width = 800, height = 600,
               delay = 1,progress=TRUE)
      }else{
        av::av_encode_video(files, output = paste0(frames_dir, "Animate_3D", ".mp4"), framerate = 24, vfilter = "pad=ceil(iw/2)*2:ceil(ih/2)*2")
      }
    }, error = function(e){
      unlink(frames_dir, recursive = TRUE)
      out(paste0("Error creating animation: ", as.character(e)), type = 3)
    }, finally = unlink(frames_dir, recursive = TRUE))
    
  if(isTRUE(display)) utils::browseURL(out_file)
  
}