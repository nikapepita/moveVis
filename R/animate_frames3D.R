#' \code{animate_frames3D} creates an animation from a list of frames computed with \code{\link{frames_spatial}}.
#'
#' View movements on 3D Map 
#'
#' \code{animate_frames3D} is a simple wrapper that displays movement tracks on an 3D map.
#'
#' @inheritParams add_gg
#' @param out_file  
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
#' 
#' @seealso \code{\link{frames_spatial}}
#' 
#' @export

animate_frames3D <- function(frames, out_file, scale = 180, multicore = TRUE, width = 8, height = 6,windowsize = c(1500, 1125), render.camera, overwrite=TRUE, out_ext = "gif", display = TRUE..){

#check installed packages
if(length(grep("rayshader", rownames(utils::installed.packages()))) == 0) out("'rayshader' has to be installed to use this function. Use install.packages('rayshader').", type = 3)

#check input arguments  
if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See frames_spatial()).", type = 3)
if((frames[[1]]$scales$scales[[1]]$aesthetics)=="fill") out("Argument 'frames' needs as basemap a DEM or any kind of raster. See frames_spatial()).", type = 3)
if(!is.null(render.camera)) if(length(frames)!=nrow(render.camera)) out("Argument 'render.camera'must be as long as Argument 'frames'.Argument 'render.camera' is set to default value NULL", type = 2)
invisible(if(!is.null(render.camera))(if((length(frames)!=nrow(render.camera))) render.camera <- NULL))

 
#adjust data for plot_gg function
                             
 colortheme = c("line","rect","text","axis.title", "axis.title.x",
                "axis.title.x.top","axis.title.y","axis.title.y.right","axis.text",
                "axis.text.x" ,"axis.text.x.top","axis.text.y","axis.text.y.right",
                "axis.ticks" ,"axis.ticks.length","axis.line"  ,"axis.line.x",
               "axis.line.y","legend.background","legend.margin","legend.spacing",
               "legend.spacing.x","legend.spacing.y","legend.key" ,"legend.key.size",
               "legend.key.height","legend.key.width","legend.text","legend.text.align",
               "legend.title","legend.title.align","legend.position","legend.direction",
               "legend.justification" ,"legend.box","legend.box.margin","legend.box.background",
               "legend.box.spacing","panel.background","panel.border","panel.spacing",
               "panel.spacing.x","panel.spacing.y","panel.grid" ,"panel.grid.minor",
               "panel.ontop","plot.background","plot.title" ,"plot.subtitle",
               "plot.caption","plot.tag","plot.tag.position","plot.margin",
               "strip.background","strip.placement","strip.text" ,"strip.text.x",
               "strip.text.y","strip.switch.pad.grid","strip.switch.pad.wrap","panel.grid.major",
               "title","axis.ticks.length.x","axis.ticks.length.y","axis.ticks.length.x.top",
               "axis.ticks.length.x.bottom","axis.ticks.length.y.left","axis.ticks.length.y.right","axis.title.x.bottom",
               "axis.text.x.bottom","axis.text.y.left","axis.title.y.left")
 
 #create dir
 frames_dir <- paste0(out_file, "frames/")
 dir.create(frames_dir, recursive = T)
 
  #transfer frames in 3D based on the basemap and plot frames in 3D
  
  plot_3D <- function(i){
  gg <- frames[[i]]
  
  tempname = names(gg$theme)
  theme.dif <- setdiff(tempname,colortheme)
  dif <- which(tempname %in% theme.dif)
  gg$theme <- gg$theme[-c(dif)]
  
  gg$layers <- gg$layers[-length(gg$layers)]
  rayshader::plot_gg(gg, scale = scale, multicore = multicore, width = width, height = height, windowsize = windowsize)
  
  if(is.null(render.camera)) {rayshader::render_camera(theta = 46-(i*0.1), phi = 60)
  } else {rayshader::render_camera(theta = render.camera[i,1], phi = render.camera[i,2])}
  
  rayshader::render_snapshot(paste0(frames_dir, "frame_", i, ".png"), clear = T)
  rgl::rgl.close()
  }
  
  rgl::clear3d()
  out("Creating 3D Plots...", type=1)
  lapply(seq(frames), plot_3D)


  
  #create gif oder mp4 Animation
  frames_files <- list.files(frames_dir, full.names = TRUE)
  
  tryCatch({
    # animate PNGs
  if(out_ext == "gif"){
    if(length(frames) > 800) out("The number of frames exceeds 800 and the GIF format is used. This format may not be suitable for animations with a high number of frames, since it causes large file sizes. Consider using a video file format instead.", type = 2)
    gifski(frames_files, gif_file=paste0(out_file, "Animate_3D", ".gif"), width = 800, height = 600,
           delay = 1,progress=TRUE)
  }else{
    av::av_encode_video(frames_files, output = paste0(out_file, "Animate_3D", ".mp4"), framerate = 24, vfilter = "pad=ceil(iw/2)*2:ceil(ih/2)*2")
  }
  }, error = function(e){
  unlink(frames_dir, recursive = TRUE)
  out(paste0("Error creating animation: ", as.character(e)), type = 3)
  }, finally = unlink(frames_dir, recursive = TRUE))
  
}
