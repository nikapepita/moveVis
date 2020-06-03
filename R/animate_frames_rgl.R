#' Animate frames 3D RGL
#'
#' \code{animate_frames_rgl} creates an animation from a list of frames computed with \code{\link{frames_rgl}}.
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
#' @importFrom rgl plot3d rgl.close
#' @importFrom rayshader render_snapshot
#' @importFrom utils txtProgressBar
#'  
#' @author Jakob Schwalb-Willmann
#' 
#' @examples
#' 
#' @seealso \code{\link{frames_rgl}} 
#' 
#' @export

animate_frames_rgl <- function(frames, out_file, fps = 25, width = 700, height = 700, res = 100, end_pause = 0, display = TRUE, 
                            overwrite = FALSE, verbose = TRUE, out_ext = "gif", ...){
  #check installed packages
  if(length(grep("rgl", rownames(utils::installed.packages()))) == 0) out("'rgl' has to be installed to use this function. Use install.packages('rgl').", type = 3)
  
  #if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of rgl scenes. See frames_spatial()).", type = 3)
  #if(!all(sapply(frames, function(x) inherits(x, "rglscene")))) out("At least one element of argument 'frames' is not a rgl scene.", type = 3)
  
  if(!is.character(out_file)) out("Argument 'out_file' must be of type 'character'.", type = 3)
  of_split <- strsplit(out_file, "/")[[1]]
  if(length(of_split) > 1) if(isFALSE(dir.exists(paste0(utils::head(of_split, n = -1), collapse = "/")))) out("Target directory of 'out_file' does not exist.", type = 3)
  if(all(file.exists(out_file), !isTRUE(overwrite))) out("Defined output file already exists and overwriting is disabled.", type = 3)
  num.args <- c(fps = fps, width = width, height = height, res = res)
  catch <- sapply(1:length(num.args), function(i) if(!is.numeric(num.args[[i]])) out(paste0("Argument '", names(num.args)[[i]], "' must be of type 'numeric'."), type = 3))
  
  #out_ext <- tolower(utils::tail(unlist(strsplit(out_file, "[.]")), n=1))
  out("Rendering animation...")
  if(end_pause > 0){
    n.add <- round(end_pause*fps)
    frames <- append(frames, rep(utils::tail(frames, n = 1), times = n.add))
    out(paste0("Number of frames: ", toString(length(frames)-n.add), " + ", toString(n.add), " to add \u2248 ", toString(dseconds(end_pause)), " of pause at the end"))
  }
  .stats(n.frames = length(frames), fps)
  
  ## progress bar
  pb <- txtProgressBar(min = 1, max = length(frames), style=3)
  
  ## create PNGs
  frames_dir <- out_file
  dir.create(frames_dir, recursive = T)
  img_frames <- file.path(
    frames_dir,
    paste0("mov_", formatC(seq_len(length(frames)), width = 3, flag = "0"), ".png")
  )
  
  ## plot frames
  frames3d <- lapply(1:length(frames_rgl), function(i){
    setTxtProgressBar(pb, i)
    plot3d(frames[[i]],zoom = zoomvalues[1],)
    render_snapshot(img_frames[i])
    
    rgl.close()
  })
  
  files <- list.files(frames_dir, pattern="mov_", full.names = T)
  
  tryCatch({
    # animate PNGs
      if(out_ext == "gif"){
       if(length(frames) > 800) out("The number of number of frames exceeds 800 and the GIF format is used. This format may not be suitable for animations with a high number of frames, since it causes large file sizes. Consider using a video file format instead.", type = 2)
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
