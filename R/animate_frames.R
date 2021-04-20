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
#' @param pointsize size of each point, default 1
#' @param point  TRUE: only points are plotted, FALSE: segments are plotted, Default TRUE
#' @param rgl.height define the height of the points, e.g zero: points have same height as basemap
#' @param rgl_theta Rotation around z-axis. Default: 45
#' @param rgl_phi Azimuth angle. Default: 45
#' @param rgl_fov Field-of-view angle. Default '0'–isometric.
#' @param rgl_zoom Zoom factor. Default: 1
#' @param mainDir character, directory where rendered frames are stored in the folder called: Output_Frames
#' @param engine character, wether ggplot or rgl as output format
#' @param out_ext character, wether mov or gif as output format. Default is gif.
#' @param verbose logical, if \code{TRUE}, messages and progress information are displayed on the console (default).
#' @param bg_plot Default: FALSE
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
#' @importFrom rgl rgl.close clear3d lines3d points3d rgl.pop legend3d rgl.clear rgl.ids
#' @importFrom rayshader render_snapshot plot_3d
#' @importFrom dplyr count filter
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
#' animate_frames(frames, mainDir = directory, out_ext = "gif")
#'
#' # animate frames as mov
#' animate_frames(frames, mainDir = directory, out_ext = "mov")
#' }
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{join_frames}}
#'
#' @export

animate_frames <-
  function(frames,
           out_file,
           fps = 25,
           width = 700,
           height = 700,
           res = 100,
           end_pause = 0,
           display = TRUE,
           overwrite = FALSE,
           pointsize = 2,
           point = TRUE,
           rgl.height = 5,
           rgl_theta = 45,
           rgl_phi = 45,
           rgl_fov = 0,
           rgl_zoom = 1,
           engine = "rgl",
           out_ext = "gif",
           verbose = TRUE,
           bg_plot = FALSE,
           ...) {
    
    if (frames$prepared_engine == "ggplot" &
        engine == "rgl")
      out(
        "The frames object is not including the rgl variables. Please redo frames_spatial() with prepared_engine = 'all' or prepared_engine = 'rgl'",
        type = 3
      )
    if (frames$prepared_engine == "rgl" &
        engine == "ggplot")
      out(
        "The frames object is not including the ggplot variables. Please redo frames_spatial() with prepared_engine = 'all' or prepared_engine = 'ggplot'",
        type = 3
      )
    
    if (inherits(verbose, "logical"))
      options(moveVis.verbose = verbose)
    
    if (!is.character(out_file))
      out("Argument 'out_file' must be of type 'character'.", type = 3)
    of_split <- strsplit(out_file, "/")[[1]]
    if (length(of_split) > 1)
      if (isFALSE(dir.exists(paste0(utils::head(of_split, n = -1), collapse = "/"))))
        out("Target directory of 'out_file' does not exist.", type = 3)
    if (all(file.exists(out_file),!isTRUE(overwrite)))
      out("Defined output file already exists and overwriting is disabled.",
          type = 3)
    num.args <-
      c(
        fps = fps,
        width = width,
        height = height,
        res = res
      )
    catch <-
      sapply(1:length(num.args), function(i)
        if (!is.numeric(num.args[[i]]))
          out(
            paste0("Argument '", names(num.args)[[i]], "' must be of type 'numeric'."),
            type = 3
          ))
    
    out_ext <-
      tolower(utils::tail(unlist(strsplit(out_file, "[.]")), n = 1))
    out("Rendering animation...")
    if (end_pause > 0) {
      n.add <- round(end_pause * fps)
      frames <-
        append(frames, rep(utils::tail(frames, n = 1), times = n.add))
      out(
        paste0(
          "Number of frames: ",
          toString(length(frames) - n.add),
          " + ",
          toString(n.add),
          " to add \u2248 ",
          toString(dseconds(end_pause)),
          " of pause at the end"
        )
      )
    }
    .stats(n.frames = length(frames), fps)
    
    #frames_expr <- expression(moveVis:::.lapply(frames, function(x) quiet(print(x))))
    
    # create PNGs
    frames_dir <- paste0(tempdir(), "/moveVis/frames/")
    dir.create(frames_dir, recursive = T)
    
    n_frames <- max(frames$move_data$frame)
    
    if(engine == "ggplot"){
      #if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See frames_spatial()).", type = 3)
      if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
      
      # create PNGs
      frames_dir <- paste0(tempdir(), "/moveVis/frames/")
      dir.create(frames_dir, recursive = T)
      out("TESTE 1")
      tryCatch({
        
        file <- file.path(frames_dir, "frame_%05d.png")
        grDevices::png(file, width = width, height = height, res = res)
        graphics::par(ask = FALSE)
        .lapply(as.list(seq(1, n_frames, 1)), function(x) quiet(print(frames[[x]])), moveVis.n_cores = 1)
        grDevices::dev.off()
        frames_files <- list.files(frames_dir, full.names = TRUE)
        
        out("TESTE 2")
        # animate PNGs
        if(out_ext == "gif"){
          if(length(frames) > 800) out("The number of frames exceeds 800 and the GIF format is used. This format may not be suitable for animations with a high number of frames, since it causes large file sizes. Consider using a video file format instead.", type = 2)
          gifski(frames_files, gif_file = out_file, width = width, height = height, delay = (1/fps), progress = verbose)
          #save_gif(.lapply(frames, function(x) quiet(print(x)), moveVis.n_cores = 1), gif_file = out_file, width = width, height = height, delay = (1/fps), progress = verbose, res = res, ...)
        }else{
          av_encode_video(frames_files, output = out_file, framerate = fps, verbose = verbose, ...)
          #av_capture_graphics(.lapply(frames, function(x) quiet(print(x)), moveVis.n_cores = 1), output = out_file, width = width, height = height, res = res, framerate = fps, verbose = verbose, ...) #, vfilter =' framerate=fps=10') 
        }
      }, error = function(e){
        unlink(frames_dir, recursive = TRUE)
        out(paste0("Error creating animation: ", as.character(e)), type = 3)
      }, finally = unlink(frames_dir, recursive = TRUE))
      
      if(isTRUE(display)) utils::browseURL(out_file)
    }
    out("TESTE 3")
    
    if (engine == "rgl") {
      
      render_frame_rgl <- function(i, bg_plot) {
      
        if (i == frames$move_data$frame[1]) {
          bg_plot = FALSE
        } else{
          bg_plot = TRUE
        }
        

        render_frame(
          frames,
          i,
          engine = "rgl",
          pointsize =  pointsize,
          point = point,
          rgl.height = rgl.height,
          rgl_theta = rgl_theta,
          rgl_phi = rgl_phi,
          rgl_fov = rgl_fov,
          rgl_zoom = rgl_zoom,
          bg_plot = bg_plot
          
        )
        
          render_snapshot(
            filename = file.path(frames_dir,paste("frame", formatC(i, width=5, flag="0"), sep="_")),
            title_text = frames$aesthetics$rgl_title,
            title_bar_color = "#022533",
            title_color = "white",
            title_bar_alpha = 1
          )
      
        rgl_id <- rgl::rgl.ids()
        rgl_id <-
          rgl_id[rgl_id$type == "linestrip" |rgl_id$type == "points"| rgl_id$type == "text", ]
        rgl.pop(type = "shapes", id = rgl_id$id)
        gc()
      }
      
      .lapply(frames$move_data$frame, function(i) quiet(render_frame_rgl(i, bg_plot =TRUE)), moveVis.n_cores = 1)
      
      #create animation
      tryCatch({
        frames_files <- list.files(frames_dir, full.names = TRUE)
        
        # animate PNGs
        if (out_ext == "gif") {
          if (length(frames) > 800)
            out(
              "The number of frames exceeds 800 and the GIF format is used. This format may not be suitable for animations with a high number of frames, since it causes large file sizes. Consider using a video file format instead.",
              type = 2
            )
          gifski(
            frames_files,
            gif_file = out_file,
            width = width,
            height = height,
            delay = (1 / fps),
            progress = verbose
          )
          #save_gif(.lapply(frames, function(x) quiet(print(x)), moveVis.n_cores = 1), gif_file = out_file, width = width, height = height, delay = (1/fps), progress = verbose, res = res, ...)
        } else{
          av_encode_video(
            frames_files,
            output = out_file,
            framerate = fps,
            verbose = verbose,
            ...
          )
          #av_capture_graphics(.lapply(frames, function(x) quiet(print(x)), moveVis.n_cores = 1), output = out_file, width = width, height = height, res = res, framerate = fps, verbose = verbose, ...) #, vfilter =' framerate=fps=10')
        }
      }, error = function(e) {
        unlink(frames_dir, recursive = TRUE)
        out(paste0("Error creating animation: ", as.character(e)),
            type = 3)
      }, finally = unlink(frames_dir, recursive = TRUE))
      
      if (isTRUE(display))
        utils::browseURL(out_file)
    }
  }