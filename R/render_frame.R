#' Render an individual frame
#'
#' This function renders an individual frame. It yields the same result as if an individual frame is extracted using default subsetting \code{[[]]} with the difference that the rendering engine can be defined as an argument. Currently, \code{moveVis} supports \code{ggplot2} for 2D and \code{rgl} for 3D renders and animations.
#'
#' @inheritParams settings
#' @inheritParams add_gg
#' @param i numeric, index number of the frame to be rendered. Per default, the last frame is rendered.
#' @param engine character, wether ggplot or rgl as output format
#' @param pointsize size of each point, default 1
#' @param point TRUE: only points are plotted, FALSE: segments are plotted, Default TRUE
#' @param height_3D  define the height of the points, e.g zero: points have same height as basemap
#' @param theta_3D  Rotation around z-axis. Default: 45
#' @param phi_3D  Azimuth angle. Default: 45
#' @param fov_3D  Field-of-view angle. Default '0'–isometric.
#' @param zoom_3D  Zoom factor. Default: 1
#' @param bg_plot Default: FALSE
#' @export
#'
#' @importFrom rgl rgl.close clear3d lines3d points3d rgl.pop legend3d rgl.viewpoint text3d
#' @importFrom rayshader render_snapshot plot_3d
#' @importFrom dplyr count filter
#' @importFrom raster extend
#'
#' @examples
#'
#' library(moveVis)
#' library(move)
#' data("move_data")
#' data("basemap_data")
#'
#' r_list <- basemap_data[[1]]
#' r_times <- basemap_data[[2]]
#'
#' # align
#' m <- align_move(m = move_data, res = 4, unit = "mins")
#'
#' # create frames
#' frames <- frames_spatial(m, r_list = r_list, r_times = r_times, fade_raster = T)
#'
#' # viewing frames calling this function:
#' render_frame(frames) # displays the last frame in 2D using ggplot2 (default)
#' render_frame(frames, i = 100) # displays frame 100 in 2D using ggplot2 (default)
#'
#' \dontrun{
#' render_frame(frames, i = 100, engine = "rgl") # displays frame 100 in 3D using rgl
#'
#' # alternatively, you can simply use `[[` to do this:
#' frames[[100]] # displays frame 100 in 2D using ggplot2 (default)
#'
#' # use the settings to change the default rendering engine
#' set_engine(engine = "rgl")
#' frames[[100]] # displays frame 100 in 3D using rgl
#' }
render_frame <-
  function(frames,
           i = length(frames),
           engine = "ggplot",
           pointsize = 2,
           point = TRUE,
           height_3D = 5,
           theta_3D = 45,
           phi_3D = 45,
           fov_3D = 0,
           zoom_3D = 1,
           bg_plot = FALSE,
           ...) {
    if (frames$prepared_engine == "2D"&
        engine == "3D")
      out(
        "The frames Object is not including the rgl variables. Please redo frames_spatial() with prepared_engine = 'all' or prepared_engine = 'rgl'",
        type = 3
      )
    if (frames$prepared_engine == "3D"&
        engine == "2D")
      out(
        "The frames Object is not including the ggplot variables. Please redo frames_spatial() with prepared_engine = 'all' or prepared_engine = 'ggplot'",
        type = 3
      )
    
    # checking subscript
    if (length(i) > 1)
      out("Subscript must be of length 1.", type = 3)
    if (i > max(frames$move_data$frame))
      out(paste0(
        "Subscript out of bounds. Length of frames is ",
        max(frames$move_data$frame),
        "."
      ),
      type = 3)
    
    # make sure there always is a correct engine selected
    if (is.null(engine)) {
      engine <- "2D"
    } else{
      if (all(engine != "2D", engine != "3D"))
        engine <- "2D"
    }
    
    if (engine == "2D") {
      if (inherits(frames, "frames_spatial")) {
        gg <- gg.spatial(
          x = .df4gg(
            frames$move_data,
            i = i,
            tail_length = frames$aesthetics$tail_length,
            path_size = frames$aesthetics$path_size,
            tail_size = frames$aesthetics$tail_size,
            tail_colour = frames$aesthetics$tail_colour,
            trace_show = frames$aesthetics$trace_show,
            trace_colour = frames$aesthetics$trace_colour,
            path_fade = frames$aesthetics$path_fade
          ),
          y = gg.bmap(
            r = frames$raster_data[[if (length(frames$raster_data) > 1)
              i
              else
                1]],
            r_type = frames$aesthetics$r_type,
            maxpixels = frames$aesthetics$maxpixels,
            alpha = frames$aesthetics$alpha,
            maxColorValue = frames$aesthetics$maxColorValue
          ),
          m_names = frames$move_data$name,
          m_colour = frames$move_data$colour,
          path_end = frames$aesthetics$path_end,
          path_join = frames$aesthetics$path_join,
          path_mitre = frames$aesthetics$path_mitre,
          path_arrow = frames$aesthetics$path_arrow,
          path_alpha = frames$aesthetics$path_alpha,
          path_legend = frames$aesthetics$path_legend,
          path_legend_title = frames$aesthetics$path_legend_title,
          path_size = frames$aesthetics$path_size,
          equidistant = frames$aesthetics$equidistant
        )
      }
      if (inherits(frames, "frames_graph")) {
        if (frames$graph_type == "flow") {
          gg <- .gg_flow(
            x = frames$move_data[frames$move_data$frame <= i, ],
            y = frames$move_data,
            path_legend = frames$aesthetics$path_legend,
            path_legend_title = frames$aesthetics$path_legend_title,
            path_size = frames$aesthetics$path_size,
            val_seq = frames$aesthetics$val_seq
          )
        }
        if (frames$graph_type == "hist") {
          gg <- .gg_hist(
            x = frames$hist_data[[i]],
            y = do.call(rbind, frames$hist_data),
            path_legend = frames$aesthetics$path_legend,
            path_legend_title = frames$aesthetics$path_legend_title,
            path_size = frames$aesthetics$path_size,
            val_seq = frames$aesthetics$val_seq,
            r_type = frames$aesthetics$r_type
          )
        }
      }
      
      # any additions?
      if (!is.null(frames$additions)) {
        for (ix in 1:length(frames$additions)) {
          x <- frames$additions[[ix]]
          if (length(x$arg) > 0)
            for (j in 1:length(x$arg))
              assign(names(x$arg)[[j]], x$arg[[j]])
          if (length(x$data) > 0)
            assign("data", x$data[[i]])
          gg <- gg + eval(x$expr[[i]])
        }
      }
      return(gg)
    }
    if (engine == "3D") {
      
      # calculte number of individuals
      categories <- as.character(unique(frames$move_data$colour))
      nr.Categories <- length(categories)
      
      cat.length <- c()
      
      cat.length <- lapply(1:nr.Categories, function(i) {
        length <- c(length(which(
          frames$move_data$colour == categories[i]
        )))
        cat.length <- c(cat.length, length)
      })
      
      categories.df <-
        as.data.frame(cbind(as.character(categories), as.numeric(cat.length)))
      categories.df <-
        categories.df[order(categories.df$V2, decreasing = TRUE), ]
      categories.df$V2 <- as.numeric(as.character(categories.df$V2))
      
      if (is.list(theta_3D ) &&
          length(theta_3D ) == length(frames))
        theta <- as.numeric(theta_3D [[i]])
      else
        theta = as.numeric(theta_3D [[1]])
      if (is.list(phi_3D ) &&
          length(phi_3D ) == length(frames))
        phi <- as.numeric(phi_3D [[i]])
      else
        phi = as.numeric(phi_3D [[1]])
      if (is.list(fov_3D ) &&
          length(fov_3D ) == length(frames))
        fov <- as.numeric(fov_3D [[i]])
      else
        fov = as.numeric(fov_3D [[1]])
      
      if (bg_plot == FALSE) {
        # clean rgl window
        clear3d()
        
        # plot 3d map
        plot_3d(
          frames$scene_3D,
          frames$matrix_elevation,
          zscale = frames$aesthetics$zscale_3D,
          zoom = zoom_3D ,
          background = frames$aesthetics$colour_background_3D,
          theta = theta,
          phi = phi,
          fov = fov
        )
        
        if(length(frames$additions)>0) lapply(1:length(frames$additions), 
                                              function (k){if(!is.null(frames$additions[[k]]$rgl_compass)) frames$additions[[k]]$rgl_compass(i)})
        
        if(length(frames$additions)>0) lapply(1:length(frames$additions), 
                                              function (k){if(!is.null(frames$additions[[k]]$rgl))frames$additions[[k]]$rgl(i)})
        
        #plot legend
        if(frames$aesthetics$path_legend==TRUE){
          
         if("colour_add" %in% colnames(frames$move_data)){
           
           legend3d("bottomright",lty = c(1,2), legend = paste(sort(unique(frames$move_data$colour_add))),
                    pch = 16, col = unique(frames$move_data$colour_add) , cex = 1,inset = c(0.02), title= frames$aesthetics$path_legend_title,  bty = "n")
           
         }else{
         legend3d("bottomright",legend = paste('Name', unique(frames$move_data$name)),
                  pch = 16,col = unique(frames$move_data$colour),cex = 1,  bty = "n")
         }}
        
      } else if (is.list(theta_3D ) &
                 length(theta_3D ) == length(frames) ||
                 is.list(phi_3D ) &
                 length(phi_3D ) == length(frames) ||
                 is.list(fov_3D ) & length(fov_3D ) == length(frames)) {
      
        
        rgl.viewpoint(
          theta = theta,
          phi = phi,
          fov = fov,
          zoom = zoom_3D 
        )
        
        
        if(length(frames$additions)>0) lapply(1:length(frames$additions), 
                                              function (k){if(!is.null(frames$additions[[k]]$rgl))frames$additions[[k]]$rgl(i)})
        
      }
      
      # add movement data, as points or lines
      if (point == FALSE) {
        m.df.temp <- frames$move_data[which(frames$move_data$frame <= i + 1), ]
        m.df.temp <- m.df.temp[order(m.df.temp$colour), ]
        
          
        categories <- as.character(unique(m.df.temp$colour))
        nr.Categories <- length(categories)
        if("colour_add" %in% colnames(m.df.temp))
          m.df.temp = m.df.temp[, c(1,2,3,4,5,6,7,13,9,10,11,12,8)]
          
        nr <- count(m.df.temp, vars = colour)
        
        nr_seg <- nr %>% filter(nr$n >= 2)
        nr_point <- nr %>% filter(n == 1)
        
        m.df.seg <-
          m.df.temp[which(m.df.temp$colour %in% nr_seg$vars), ]
        m.df.point <-
          m.df.temp[which(m.df.temp$colour == nr_point$vars), ]
        
        if (!(nrow(m.df.point) == 0))
        {
          
          points3d(
            m.df.point[, 10],
            (m.df.point[, 12] / frames$aesthetics$zscale_3D) + height_3D+(j/2),-m.df.point[, 11],
            size = pointsize,
            col = m.df.point[, 8] #8
          )
        }
        
        if (!(nrow(m.df.seg) == 0))
        {
          if (length(unique(m.df.seg$colour)) > 1) {
            m.df.seg <- split(m.df.seg, m.df.seg$colour)
            
            
            for (j in 1:length(m.df.seg)) {
              lines3d(
                m.df.seg[[j]][, 10],
                (m.df.seg[[j]][, 12] / frames$aesthetics$zscale_3D) +
                  height_3D +(j/2) ,-m.df.seg[[j]][, 11],
                lwd = pointsize,
                col = m.df.seg[[j]][, 8]
              )
            }
          } else{
            
            lines3d(
              m.df.seg[, 10],
              (m.df.seg[, 12] / frames$aesthetics$zscale_3D) + height_3D+(j/2),-m.df.seg[, 11],
              lwd = pointsize,
              col = m.df.seg[, 8]
            )
          }
        }
        
      } else{
        # create frames
        
        m.df.temp <-
          frames$move_data[which(frames$move_data$frame <= i), ]
        m.df.temp <- m.df.temp[order(m.df.temp$colour), ]
        
        categories <- as.character(unique(m.df.temp$colour))
        nr.Categories <- length(categories)
        
        points3d(
          m.df.temp[, 10],
          (m.df.temp[, 12] / frames$aesthetics$zscale_3D) + height_3D++(j/2),-m.df.temp[, 11],
          size = pointsize,
          color = m.df.temp[, 11]
        )
      }
      
      render_snapshot(
        title_text = frames$aesthetics$title_3D,
        title_bar_color = "#022533",
        title_color = "white",
        title_bar_alpha = 1
      )
    }
  }

