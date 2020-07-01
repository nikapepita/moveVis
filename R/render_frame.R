#' Render an individual frame
#' 
#' This function renders an individual frame. It yields the same result as if an individual frame is extracted using default subsetting \code{[[]]} with the difference that the rendering engine can be defined as an argument. Currently, \code{moveVis} supports \code{ggplot2} for 2D and \code{rgl} for 3D renders and animations.
#' 
#' @inheritParams settings
#' @inheritParams add_gg
#' @param i numeric, index number of the frame to be rendered. Per default, the last frame is rendered.
#' 
#' @export
#' 
#' @importFrom rgl points3d rgl.close lines3d legend3d scene3d clear3d text3d
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
render_frame <- function(frames, i = length(frames), engine = "ggplot2", pointsize=2,point=TRUE,height=5){
  
  # checking subscript
  if(length(i) > 1) out("Subscript must be of length 1.", type = 3)
  if(i > max(frames$move_data$frame)) out(paste0("Subscript out of bounds. Length of frames is ", max(frames$move_data$frame), "."), type = 3)
  
  # make sure there always is a correct engine selected
  if(is.null(engine)){
    engine <- "ggplot2"
  }else{
    if(all(engine != "ggplot2", engine != "rgl")) engine <- "ggplot2"
  }
  
  if(engine == "ggplot2"){
    if(inherits(frames, "frames_spatial")){
      gg <- gg.spatial(x = .df4gg(frames$move_data,
                                  i = i,
                                  tail_length = frames$aesthetics$tail_length,
                                  path_size = frames$aesthetics$path_size,
                                  tail_size = frames$aesthetics$tail_size,
                                  tail_colour = frames$aesthetics$tail_colour,
                                  trace_show = frames$aesthetics$trace_show,
                                  trace_colour = frames$aesthetics$trace_colour,
                                  path_fade = frames$aesthetics$path_fade),
                       y = gg.bmap(r = frames$raster_data[[if(length(frames$raster_data) > 1) i else 1]],
                                   r_type = frames$aesthetics$r_type,
                                   maxpixels = frames$aesthetics$maxpixels,
                                   alpha = frames$aesthetics$alpha,
                                   maxColorValue = frames$aesthetics$maxColorValue),
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
                       equidistant = frames$aesthetics$equidistant)
    }
    if(inherits(frames, "frames_graph")){
      if(frames$graph_type == "flow"){
        gg <- .gg_flow(x = frames$move_data[frames$move_data$frame <= i,],
                       y = frames$move_data,
                       path_legend = frames$aesthetics$path_legend,
                       path_legend_title = frames$aesthetics$path_legend_title,
                       path_size = frames$aesthetics$path_size,
                       val_seq = frames$aesthetics$val_seq)
      }
      if(frames$graph_type == "hist"){
        gg <- .gg_hist(x = frames$hist_data[[i]],
                       y = do.call(rbind, frames$hist_data),
                       path_legend = frames$aesthetics$path_legend,
                       path_legend_title = frames$aesthetics$path_legend_title,
                       path_size = frames$aesthetics$path_size,
                       val_seq = frames$aesthetics$val_seq,
                       r_type = frames$aesthetics$r_type)
      }
    }
    
    # any additions?
    if(!is.null(frames$additions)){
      for(ix in 1:length(frames$additions)){
        x <- frames$additions[[ix]]
        if(length(x$arg) > 0) for(j in 1:length(x$arg)) assign(names(x$arg)[[j]], x$arg[[j]])
        if(length(x$data) > 0) assign("data", x$data[[i]])
        gg <- gg + eval(x$expr[[i]])
      }
    }
    return(gg)
  }
  if(engine == "rgl"){
    
    ##calculte number of individuals
    categories <- as.character(unique(frames$move_data$colour))
    nr.Categories <- length(categories)
    
    cat.length <- c()
    
    cat.length <- lapply(1:nr.Categories, function(i){
      length <- c(length(which(frames$move_datacolour == categories[i])))
      cat.length <- c(cat.length,length)
    })
    
    categories.df <- as.data.frame(cbind(as.character(categories),as.numeric(cat.length)))
    categories.df <- categories.df[order(categories.df$V2, decreasing = TRUE),]
    categories.df$V2 <- as.numeric(as.character(categories.df$V2))
    
    rgl::clear3d()
    
    ##plot background basemap
    frames$rgl_background()
    
    
    if(point==FALSE){
        
        m.df.temp <- frames$move_data[which(frames$move_data$frame<=i+1),]
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
          m.df.point[,10],
          (m.df.point[,12] / frames$rgl_zscale)+height,  
          -m.df.point[,11],
          size = pointsize, col = m.df.point[,8])}
        
        if(!(nrow(m.df.seg)==0)) 
        {
          
          if(length(unique(m.df.seg$colour))>1){
            
            m.df.seg <- split(m.df.seg,m.df.seg$colour)
            
            for (i in 1:length(m.df.seg)){
              rgl::lines3d(m.df.seg[[i]][,10],
                      (m.df.seg[[i]][,12]/frames$rgl_zscale)+height,  
                      -m.df.seg[[i]][,11],
                      lwd=pointsize, col = m.df.seg[[i]][,8])
            }
          }else{
            rgl::lines3d(m.df.seg[,10],
                    (m.df.seg[,12]/frames$rgl_zscale)+height,  
                    -m.df.seg[,11],
                    lwd=pointsize, col = m.df.seg[,8])
          }
        }
        
    }else{
      ##create frames
        
        m.df.temp <- frames$move_data[which(frames$move_data$frame<=i),]
        m.df.temp <- m.df.temp[order(m.df.temp$colour),]
        
        categories <- as.character(unique(m.df.temp$colour))
        nr.Categories <- length(categories)
        
        rgl::points3d(
          m.df.temp[,10],
          m.df.temp[,12] / frames$rgl_zscale,
          -m.df.temp[,11],
          size = pointsize, col = m.df.temp[,8])

    }
    return(NULL)
  }
}