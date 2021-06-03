#' @export
catn <- function(...){
  cat(..., '\n')
}

#' @export
#' ... = png file paths
paste_png = function(..., fout, width, height, nrow, ncol, units = 'pixels'){

  plots <- lapply(c(...),
                  function(x){
                    img <- as.raster(png::readPNG(x))
                    grid::rasterGrob(img, interpolate = FALSE)
                  })

  ggsave(fout,
         width=as.numeric(width)/300,
         height=as.numeric(height)/300,
         plot = gridExtra::marrangeGrob(grobs = plots, nrow=as.numeric(nrow), ncol=as.numeric(ncol),top=NULL),
         limitsize = F)
}

#' @export
cbind_jagged = function(...){
  cols = list(...)
  max_length = max(map_int(cols, length))
  cols = map(cols, ~c(.x, rep(NA, max_length - length(.x))))
  do.call(cbind, cols)
}

#'@export
colormap = function(lowcol, highcol, ncols = 10000){
  cfun = grDevices::colorRampPalette(c(lowcol, highcol))(ncols)
  f = function(vals){
    normvals = round(1+(ncols-1)*(vals - min(vals))/diff(range(vals)))

    return(setNames(cfun[normvals], names(vals)))
  }
  f
}

#'@export
to_table = function(named_vector){
  cbind(names(named_vector), unname(named_vector))
}

circle_marker = '●'

#'@export
test_cols = function(cols){

  plot(seq_along(cols), rep(1, length(cols)), col = cols, pch = 16, ylim = c(0.5,2.5))

  if (!is.null(names(cols))) {
    text(seq_along(cols), rep(2, length(cols)), names(cols), col = cols)
  }

}
