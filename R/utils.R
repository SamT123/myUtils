#' @export
catn <- function(...) {
  cat(..., '\n')
}


#' ... = png file paths
#' @export
paste_png = function(
  ...,
  fout,
  width,
  height,
  nrow,
  ncol,
  widths = NULL,
  heights = NULL,
  units = 'pixels',
  just = "centre"
) {
  plots <- lapply(c(...), function(x) {
    img <- as.raster(png::readPNG(x))
    grid::rasterGrob(img, interpolate = FALSE, just = just)
  })

  ggsave(
    fout,
    width = as.numeric(width) / 300,
    height = as.numeric(height) / 300,
    plot = gridExtra::arrangeGrob(
      grobs = plots,
      nrow = as.numeric(nrow),
      ncol = as.numeric(ncol),
      top = NULL,
      widths = widths,
      heights = heights
    ),
    limitsize = F
  )
}

#' ... = ggplot objects
#' @export
paste_gg = function(
  ...,
  fout,
  save_heights = 5,
  save_widths = 5,
  row_heights = NULL,
  row_widths = NULL,
  total_height = 5,
  total_width = 10,
  nrow,
  ncol
) {
  plots = list(...)
  if (length(save_heights) == 1) {
    save_heights = rep(save_heights, length(plots))
  }
  if (length(save_widths) == 1) {
    save_widths = rep(save_widths, length(plots))
  }

  for (i in seq_along(plots)) {
    ggsave(
      plots[[i]],
      filename = paste0('TEMPFILE', i, '.png'),
      width = save_widths[[i]],
      height = save_heights[[i]]
    )
  }

  args = c(
    as.list(paste0('TEMPFILE', seq_along(plots), '.png')),

    list(
      fout = fout,
      widths = row_widths,
      heights = row_heights,
      width = total_width,
      height = total_height,
      nrow = nrow,
      ncol = ncol
    )
  )

  do.call(
    paste_png,
    args
  )

  purrr::walk(seq_along(plots), ~ file.remove(paste0('TEMPFILE', .x, '.png')))
}

#' @export
to_pdf = function(png_file) {
  library(raster)
  pdf_file = paste0(
    tools::file_path_sans_ext(png_file),
    ".pdf"
  )

  r <- brick(png_file)
  pdf(
    pdf_file,
    width = dim(r)[[2]] / 1200,
    height = dim(r)[[1]] / 1200
  )
  plotRGB(r, maxpixels = Inf)
  dev.off()

  pdf_file
}

#' @export
cbind_jagged = function(...) {
  cols = list(...)
  cols_split = list()
  for (col in cols) {
    if (is.array(col)) {
      for (i in seq_len(ncol(col))) {
        cols_split = c(cols_split, list(col[, i]))
      }
    } else {
      cols_split = c(cols_split, list(col))
    }
  }
  max_length = max(map_int(cols_split, length))
  cols_split = map(cols_split, ~ c(.x, rep(NA, max_length - length(.x))))
  do.call(cbind, cols_split)
}

#'@export
colormap = function(lowcol, highcol, ncols = 10000) {
  cfun = grDevices::colorRampPalette(c(lowcol, highcol))(ncols)
  f = function(vals) {
    normvals = round(1 + (ncols - 1) * (vals - min(vals)) / diff(range(vals)))

    return(setNames(cfun[normvals], names(vals)))
  }
  f
}

#'@export
colorpal = function(colors = list("black" = 0, "red" = 1), dens = 1000) {
  colors = colors[order(unlist(colors))]

  o = c()

  for (i in seq_len(length(colors) - 1)) {
    o = c(
      o,
      setNames(
        seq(colors[[i]], colors[[i + 1]], length.out = dens),
        colorRampPalette(c(names(colors)[[i]], names(colors)[[i + 1]]))(dens)
      )
    )
  }

  function(x) {
    purrr::map_chr(
      x,
      function(x_i) {
        if (is.na(x_i)) {
          return(NA)
        }
        names(o)[which.min(abs(o - x_i))]
      }
    )
  }
}


#'@export
to_table = function(named_vector) {
  cbind(names(named_vector), unname(named_vector))
}

circle_marker = '●'

#'@export
test_cols = function(cols) {
  plot(
    rep(1, length(cols)),
    seq_along(cols),
    col = cols,
    pch = 16,
    xlim = c(0.9, 2),
    xlab = '',
    ylab = '',
    xaxt = 'n',
    yaxt = 'n'
  )

  if (!is.null(names(cols))) {
    text(
      rep(1.1, length(cols)),
      seq_along(cols),
      names(cols),
      col = cols,
      adj = 0
    )
  }
}

#' @export
dbpath = '../../../../../pkgs/databases/h3n2/'

#' @export
add_titles = function(
  plots,
  col_titles,
  row_titles,
  fontsize = 10,
  margins = c(.2, .2)
) {
  nc = length(col_titles)
  nr = length(row_titles)

  blank_plot = ggplot() + theme_void()

  col_title_plots = map(
    col_titles,
    ~ gridExtra::arrangeGrob(
      blank_plot,
      bottom = grid::textGrob(
        .x,
        gp = grid::gpar(
          fontsize = fontsize
        )
      )
    ) %>%
      ggplotify::as.ggplot()
  )

  row_title_plots = map(
    row_titles,
    ~ gridExtra::arrangeGrob(
      blank_plot,
      right = grid::textGrob(
        .x,
        gp = grid::gpar(fontsize = fontsize),
        rot = 90
      )
    ) %>%
      ggplotify::as.ggplot()
  )

  plots_new = list()
  pads = 0
  for (i in seq_len((nr + 1) * (nc + 1))) {
    if (i == 1) {
      plots_new = c(plots_new, list(blank_plot))
      pads = pads + 1
    } else if (i <= nc + 1) {
      plots_new = c(plots_new, list(col_title_plots[[i - 1]]))
      pads = pads + 1
    } else if (i %% (nc + 1) == 1) {
      plots_new = c(plots_new, list(row_title_plots[[(i - 1) / (nc + 1)]]))
      pads = pads + 1
    } else {
      plots_new = c(plots_new, list(plots[[(i - pads)]]))
    }
  }

  gridExtra::arrangeGrob(
    grobs = plots_new,
    nrow = nr + 1,
    ncol = nc + 1,
    heights = c(margins[[1]], rep(1, nr)),
    widths = c(margins[[2]], rep(1, nc))
  ) %>%
    ggplotify::as.ggplot() -> P
  P
}

#' @export
add_titles_png = function(
  pngs,
  col_titles,
  row_titles,
  fontsize = 10,
  margins = c(.2, .2)
) {
  plots <- lapply(pngs, function(x) {
    img <- as.raster(png::readPNG(x))
    grid::rasterGrob(img, interpolate = FALSE)
  })

  add_titles(plots, col_titles, row_titles, fontsize, margins)
}

#' @export
gitcommit <- function(msg = "commit from Rstudio", dir = getwd()) {
  cmd = sprintf("cd %s; pwd; git commit -am\"%s\"", dir, msg)
  system(cmd)
}

# # @import stringr
# # @export
# fast_fasta = function(path){
#   message('Loading seqs')
#   seqs = readr::read_lines(path)
#
#   nms = seqs[seq(1, length(seqs), 2)]
#   sqs = seqs[seq(1, length(seqs), 2)+1]
#   rm(seqs)
#
#   if (all(substr(nms[1:min(100, length(nms))], 1, 1) == '>')){
#
#     message('Setting names')
#     names(sqs) = substring(nms, 2, 1000000)
#   }
#
#   sqs
# }
#
#
# # @export
# fast_fasta.write = function(seqs, names, path){
#   stopifnot(length(seqs) == length(names))
#
#   x <- vector(class(seqs), 2*length(seqs))
#   x[c(TRUE, FALSE)] <- paste0('>',names)
#   x[c(FALSE, TRUE)] <- seqs
#
#   readr::write_lines(x, file = path)
# }

#' @export
to_repr = function(v, newlines = F, v_class = 'infer') {
  if (v_class == 'infer') {
    v_class = class(v)
  }

  if (!is.null(names(v))) {
    w = purrr::map_chr(seq_along(v), function(i) {
      if (names(v)[[i]] != '') {
        return(paste(names(v)[[i]], ' = ', v[[i]]))
      } else {
        return(v[[i]])
      }
    })
  } else {
    w = v
  }
  o = paste0(w, collapse = ifelse(newlines, '",\n  "', '", "'))
  o = paste0('c("', o, '")')

  class(o) = v_class
  cat(o)
}

#' @export
with_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  force(code)
}


#' @export

#'@export
remove_internal_ws = function(x) {
  x_old = x
  x = str_replace_all(x, fixed('  '), ' ')

  while (x != x_old) {
    x_old = x
    x = str_replace_all(x, fixed('  '), ' ')
  }

  x
}

#'@export
ins = function(x, loc, ins) {
  lhs = str_sub(x, 1, loc)
  rhs = str_sub(x, loc + 1)

  paste0(lhs, ins, rhs)
}

#'@export
linewrap = function(x, len) {
  x_len = str_length(x)

  if (x_len < len) {
    return(x)
  }

  for (i in seq(len, x_len, len)) {
    x = ins(x, i + 1 * (i / len - 1), '\n')
  }
  x
}

#'@export
ellipsis = function(x, n) {
  add = stringr::str_length(x) > n
  x = substr(x, 1, n)
  x[add] = paste0(x[add], '...')
  x
}


a = (list(
  list(a = 1, b = 2),
  list(a = 1, b = 2, c = 3),
  list(a = 1, b = 2)
))

match(a, unique(a))


efficient_map_vectorized = function(v, f, verbose = T, ...) {
  v_unq = unique(v)
  if (verbose) {
    message('length = ', length(v_unq))
  }
  o = f(v_unq, ...)
  o[match(v, v_unq)]
}


#'@export
efficient_map = function(
  l,
  f,
  ...,
  updates = Inf,
  l_proxy = NULL,
  num_cores = 1,
  outfile = 'outfile.txt',
  delete_outfile = T
) {
  unq_l = unique(l)
  len = length(unq_l)

  if (!is.null(l_proxy)) {
    unq_l_match = unique(l_proxy)
    l_match = l_proxy
  } else {
    unq_l_match = unq_l
    l_match = l
  }

  o = as.list(rep(NA, len))

  if (!is.infinite(updates)) {
    catn('Total to do = ', len)
  }

  if (num_cores > 1) {
    if (file.exists(outfile)) {
      stop(
        "outfile: ",
        outfile,
        ' already exists - stopping to prevent modification'
      )
    }

    message('Registering cluster...')
    cl = parallel::makeCluster(num_cores, outfile = outfile)
    doParallel::registerDoParallel(cl)

    message("Output from function will be in `", outfile, "`")
    o = parallel::parLapply(
      cl,
      unq_l,
      f,
      ...
    )
    parallel::stopCluster(cl)

    if (delete_outfile) file.remove(outfile)
  } else {
    for (i in seq_along(unq_l)) {
      if (i %% updates == 0) {
        myUtils::catn(i, ' / ', len)
      }
      o[[i]] = f(unq_l[[i]], ...)
    }
  }

  o[match(l_match, unq_l_match)]
}


#'@export
reload = function(pkg) {
  devtools::unload(pkg)
  library(pkg, character.only = T)
  NULL
}


#'@export
replace = function(v, replace_map) {
  map_if(v, ~ .x %in% names(replace_map), ~ replace_map[[.x]])
}

#'@export
anchorify = function(id) {
  id %>%
    str_replace_all("[\\\\/:*?\"<>| ]", "_")
}


#'@export
compress <- function(to_zip, zip_path, recurse = F, root = '.') {
  myUtils::with_dir(
    dir = root,
    code = {
      cmd = paste0(
        'zip ',
        ifelse(recurse, ' -rj ', ' -j '),
        zip_path,
        ' ',
        to_zip
      )
      system(cmd)
    }
  )
}

#'@export
add_to_PATH = function(path) {
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(old_path, path, sep = ":"))
}


# plotting ---------------------------------------------------------------------

#'@export
default_ggplot_params = list(
  linewidth = 0.2 / 0.75,
  broad_linewidth = 0.4 / 0.75,

  pointsize = 1 / 0.75,

  textsize_pt = 10,
  textsize_pt_small = 8,

  lightgrey = "grey92",
  midlightgrey = "grey75",
  middarkgrey = "grey60",
  darkgrey = "grey40",

  few_pal = setNames(
    rev(ggthemes::few_pal()(8)),
    c("red", "yellow", "purple", "brown", "pink", "green", "orange", "blue")
  )
)

#'@export
set_ggplot_default_params = function() {
  update_geom_defaults(
    "text",
    list(family = "Arial", size = default_ggplot_params$textsize_pt_small)
  )
  update_geom_defaults(
    "line",
    list(linewidth = default_ggplot_params$linewidth)
  )
  update_geom_defaults(
    "hline",
    list(linewidth = default_ggplot_params$linewidth)
  )
  update_geom_defaults(
    "vline",
    list(linewidth = default_ggplot_params$linewidth)
  )
  update_geom_defaults(
    "point",
    list(linewidth = default_ggplot_params$pointsize)
  )
}

#'@export
theme_default = function() {
  theme_classic(
    base_size = default_ggplot_params$textsize_pt,
    # base_family = "Arial",
    base_line_size = default_ggplot_params$linewidth,
    base_rect_size = default_ggplot_params$linewidth
  ) +
    theme(
      axis.line = element_line(
        linewidth = rel(1)
        # color = default_ggplot_params$midgrey
      ),
      axis.ticks = element_line(
        linewidth = rel(0.6)
        # color = default_ggplot_params$midgrey
      ),

      axis.text = element_text(size = default_ggplot_params$textsize_pt_small),
      legend.text = element_text(
        size = default_ggplot_params$textsize_pt_small
      ),
      strip.text = element_text(size = default_ggplot_params$textsize_pt_small),

      axis.title = element_text(size = default_ggplot_params$textsize_pt),
      legend.title = element_text(size = default_ggplot_params$textsize_pt),

      legend.key.height = unit(0.075, "mm"),
      legend.key.width = unit(0.075, "mm"),
      legend.key = element_rect(
        fill = NA,
        color = NA
      ),

      legend.spacing = unit(0, "mm"),

      legend.position.inside = c(0, 1),
      legend.justification = c(0, 1),

      legend.background = element_rect(
        fill = colorspace::adjust_transparency("white", alpha = 0.4),
        color = NA
      ),
      legend.box.background = element_rect(
        fill = NA,
        color = NA,
        linewidth = 0.2
      ),
      legend.box.margin = margin(t = 0, 1, 1, 1, "mm"),
      legend.box = "horizontal",

      panel.grid = element_line(
        linewidth = rel(0.8),
        color = default_ggplot_params$lightgrey
      ),
      panel.spacing = unit(0.1, "in"),

      strip.background = element_rect(
        fill = NA, #default_ggplot_params$lightgrey,
        color = "transparent"
      ),

      plot.margin = margin(l = 2, t = 3, r = 3, unit = "mm"),
    )
}
