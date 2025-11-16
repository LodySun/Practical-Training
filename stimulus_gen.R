library(magick)

target_cm <- 2.82
assumed_dpi <- 110
canvas <- round(target_cm / 2.54 * assumed_dpi) #canvas size
bg_col <- "#D9D9D9"                 #background color(white is too strong)
offset_px <- round(0.15 * canvas)   #offset
shape_size <- round(0.12 * canvas)  #shape size
spread <- round(0.17 * canvas)      #distance between each element
stroke <- "black"
stroke_lwd <- 3
pad <- 8
alpha <- 1                 #transparency
outdir <- "stimuli"        #dir
dir.create(outdir, showWarnings = FALSE)

#colors
col_map <- c(
  red   = "#E53935",
  green = "#43A047",
  yellow= "#FDD835",
  blue  = "#1E88E5"
)

#rules
shapes  <- c("triangle","cross","circle","star")
colors  <- c("red","green","yellow","blue")
numbers <- 1:4
dirs    <- c("up","right","down","left")

#generation
new_canvas <- function() {
  image_blank(width = canvas, height = canvas, color = bg_col)
}

open_dev <- function() {
  img <- image_graph(width = canvas, height = canvas, bg = bg_col, res = 96)
  par(mai = c(0,0,0,0), xaxs = "i", yaxs = "i")
  plot(
    NA,
    xlim = c(0, canvas),
    ylim = c(0, canvas),
    type = "n",
    axes = FALSE,
    xlab = "",
    ylab = "",
    asp  = 1
  )
  return(img) 
}


close_dev <- function() {

  dev.off()
}


dir_offset <- function(dir, d = offset_px) {
  switch(dir,
         up    = c( 0, +d),
         down  = c( 0, -d),
         left  = c(-d,  0),
         right = c(+d,  0))
}


safe_gap <- function(shape, n, base_gap = spread, R = shape_size, pad = pad) {

  if (shape == "circle") {
    gap_diag_needed <- ceiling((2*R + pad) / sqrt(2))
    gap_axis_needed <- ceiling(R + pad/2)
    gap <- max(base_gap, gap_diag_needed, gap_axis_needed)
  } else {

    gap <- base_gap
  }
  

  if (n == 4) {
    gap <- gap * 0.75
  }
  return(gap)
}

layout_offsets <- function(n, shape, gap = spread) {

  gap <- safe_gap(shape, n, base_gap = gap, R = shape_size, pad = pad)
  
  if (n == 1) {
    matrix(c(0,0), ncol = 2, byrow = TRUE)
  } else if (n == 2) {
    matrix(c(-gap/2, +gap/2,
             +gap/2, -gap/2), ncol = 2, byrow = TRUE)
  } else if (n == 3) {
    matrix(c(-gap, 0,
             0,   +gap,
             +gap, 0), ncol = 2, byrow = TRUE)
  } else {
    # n == 4
    matrix(c(-gap, +gap,
             +gap, +gap,
             -gap, -gap,
             +gap, -gap), ncol = 2, byrow = TRUE)
  }
}


tri_height_factor <- 1
draw_triangle <- function(cx, cy, sz, col, height_factor = tri_height_factor) {
  h0 <- sqrt(3) * sz
  h  <- h0 * height_factor  
  xs <- c(cx - sz, cx + sz, cx)
  ys <- c(cy - h/2, cy - h/2, cy + h/2)  
  polygon(xs, ys,
          col = adjustcolor(col, alpha.f = alpha),
          border = stroke)
}

draw_cross <- function(cx, cy, sz, col) {
  w <- sz * 0.6
  L <- sz * 1.0
  h <- w / 2
  
  xL  <- cx - L;  xR  <- cx + L
  xIl <- cx - h;  xIr <- cx + h
  yB  <- cy - L;  yT  <- cy + L
  yIb <- cy - h;  yIt <- cy + h

  xs <- c(xL,  xIl, xIl, xIr, xIr, xR,  xR,  xIr, xIr, xIl, xIl, xL)
  ys <- c(yIb, yIb, yB,  yB,  yIb, yIb, yIt, yIt, yT,  yT,  yIt, yIt)
  
  polygon(
    xs, ys,
    col    = adjustcolor(col, alpha.f = alpha),
    border = stroke,
    lwd    = stroke_lwd,
    xpd    = NA
  )
}

draw_circle <- function(cx, cy, sz, col) {
  t <- seq(0, 2*pi, length.out = 100)
  xs <- cx + sz * cos(t)
  ys <- cy + sz * sin(t)
  polygon(xs, ys,
          col = adjustcolor(col, alpha.f = alpha),
          border = stroke, lwd = stroke_lwd)
}

draw_star5 <- function(cx, cy, sz, col) {
  n <- 10
  ang <- seq(-pi/2, -pi/2 + 2*pi, length.out = n+1)[1:n]
  r_out <- sz
  r_in  <- sz * 0.4
  r <- rep(c(r_out, r_in), length.out = n)
  xs <- cx + r * cos(ang)
  ys <- cy + r * sin(ang)
  polygon(xs, ys,
          col = adjustcolor(col, alpha.f = alpha),
          border = stroke, lwd = stroke_lwd)
}

draw_shape <- function(shape, cx, cy, sz, col) {
  switch(shape,
         triangle = draw_triangle(cx, cy, sz, col),
         cross    = draw_cross(cx, cy, sz, col),
         circle   = draw_circle(cx, cy, sz, col),
         star     = draw_star5(cx, cy, sz, col))
}

draw_card <- function(shape, color, number, direction, file_path) {
  while (dev.cur() > 1) dev.off()  
  
  img <- open_dev()  
  
  doff <- dir_offset(direction, offset_px)
  cx <- canvas / 2 + doff[1]
  cy <- canvas / 2 + doff[2]
  
  offs <- layout_offsets(number, shape, spread)
  
  for (i in seq_len(nrow(offs))) {
    dx <- offs[i, 1]; dy <- offs[i, 2]
    draw_shape(shape, cx + dx, cy + dy, shape_size, col_map[[color]])
  }
  
  close_dev()                 
  image_write(img, file_path) 
}

bitmap_names <- c()
for (sh in shapes) {
  for (col in colors) {
    for (num in numbers) {
      for (dirc in dirs) {
        base <- sprintf("%s%d%s_%s", sh, num, col, dirc) # 例: triangle3yellow_left
        outfile <- file.path(outdir, paste0(base, ".png"))
        draw_card(shape = sh, color = col, number = num, direction = dirc, file_path = outfile)
        bitmap_names <- c(bitmap_names, base)
      }
    }
  }
}

writeLines(bitmap_names, con = "mybitmaplist.txt")

cat(sprintf("Done. Generated %d stimuli in '%s' and list file 'mybitmaplist.txt'.\n",
            length(bitmap_names), outdir))
