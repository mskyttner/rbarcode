#' @export
rmdtext <- function(text){
  writeLines(text, con="input.Rmd");
  knit2html("input.Rmd", output="output.html");
  invisible();
}

#' @export
colored_bar <- function(color, char = "") {
  bar <- intToUtf8(9608)  # a full block drawing char
  paste0("<span title=\"", char, "\" style=\"color:", 
         color, ";\">", bar, "</span>")
}

#' @export
color_legend <- function(muted = FALSE) {
  require("xtable")
  cA <- colored_bar(hex_col("red", muted), "A")
  cC <- colored_bar(hex_col("blue", muted), "C")
  cT <- colored_bar(hex_col("green", muted), "T")
  cG <- colored_bar(hex_col("yellow", muted), "G")
  cN <- colored_bar(hex_col("black", muted), "N")
  
  legend <- data.frame(Color = c(cA, cC, cT, cG, cN), 
               Key = c("A", "C", "T", "G", "N"))
}

#' @export
color_symbol <- function(char) {
  
  muted <- FALSE
  if (tolower(char) == char) muted <- TRUE
  
  if (char == "a" ||  char == "A") 
    colored_bar(hex_col("red", muted), char) 
  else if (char == "c" || char == "C") 
    colored_bar(hex_col("blue", muted), char) 
  else if (char == "g" || char == "G") 
    colored_bar(hex_col("yellow", muted), char)
  else if (char == "t" || char == "T") 
    colored_bar(hex_col("green", muted), char)
  else if (char == "n" || char == "N") 
    colored_bar(hex_col("black", muted), "unknown")
  else if (char == "x" || char == "X") 
    colored_bar(hex_col("gray", muted), "match") 
  else if (char == "!") 
    colored_bar(hex_col("white", muted), "no match")
  else if (char == "\n") 
    "<br/>"
  else colored_bar(hex_col("purple", muted), char)
}

#' @export
color_sequence <- function(atcg) {
  gsubfn(".", color_symbol, atcg)  
}

#' @export
actg_as_p <- function(atcg) {
  p <- paste0("<p>", color_sequence(atcg), "</p>")
}

#' @export
actg_as_html <- function(paras) {
  meta <- paste0("<head><meta charset=\"UTF-8\"></head>")
  p <- paste(paras, collapse="<br/>\n")
  html <- paste0("<html>", meta, "<pre>", p, "</pre>", "</html>")  
}

#' @export
barcode <- function(atcg, line_width = 7, width = 20, height = 3) {
  red <- "#e41a1c"
  blue <- "#377eb8"
  green <- "#4daf4a"
  purple <- "#984ea3"
  black <- "#252525"
  gray <- "#cccccc"
  dna <- unlist(strsplit(tolower(atcg), ""))
  cols <- dna
  se <- 1:length(dna)
  pl <- rep(1, length(dna))
  cols[dna=='a'] <- red
  cols[dna=='c'] <- blue
  cols[dna=='g'] <- black
  cols[dna=='t'] <- green
  cols[dna=='n'] <- gray
  cols[dna=='\n'] <- purple
  par(mar = rep(0, 4))
  plot(se, pl, type = "n", axes = FALSE, ylab = "", xlab = "")
  abline(v = se, lwd = line_width, col = cols)
}

#' @export
actg_pdf <- function(rmd) {
  setwd("~/barcode/")
  require("knitr")
  knit(input="actg.Rmd", output="actg.md")
  system("pandoc -o actg.md -t latex -o test.pdf")
  browseURL(url="file:///home/markus/barcode/test.pdf")
}

#' @export
actg_unwrap <- function(x) {
  gsub("\n", "", x, fixed = TRUE)
}

#' @export
actg_wrap <- function(x, width = 63) {
  se <- paste0("(.{", width, "})")
  re <- "\\1\n"
  gsub(se, re, x)
}

#' @export
actg_k3 <- function(x, invert = FALSE) {
  se <- "(.)(.)(.)"
  re <- ifelse(invert, "\\1\\2", "\\3")
  gsub(se, re, x)
}

#' @export
actg_diff <- function(x, y, width = 63, muting = TRUE) {
  require("stringr")
  a <- actg_unwrap(x)
  b <- actg_unwrap(y)
  maxlen <- max(nchar(a), nchar(b))
  a <- str_pad(a, maxlen, side = "right")
  b <- str_pad(b, maxlen, side = "right")
  xx <- unlist(strsplit(actg_unwrap(a), ""))
  yy <- unlist(strsplit(actg_unwrap(b), ""))
  m <- paste(collapse = "", ifelse(xx == yy, "!", "X"))
  aa <- paste(collapse = "", ifelse(xx == yy, tolower(xx), toupper(xx)))
  bb <- paste(collapse = "", ifelse(xx == yy, tolower(yy), toupper(yy)))
  aw <- actg_wrap(aa, width)
  bw <- actg_wrap(bb, width)
  mw <- actg_wrap(m, width)
  aws <- unlist(strsplit(aw, "\n", fixed = TRUE))
  bws <- unlist(strsplit(bw, "\n", fixed = TRUE))
  mws <- unlist(strsplit(mw, "\n", fixed = TRUE))
  res <- paste(aws, mws, bws, sep = "\n", collapse = "\n\n")  
  ifelse(muting, res, toupper(res))
}

#paras <- sapply(df$actg, actg_as_p)
#html <- actg_as_html(paras)
#writeLines(html, "~/test/actg.html")
#browseURL(url="file:///home/markus/test/actg.html")


#' @export
hex_col <- function(color = c("blue", "green", "red", 
                              "yellow", "purple", "gray", 
                              "black", "white"),
                    muted = FALSE) {

  strong <- muted
  
  # colors from colorbrewer2.org
  
  blue <- ifelse(strong, "#a6cee3", "#1f78b4")
  green <- ifelse(strong, "#b2df8a", "#33a02c")
  red <- ifelse(strong, "#fb9a99", "#e31a1c")
  yellow <- ifelse(strong, "#fdbf6f", "#ff7f00")
  purple <-  ifelse(strong, "#cab2d6", "#6a3d9a")
  gray <- ifelse(strong, "#cccccc", "#636363")
  black <- "#252525"
  white <- "#ffffff"  
    
  out <- match.arg(color)
  
  switch(out,
    blue = return(blue),
    green = return(green),
    red = return(red),
    yellow = return(yellow),
    purple = return(purple),
    gray = return(gray),
    black = return(black),
    white = return(white)
  )
  
}