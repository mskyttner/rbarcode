#!/usr/bin/Rscript
require("opencpu")
require("rbarcode")

opencpu$start(12345)

cat("Showing markdown app \n")
opencpu$view("/library/rbarcode/www")
cat("Press Enter to proceed \n")
x <- readLines(file("stdin"), n = 1)

cat("Done \n")

opencpu$stop()

q(status=0)
