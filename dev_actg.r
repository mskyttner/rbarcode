require("devtools")

#create("~/rbarcode")

setwd("~/rbarcode")
options(devtools.desc.author="'Markus Skyttner <markus.skyttner@rnm.se> [aut, cre]'")

load_all()
document()
build()
install()


setwd("~/rbarcode/inst/reports")
require("knitr")
knit2html(input="actg.Rmd", output="index.html")

# package from http://documentup.com/ramnathv/slidify
require("slidify")
require("RecordLinkage")
slidify("inst/www/actg_deckdir/index.Rmd", knit_deck=TRUE)

# publish to github
# create an empty repo on github, then 
# replace USER and REPO with your repo details

setwd("inst/www/actg_deckdir")
publish(user = "mskyttner", repo = "actg-barcode") 
setwd("~/rbarcode")

require("opencpu")
opencpu$start(12345)
opencpu$view("/library/rbarcode/www")
opencpu$stop()

