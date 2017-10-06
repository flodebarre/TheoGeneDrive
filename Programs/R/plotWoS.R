# Clear memory
rm(list=ls())

# Load result exported from WOS
tab <- read.table("data/analyze.txt", sep="\t", header=TRUE)

# Load results for Articles, Letters and Proceedings
tabALP <- read.table("data/analyze_ALP.txt", sep="\t", header=TRUE)

# Categories are
# ARTICLE (126)			NEWS ITEM (8)			LETTER (6)			BOOK CHAPTER (5)
# REVIEW (26)			MEETING ABSTRACT (8)			EDITORIAL MATERIAL (6)			PROCEEDINGS PAPER (4)


# Graphical parameters
maincol <- colERC <- rgb(243, 101, 35, alpha = 255, maxColorValue = 255) # Same color as ERC logo, only transparent
col1 <- rgb(220, 32, 82, alpha = 150,  maxColorValue = 255) # Pink
col2 <- rgb(243, 157, 35, alpha = 150, maxColorValue = 255) # Yellow

lwdh <- 6 # width of bars
lendh <- 1 # line end of bars

cext <- 0.8 # size of text legend
dy <- 0.5 # distance between arrow head and bar
lna <- 0.1 # size of arrow head

# PLOT ####################################################
pdf("Pics/GeneDriveArticles.pdf", width = 4, height = 1.8)
par(las = 1, mgp = c(1.5, 0.5, 0),
    mar = c(2, 2.5, 0.2, 0), cex = 0.9, cex.axis = 0.8, cex.lab=0.8)
xmax <- max(tab$Publication.Years)
plot(tab$Publication.Years, tab$records, xlab = "", ylab = "Number of publications", 
     type = "n", axes = FALSE)
axis(1, pos = 0, at = seq(min(tab$Publication.Years), max(tab$Publication.Years)), labels = FALSE, tck = -0.02, lwd.ticks = 0.8)
axis(1, pos = 0, at = c(2017, seq(1990, 2015, by = 5)), lwd = 0, lwd.ticks = 1.5, tck = -0.03, las = 1)
xmin <- 1990
axis(2, pos = xmin)
mtext("Year", side = 1, line = 1, cex = 0.8)

# Grid lines
for(i in seq(10, 50, by = 10)){
  lines(c(xmin, xmax), rep(i, 2), lty = 3, col = gray(0.5))
}

# Bars for all types of publications
lines(tab$Publication.Years, tab$records, 
     type = "h", lwd = lwdh, lend = lendh, col = col2)
# Bars for articles, letters and proceeding papers
lines(tabALP$Publication.Years, tabALP$records, 
      type = "h", lwd = lwdh, lend = lendh, col = maincol)

# ADD LEGEND

# Position
y03 <- 10
x03 <- 1998
# Text
text(x03, y03, labels = "A. Burt suggested using homing
endonucleases for gene drives", cex = cext, pos =3)
# Arrow
arrows(x03, y03, 2003, tab[tab$Publication.Years==2003,"records"]+dy, length = lna)

x14 <- 2006
y14 <- 23
text(x14, y14, labels = "Esvelt et al. suggested using 
 CRISPR-Cas9 for gene drives", cex = cext, pos =3)
arrows(x14, y14, 2014, tab[tab$Publication.Years==2014,"records"]+dy, length = lna)

x15 <- 2012.5
y15 <- 35
text(x15, y15, labels = "First proofs of 
concept in the lab", cex = cext, pos =3)
arrows(x15, y15, 2015, tab[tab$Publication.Years==2015,"records"]+dy, length = lna)

dev.off()
system("xdg-open Pics/GeneDriveArticles.pdf")

