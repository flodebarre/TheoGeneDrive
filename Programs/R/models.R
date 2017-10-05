# Clean memory
rm(list = ls())

# Load package to solve ODEs
require("deSolve")

# INITIALIZATIONS #########################
# Graphical parameters
maincol <- colERC <- rgb(243, 101, 35, maxColorValue = 255) # Same color as ERC logo
lwdp <- 2 # Line width
cexl <- 0.7 # text size

# Model parameters
prms <- c(s = 0.2, h = 0.5, c = 0.9)

tmax <- 50
times <- seq(0, tmax, by = 1) # Times for continuous time version
# Initial values
pDinit <- 10^(-6) # Initial proportion of drive in the population
yini  <- c(gDD = 0, g0D = 2*pDinit) # Genotypic frequencies

# MODEL DEFINITIONS ####################################
# ODEs, continuous time model, with conversion during meiosis
CDuringMeiosis <- function(time, state, pars) {
  with(as.list(c(state, pars)), {
    g00 <- 1 - gDD - g0D # genotype 00
    # Rates of gamete production
    FD <- (1-s)*gDD + 1/2*(1+c)*(1-h*s)*g0D
    F0 <- g00 + 1/2*(1-c)*(1-h*s)*g0D
    # Changes in genotype frequencies
    dgDD <- FD^2 - (FD+F0)^2*gDD
    dg0D <- 2*F0*FD - (FD+F0)^2*g0D
    
    return(list(c(dgDD, dg0D)))
  })
}

# Difference equations, when conversion takes place during meiosis
DDuringMeiosis <- function(pD, pars){
  with(as.list(c(pars)), {
    pDp <- pD^2*(1-s) + pD*(1-pD)*(1-h*s)*(1+c)
    p0p <- (1-pD)^2       + pD*(1-pD)*(1-h*s)*(1-c)
    return(c(pDp/(pDp+p0p)))
    })
}

# Difference equations, when conversion takes place at the embryon stage
DEmbryo <- function(pD, pars){
  with(as.list(c(pars)), {
    pDp <- pD^2*(1-s) + pD*(1-pD)*(1-s)*2*c + pD*(1-pD)*(1-h*s)*(1-c)
    p0p <- (1-pD)^2 + pD*(1-pD)*(1-h*s)*(1-c)
    return(c(pDp/(pDp+p0p)))
  })
}

# CALCULATIONS ###########################
# During meiosis, discrete time
pDDM <- rep(0, tmax)
pDDM[1] <- pDinit
for(i in 2:tmax){
  pDDM[i] <- DDuringMeiosis(pDDM[i-1], prms)
}
# Embryo, discrete time
pDE <- rep(0, tmax)
pDE[1] <- pDinit
for(i in 2:tmax){
  pDE[i] <- DEmbryo(pDE[i-1], prms)
}
# During meiosis, continuous time
pgDC   <- data.frame(ode(yini, times, CDuringMeiosis, prms))
pDC <- data.frame(cbind(time = pgDC$time, pD = pgDC$gDD + 1/2*pgDC$g0D))

# PLOTTING ##############################
# Common function to initialize plots to have the same parameters
initplot <- function(tit, xlabel="time (gen.)"){
  pdf(tit, width = 1.65, height =1.)
  par(mar = c(1.5, 1.5, 0.1, 0.5), las = 1, cex.axis = 0.9*cexl, mgp = c(0.65, 0.1, 0), tck = -0.015, cex.lab = cexl, bg = NA
  )
  # Empty plot
  plot(c(0,tmax), c(0,1), type = "n", xlab = "", ylab = "pD", frame.plot = FALSE, axes = FALSE)
  axis(1, pos = 0)
  axis(2, pos = 0)
  mtext(text = xlabel, side = 1, line = 0.5, cex = cexl)
  
}

# Discrete time, During Meiosis
initplot("Pics/DDM.pdf")
  lines(pDDM, lwd = lwdp, col = maincol, type = "l",  xlab = "time (gen.)")
dev.off()
system("xdg-open Pics/DDM.pdf")


initplot("Pics/DE.pdf")
lines(pDE, lwd = lwdp, col = maincol, type = "l",  xlab = "time (gen.)")
dev.off()
system("xdg-open Pics/DE.pdf")

initplot("Pics/CDM.pdf", "time")
lines(pDC$time, pDC$pD, lwd = lwdp, col = maincol, type = "l",  xlab = "time (gen.)")
dev.off()
system("xdg-open Pics/CDM.pdf")
