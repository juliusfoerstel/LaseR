## Parameter


#von Jenny
lambda234 = 2.8263e-06;  # decay constant U234
lambda238 = 1.55125e-10;  # decay constant U238
lambda230 = 9.1577e-06;# decay constant Th230
lambda232 = 4.9475e-11; # decay constant Th232

# Isotopic masses
# Source: http://physics.nist.gov/cgi-bin/Compositions/stand_alone.pl

m30 <- 230.0331341
m32 <- 232.0380558
m34 <- 234.0409523
m35 <- 235.0439301
m38 <- 238.0507884


#natural ratios
ratio85 <- 137.881


#resistor conversion
res11<- 1/(1.6*10^(-19))/10^11


#NIST 612 values
nist.th <- 37.23
nist.th.e <- 0.72
nist.u <- 37.15
nist.u.e <- 1.23

nist.ThU <- nist.th/nist.u
nist.ThU.e <- nist.ThU * sqrt((nist.u.e/nist.u)^2+(nist.th.e/nist.th)^2)