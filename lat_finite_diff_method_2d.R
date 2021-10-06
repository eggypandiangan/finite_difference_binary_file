#!/usr/bin/env Rscript
setwd(getwd())
cat("\f"); rm(list=ls()); 
library(maps)
library(plotrix)
#plot u,v
fu<-file("u_jan2013.bin","rb")
fv<-file("v_jan2013.bin","rb")
funfo<-file.info("u_jan2013.bin")
fvnfo<-file.info("v_jan2013.bin")
u1<-readBin(fu,numeric(), size=4, n=funfo$size, endian = "little");close(fu)
v1<-readBin(fv,numeric(), size=4, n=funfo$size, endian = "little");close(fv)
lon<-seq(90,150,0.25)
lat<-seq(-15,15,0.25)
u<-matrix(u1,nrow = length(lat),ncol = length(lon),byrow = TRUE)
v<-matrix(v1,nrow = length(lat),ncol = length(lon),byrow = TRUE)

#hitung divergensi/konvergensi
dx<-0.25*111000
dy<-dx
dudx<-u*NaN
dudx[1:(nrow(u)),2:(ncol(u)-1)]=(u[1:(nrow(u)),3:(ncol(u))]-u[1:(nrow(u)),1:(ncol(u)-2)])/(2*dx)
dudx[1:(nrow(u)),1]=(u[1:(nrow(u)),2]-u[1:(nrow(u)),1])/(dx);
dudx[1:(nrow(u)),ncol(u)]=(u[1:(nrow(u)),ncol(u)]-u[1:(nrow(u)),(ncol(u)-1)])/(dx);
dvdy<-v*NaN
dvdy[2:(nrow(v)-1),1:(ncol(v))]=(v[3:(nrow(v)),1:(ncol(v))]-v[1:(nrow(v)-2),1:(ncol(v))])/(2*dy)
dvdy[1,1:(ncol(v))]=(v[2,1:(ncol(v))]-v[1,1:(ncol(v))])/(dy)
dvdy[nrow(v),1:(ncol(v))]=(v[nrow(v),1:(ncol(v))]-v[(nrow(v)-1),1:(ncol(v))])/(dy)
divg<-dudx + dvdy;
# dev.new()
svg("Divergensi.svg")
filled.contour(lon,lat,t(divg),xlim = c(118,122),ylim = c(-6,-1),plot.axes = {c(map("world"
      ,col = "black", add = T),map.axes(),vectorField(u,v,lon,lat,scale = 0.5,
      headspan = 0.7))},color.palette = rainbow, plot.title = title(main="Divergensi/Konvergensi",
      ylab = "Latitude", xlab = "Longitude"))
dev.off()

#hitung vortisitas
dvdx<-v*NaN
dvdx[1:(nrow(v)),2:(ncol(v)-1)]=(v[1:(nrow(v)),3:(ncol(v))]-v[1:(nrow(v)),1:(ncol(v)-2)])/(2*dx)
dvdx[1:(nrow(v)),1]=(v[1:(nrow(v)),2]-v[1:(nrow(v)),1])/(dx);
dudx[1:(nrow(v)),ncol(v)]=(v[1:(nrow(v)),ncol(v)]-v[1:(nrow(v)),(ncol(v)-1)])/(dx);
dudy<-u*NaN
dudy[2:(nrow(u)-1),1:(ncol(u))]=(u[3:(nrow(u)),1:(ncol(u))]-u[1:(nrow(u)-2),1:(ncol(u))])/(2*dy)
dudy[1,1:(ncol(u))]=(u[2,1:(ncol(u))]-u[1,1:(ncol(u))])/(dy)
dudy[nrow(u),1:(ncol(u))]=(u[nrow(u),1:(ncol(u))]-v[(nrow(u)-1),1:(ncol(u))])/(dy)
zeta<-dvdx-dudy
# dev.new()
svg("Vortisitas.svg")
filled.contour(lon,lat,t(zeta),xlim = c(118,122),ylim = c(-6,-1),plot.axes = {c(map("world"
      ,col = "black", add = T),map.axes(),vectorField(u,v,lon,lat,scale = 0.5,
      headspan = 0.7))},color.palette = rainbow, plot.title = title(main="Vortisitas",
      ylab = "Latitude", xlab = "Longitude"))
dev.off()
