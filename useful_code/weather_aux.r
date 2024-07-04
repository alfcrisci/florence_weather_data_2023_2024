require(ggplot2)
require(RColorBrewer)
require(scales)
require(terra)



apply.hourly <- function(x, FUN, roundtime = "round", na.rm = TRUE){
  if(!is.xts(x)){
    stop("x must be an xts object")
  }
  
  if(!is.na(roundtime)){
    if(roundtime == "round"){
      time(x) <- round.POSIXt(terra::time(x), "hours")
    } else if(roundtime == "trunc"){
      terra::time(x) <- terra::trunc.POSIXt(time(x), "hours")
    } else {
      stop("roundtime must be either round or trunc")
    }
  }
  
  ap <- endpoints(x,'hours')
  if(na.rm){
    period.apply(x,ap,FUN, na.rm = TRUE)
  } else {
    period.apply(x,ap,FUN)
  }
  
}

month2season=function(x,mseq="DJF",label=F){
  result=x
  res=list()
  
  for ( i in 1:12) {
    res[[i]]=which(x==i)
  }
  
  labels=c(1,2,3,4)
  if(label==T) {labels=c("DJF","MAM","JJA","SON")}
  if((label==T) & (mseq=="JFM")) {labels=c("JFM","AMJ","JAS","OND")}
  
  if ( mseq=="DJF") {
    result[c(res[[12]],res[[1]],res[[2]])]=labels[1]
    result[c(res[[3]],res[[4]],res[[5]])]=labels[2]
    result[c(res[[6]],res[[7]],res[[8]])]=labels[3]
    result[c(res[[9]],res[[10]],res[[11]])]=labels[4]
  } 
  
  if ( mseq=="JFM") {
    result[c(res[[1]],res[[2]],res[[12]])]=labels[1]
    result[c(res[[3]],res[[4]],res[[5]])]=labels[2]
    result[c(res[[6]],res[[7]],res[[8]])]=labels[3]
    result[c(res[[9]],res[[10]],res[[11]])]=labels[4]
    
  }   
  
  return(result)
  
}


month_days=c(31,28,31,30,31,30,31,31,30,31,30,31)



# https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r


mround <- function(x, base){
base * round(x/base)
}

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

extentSpToPoly<-function (sp,proj="+init=epsg:4326")
                        {polysp <- as(extent(sp), "SpatialPolygons")
                         if (proj4string(sp) == proj) {proj4string(polysp) = CRS(proj)} else {proj4string(polysp) = proj4string(sp);polysp=spTransform(polysp,CRS(proj))};
                         df_pol=data.frame(ID=1)
                         rownames(df_pol)[1]=1
                         polysp=SpatialPolygonsDataFrame(polysp,df_pol)
                         return(polysp)
                        }     
  


extentToPoly<-function (vec,proj="+init=epsg:4326")
                       {polysp <- as(extent(vec), "SpatialPolygons")
                       proj4string(polysp) = CRS(proj)
                       df_pol=data.frame(ID=1)
                       rownames(df_pol)[1]=1
                       polysp=SpatialPolygonsDataFrame(polysp,df_pol)
                       return(polysp)
}     

windrose_table_data=function(windroseobj) {
  aa=na.omit(windroseobj$data)
  res=list()
  bb=rbind(tapply(aa$spd,aa$dir_binned,FUN = max),
          tapply(aa$spd,aa$dir_binned,FUN = mean),
          tapply(aa$spd,aa$dir_binned,FUN = median),
          tapply(aa$spd,aa$dir_binned,FUN = function(x) quantile(x,probs=c(0.95),na.rm=T)))
  bb=as.data.frame(bb)
  bb$ALL=rbind(max(aa$spd,na.rm=T),
               mean(aa$spd,na.rm=T),
               median(aa$spd,na.rm=T),
               quantile(aa$spd,probs=c(0.95),na.rm=T))
  res$table=bb
  
  if ( windroseobj$dirres==22.5) {
    
    names(res$table)=c("N","NNE","NE","ENE", "E","ESE", "SE","SSE", 
                       "S","SSW", "SW","WSW", "W","WNW","NW","NNW","ALL")
  }  
  
  if ( windroseobj$dirres==45) {
    
    
    names(res$table)==c("N","NE", "E","SE","S","SW","W","NW","ALL")
    
  }              
  
  rownames(res$table)=c("Max","Mean","Median","Q95")
  
  res$calm=data.frame(N_data=nrow(windroseobj$data),
                      Nmissing_data=windroseobj$missing_data,
                      frew_calms=windroseobj$calm_freq)
  
  return(res)  
  
}

extentSpToRaster<-function (sp,res_ini=100,value_ini=0,proj="+init=epsg:4326")
                {r <- raster(extent(sp))
                res(r)=resini
                values(r)=valueini
                if (proj4string(sp) == proj) {proj4string(r) = proj4string(sp)} else {proj4string(r) = proj4string(sp);r=projectRaster(r, crs=proj)};
                return(r)
                }     

extentToRaster<-function (vec,res_ini=100,value_ini=0,proj="+init=epsg:4326")
                {r <- raster(extent(vec))
                 res(r)=resini
                 values(r)=valueini
                 proj4string(r) = CRS(proj)
                 return(r)
}     


#r2 <- crop(r, extent(SPDF))
#r3 <- mask(r2, SPDF)

