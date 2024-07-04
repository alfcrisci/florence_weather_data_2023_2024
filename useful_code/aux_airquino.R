################################################################################
# Main references Airquino 

# A  https://www.snap4city.org/drupal/node/508
# B  https://github.com/n3d1117/airqino-calibration/

################################################################################
# call examples
# https://airqino-api.magentalab.it/getHourlyAvg/SMART181/2022-11-23/2022-11-24?pivot=true
# https://airqino-api.magentalab.it/getHourlyAvg/SMART181/2022-11-23/2022-11-24?pivot=false
# https://airqino-api.magentalab.it/getCurrentValues/SMART181
# https://airqino-api.magentalab.it/getSingleDay/SMART181/2022-11-24

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

mA_getrange_airquino=function(codestation="SMART61",dateini="2023-07-22",datefin="2023-07-24") {
  service='https://airqino-api.magentalab.it/getRange/'  
  respon=GET(paste0(service,codestation,'/',dateini,'/',datefin))
  tot_ls_airqino=content(respon)
  res=as.data.frame(do.call("rbind",tot_ls_airqino[[1]]))
  
}

mA_getHourlyAvg=function(codestation="SMART61",dateini="2023-07-22",datefin="2023-07-24",pivot=T) {
  service='https://airqino-api.magentalab.it/getHourlyAvg/'
  callhttp=paste0(service,codestation,'/',dateini,'/',datefin,'?pivot=false')
  if ( pivot == T) {
    callhttp=paste0(service,codestation,'/',dateini,'/',datefin,'?pivot=true')            
  }
  
  respon=GET(callhttp)
  tot_ls_airqino=content(respon)
  res=as.data.frame(do.call("rbind",tot_ls_airqino[[1]]))
  
}

mA_getCurrentValues=function(codestation="SMART61") {
  service='https://airqino-api.magentalab.it/getCurrentValues/'
  callhttp=paste0(service,codestation)
  respon=GET(callhttp)
  tot_ls_airqino=content(respon)
  res=as.data.frame(do.call("rbind",tot_ls_airqino[[1]]))
  
}

mA_getSingleDay=function(codestation="SMART61",dateini="2023-07-22") {
  service='https://airqino-api.magentalab.it/getSingleDay/'
  callhttp=paste0(service,codestation,'/',dateini)
  respon=GET(callhttp)
  tot_ls_airqino=content(respon)
  res=as.data.frame(do.call("rbind",tot_ls_airqino[[1]]))
  
}