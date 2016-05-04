timeMatrixCheck = function(timeMatrix,data,latLngData,timeCutOff)
{
  OID = colnames(timeMatrix)[-1]
  colNames = c("Addr1","Addr2","Pincode","City","State")
  
  for(i in OID)
  {
    if(timeMatrix['Kudlu_Hub',i] > timeCutOff)
    {
      tempAddr = data[data$order_external_id %in% i,colNames]
      tempAddr = tempAddr[, !apply(tempAddr,2, function(x) any(is.na(x)))]
      
      
      URL = URLencode(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",
                            paste(apply(tempAddr,1,as.character),collapse = ",",sep = ""),
                  "&key=AIzaSyCCX84qpO8mT_ig8RpEDlW9dcuiHPYBxL0",sep = ""))
      URL = gsub("\\#","",URL)
      R = GET(URL) %>%
          content(., encoding = "UTF-8")
      if(length(R$results) != 0)
      {
        if(!is.null(R$results[[1]]$geometry$location))
        {
          tempLatLng = list(Lat = R$results[[1]]$geometry$location$lat,
                            Lng = R$results[[1]]$geometry$location$lng)
          
          if(time(point1 = list(Lng = 77.6533668,Lat = 12.8852659),
                  point2 = tempLatLng) < timeMatrix['Kudlu_Hub',i])
          {
            data$Lat[data$order_external_id %in% i] = latLngData$Lat[data$order_external_id %in% i] = R$results[[1]]$geometry$location$lat
            data$Lng[data$order_external_id %in% i] = latLngData$Lng[data$order_external_id %in% i] =R$results[[1]]$geometry$location$lng
            
            newTime = NULL
            LatLngList = apply(latLngData[,c("Lat","Lng")],1,as.list)
            for (l in 1:length(LatLngList)) 
            {
              newTime = c(newTime,time(point2 = as.list(data[data$order_external_id %in% i,c("Lat","Lng")]),
                                       point1 = LatLngList[[l]]))
            }
            
            print(length(newTime))
            timeMatrix[i,] = newTime
            timeMatrix[,i] = newTime
            
            
          }
        } 
      }
    }
  }
  
  return(list(data = data,latLngData = latLngData,timeMatrix = timeMatrix))
}