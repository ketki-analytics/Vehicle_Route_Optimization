LatLngSanityCheck = function(latLngData,data)
{
  for(i in 2:NROW(latLngData))
  {
    t_Hub_j = time(point1 = as.list(latLngData[1,-1]),point2 = as.list(latLngData[i,-1]))
    if(t_Hub_j > 2)
    {
      pincode = data %>% filter(.,order_external_id %in% as.character(latLngData[i,1])) %>% select(.,Pincode)
      pincode = as.character(pincode[1,1])
      R = LatLngFromPincode(pincode)
      R$Lng = as.character(R$Lng)
      R$Lat = as.character(R$Lat)
      t_Hub_j_new = time(point1 = as.list(latLngData[1,-1]),point2 = R)
      if(t_Hub_j_new < t_Hub_j)
      {
        latLngData[i,"Lng"] = R$Lng
        latLngData[i,"Lng"] = R$Lat
      }
    }
  }
  return(latLngData)
}