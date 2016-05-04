source(paste(githubDir,'geoCodeAPI.R',sep=""))

unresolvedTrackingIDs = NULL
data$Lat = data$Lng = NA
for(i in 1:NROW(data))
{
	LatLng = point(	addr1 = data$Addr1[i], 
					addr2 = data$Addr2[i], 
					pincode = data$Pincode[i],
					city = data$City[i], 
					state = data$State[i])	
	if(!is.null(LatLng))
	{
		data$Lat[i] = LatLng$lat
		data$Lng[i] = LatLng$lng
	}
	if(is.null(LatLng))
		unresolvedTrackingIDs = c(unresolvedTrackingIDs,data$order_external_id[i])
}

latLngData =data[!is.na(data$Lng) & !is.na(data$Lat),names(data) %in% c("order_external_id","Lng","Lat")]
latLngData$order_external_id = as.character(latLngData$order_external_id)
## Including the Hub Lat Lng to calculate the Time Matrix
latLngData = rbind(c('Hub',hubLatLng),latLngData)

### Calculation of Time Matrix
timeMatrix = matrix(data = 0, nr = NROW(latLngData), nc = NROW(latLngData))
for(i in 1:(NROW(latLngData)-1))
	for(j in (i+1):NROW(latLngData))
		timeMatrix[i,j] = timeMatrix[j,i] = time(point1 = as.list(latLngData[i,-1]),point2 = as.list(latLngData[j,-1]))

colnames(timeMatrix) = latLngData$order_external_id
rownames(timeMatrix) = latLngData$order_external_id

## Keeping the Backup and applying cut off Time as 2 hours
timeMatrixOriginal = timeMatrix
timeMatrix[timeMatrix > 2] = 2

#Load Calculation based on LBH
loadData = data %>%
           transform(.,Length = as.numeric(as.character(Length)), Width = as.numeric(as.character(Width)),Height = as.numeric(as.character(Height))) %>%
			mutate(.,Volume =  Length*Height*Width/12^3) %>%
			dplyr::select(.,order_external_id,Volume)

slotData = dplyr::select(data,order_external_id,Slot_Start_Time,Slot_End_Time )
save(list=ls(),file = paste('optimizationData ',unlist(strsplit(fileName,"\\."))[1],'.RData',sep=""))
