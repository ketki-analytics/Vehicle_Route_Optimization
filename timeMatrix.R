source(paste(githubDir,'geoCodeAPI.R',sep=""))
source(paste(githubDir,'LatLngSanityCheck.R',sep=""))
writeLines('Loading Source Codes are successful')
writeLines('####################################################################################################')
####################################################################################################
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
writeLines('Lattitude and Longitude calculation is done')
if(!is.null(unresolvedTrackingIDs))
	writeLines(paste('Unresolved Tracking IDs ->',paste(unresolvedTrackingIDs,collapse = ",")))
writeLines('####################################################################################################')
## Including the Hub Lat Lng to calculate the Time Matrix
latLngData = rbind(c('Hub',hubLatLng),latLngData)
####################################################################################################
## Lat Lng Sanity check, if any ID is beyond 2hrs distance then validae with pincode
latLngData  = LatLngSanityCheck(latLngData,data)
writeLines('LatLng Sanity check is done')
writeLines('####################################################################################################')
####################################################################################################
### Calculation of Time Matrix
timeMatrix = matrix(data = 0, nr = NROW(latLngData), nc = NROW(latLngData))
writeLines('Time Matrix Iteration is starting')
for(i in 1:(NROW(latLngData)-1))
{
	for(j in (i+1):NROW(latLngData))
	{
		t_ij = try(time(point1 = as.list(latLngData[i,-1]),point2 = as.list(latLngData[j,-1])))
		if(!grepl("Error",t_ij) | !is.null(t_ij))
			timeMatrix[i,j] = timeMatrix[j,i] = t_ij
		else
			writeLines(paste('Time Matrix was not able to resolve for i =',i,'j=',j))
	}
}
writeLines('Time Matrix iteration is completed')
writeLines('####################################################################################################')
rm(t_ij)
colnames(timeMatrix) = latLngData$order_external_id
rownames(timeMatrix) = latLngData$order_external_id
####################################################################################################
## Keeping the Backup and applying cut off Time as 2 hours
timeMatrixOriginal = timeMatrix
timeMatrix[timeMatrix > 2] = 2

####################################################################################################
#Load Calculation based on LBH
writeLines('Load Data Calculation started')
loadData = data %>%
           transform(.,Length = as.numeric(as.character(Length)), Width = as.numeric(as.character(Width)),Height = as.numeric(as.character(Height))) %>%
			mutate(.,Volume =  Length*Height*Width/12^3) %>%
			dplyr::select(.,order_external_id,Volume)
writeLines(paste('Total Volume',sum(loadData$Volume)))
writeLines('####################################################################################################')
####################################################################################################
slotData = dplyr::select(data,order_external_id,Slot_Start_Time,Slot_End_Time )
save(list=ls(),file = paste('optimizationData ',unlist(strsplit(fileName,"\\."))[1],'.RData',sep=""))
writeLines(paste('Unique Slot Start Times',paste(unique(slotData$Slot_Start_Time),collapse=",")))
writeLines('####################################################################################################')
####################################################################################################
source(paste(githubDir,'runningOptimizationModel.R',sep=""))