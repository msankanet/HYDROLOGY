

ui <- fluidPage(
  textInput("mainfolder", "What is the path of the folder ?","D:/CCmodels/"),
    textInput("sourceofstations_path", "What is the path of the station data file (a CSV file) ?","C:/Users/Hasala/Downloads/Book2.csv"),
    # CODE BELOW: Display the text output, greeting
    # Make sure to add a comma after textInput()

	   actionButton("go", "Go")
	 
)

server <- function(input, output) {
    # CODE BELOW: Render a text output, greeting
    
	
	
    #output$sourceofstations_path <- renderText({input$sourceofstations_path})
    
	   # output$mainfolder <- renderText({input$mainfolder})
	
observeEvent(input$go, {
library(tidyverse)
library(readxl)    
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
                      # Install stringr package
library("stringr")     

Thefolderz<-as.character(input$mainfolder) #Enter the folder path that climate change data are
folderslistmodels<- list.files(Thefolderz) #Getting the Climate Data from 

numberofModels=length(folderslistmodels)

sourceofstations<- read_csv(as.character(input$sourceofstations_path))
namelist=colnames(sourceofstations)
latlist=sourceofstations[1,]
lonlist=sourceofstations[2,]
latlist=as.matrix(latlist)
lonlist=as.matrix(lonlist)




#namelist<-c('muru','vavu','medawa','mihin','maradan','mahail','kandalama')
#latlist<-c(8.800,8.800,8.500,8.400,8.100,8.100,7.900)
#lonlist<-c(80.100,80.500,80.500,80.500,80.600,80.500,80.700)
#numberofModels

for (jj in 1:numberofModels){



foldersscens<-paste0(Thefolderz,folderslistmodels[jj],"/","prcp","/") #Getting the Climate Data from 
folderslistscenzz<- list.files(foldersscens) #Getting the Climate Data from 
NumberofScenarios<-length(folderslistscenzz)
numberofStations<-length(latlist)

for (jjj in 1:NumberofScenarios){

janka3 <- data.frame(matrix(ncol = 0, nrow = 0))

fileslistncdf<- list.files(paste0(foldersscens,folderslistscenzz[jjj],"/"),pattern = "\\.nc$") #Getting the Climate Data from 




for (jjjj in 	1:length(fileslistncdf)){


climate_filepath <- paste0(foldersscens,folderslistscenzz[jjj],"/",fileslistncdf[jjjj])


climate_output <- nc_open(climate_filepath)



if (jjjj==1){

tas_time <- nc.get.time.series(climate_output, v = "pr", time.dim.name = "time")
tas_time_first<-format(tas_time[c(1:1)], "%Y%m%d")






}


if (jjjj==length(fileslistncdf)){

tas_time <- nc.get.time.series(climate_output, v = "pr", time.dim.name = "time")
tas_time_last<-format(tas_time[c(1:1)], "%Y%m%d")

}

datesz=seq(as.Date(tas_time_first,"%Y%m%d"), as.Date("2014-12-31"), by="days")

datesz[month(datesz)==2 && day(datesz)==29]







lon <- ncvar_get(climate_output, varid = "lon")
lat <- ncvar_get(climate_output, varid = "lat")

tas_time <- nc.get.time.series(climate_output, v = "pr",
                               time.dim.name = "time")


lon_index <- which.min(abs(lon - lonlist[1]))
lat_index <- which.min(abs(lat - latlist[1]))


tas_test <- nc.get.var.subset.by.axes(climate_output, "pr",
                                 axis.indices = list(X = lon_index,
                                                     Y = lat_index))

#create data frame with 0 rows and 5 columns
janka <- data.frame(matrix(ncol = 0, nrow = length(tas_test)))



for (ij in 1:numberofStations){

lon_index <- which.min(abs(lon - lonlist[ij]))
lat_index <- which.min(abs(lat - latlist[ij]))


tas <- nc.get.var.subset.by.axes(climate_output, "pr",
                                 axis.indices = list(X = lon_index,
                                                     Y = lat_index))
													 
													 
janka2=data.frame(  tas = as.vector(tas)) 
		
janka=cbind(janka,janka2)


}
janka3=rbind(janka3,janka)



			

}
setwd(paste0(foldersscens,folderslistscenzz[jjj],"/"))

tas_time_hold<-as.Date("19000101", "%Y%m%d")

 date_diff <- tas_time_hold- as.Date(as.character(tas_time_first), format="%Y%m%d")

 date_diff <-as.double(date_diff)

if (date_diff >0){

 janka2222=janka3[date_diff:dim(janka3)[1],]
 tas_time_first="19000101"
 
 }else{
  janka2222=janka3
 
}

for (iji in 1:3){

janka44=as.numeric(unlist(janka2222[iji]))
janka44=as.numeric(format(round(janka44*86400, 2), nsmall = 2, scientific = FALSE))
janka55=c(tas_time_first,janka44)
write.table(janka55, paste0(namelist[iji],".txt"), append = FALSE,quote = FALSE, sep="\t", dec = ".",
            row.names = FALSE, col.names = FALSE)

}

}
}

  }
  )  
}

shinyApp(ui = ui, server = server)


