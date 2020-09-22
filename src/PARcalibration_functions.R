#Helper functions for brew and for analysis

include_graph <- function(width = 1, filename) {
       paste("\\includegraphics[width=", width, "\\linewidth]{",filename, "}", sep = "")
}



filename <- function(y,sensor_id){
  paste('graphs/', sensor_id, y, '.pdf', sep = '')
}
  
  
  
#Import the dataset

data_import<-function(file){
  data<-read.table(file,skip=4,sep=',',dec='.',header=F, na.strings ="NAN")
  colnames(data)<-colnames(read.table(file,skip=1,sep=',',dec='.',header=T, na.strings ="NAN"))
  data<-data[complete.cases(data),]
  raw_records<-as.xts(read.zoo(data))
  return(raw_records)
}

# Data preparation function
#Prefix= PAR_mv,DiffV.
data_preparation<-function(xts.df,references){
  # Remove the first and last 30 minutes to aovid disturbance during setup
  start.date = start(xts.df)+minutes(30)
  end.date = end(xts.df)-minutes(30)
  records.clip=xts.df[paste(start.date,end.date,sep="::")]
   #Aggregate the observation of 5 minutes
  ends <-endpoints(records.clip,'minutes',5)
  records_10minutes<-period.apply(records.clip,ends,mean,na.rm=T)
  #Convert to data frame
  records<-as.data.frame(records_10minutes)
  #records$TimeStamp<-rownames(records)
  # Average the two Li-COR reference sensors
  #records$reference=rowMeans(records[,references])
  return(records)  
}

# Remove noise from dark current
remove_noise<-function(df,sensor_id){
  out<-df[which(df[,sensor_id]>0.01),]
  return(out)
}


#Create scatterplot of reference and sensor
scat_plot<-function(df,sensor_id,reference='reference'){
  p1<-ggplot(df,aes_string(x=sensor_id,y=reference))+
    geom_point(shape=1)+
    geom_smooth(method=lm)+
    xlab('Current (mV) AWF Sensor')+
    ylab(expression(paste('PPFD (',mu,'mol ', m^{-2},s^{-1},') LI-COR')))+
    ggtitle(sensor_id)
  return(p1)
}

#Create diurnal variation plot for the reference and the predicted PAR value
pred_plot<-function(df,sensor_id, coeff){
  df$predicted<-df[,sensor_id]*coeff
  df$timestamp<-as.POSIXct(df$timestamp)
  day.lon<-melt(df, id.vars = "timestamp",value.name = 'Variable',variable.name = 'Sensor')
  sub<-day.lon[which(day.lon$variable%in%c('predicted','reference')),]
  group.colors <- c(predicted = "red", reference = "blue")
  p1<-ggplot(sub, aes(timestamp,value, colour=variable, group=1)) + 
    geom_line() +
    scale_color_manual(values=group.colors)+
    #geom_point(color='grey')+
    xlab("Time") +
    scale_x_datetime(labels = date_format("%H-%M"), date_breaks = "1 hours")+
    ylab(expression(paste('PPDF (',mu,'mol ', m^{-2},s^{-1},')')))+
    scale_y_continuous()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p1)
}
