#'@title Barcode Stacked area plot
#'
#'@description Creates a stacked area plot over time for the top clones in your data set.
#'
#'@param your_data A data frame. Usually individual barcodes in rows and samples in columns.
#'@param samples A vector of the sample names you wish to include, in time order.
#'@param time The numeric time points associated with samples, in ascending order.
#'@param n_clones The number of clones to display.
#'@param title The title.
#'@param colors The color palette of the figure. Default is from the viridis package.
#'@param mode The type of stacked area plot. "PROP" is a proportioned stacked area plot, displaying the proportion of each barcode at the time points. "Plain" displays the pure counts of each barcode.
#'@param n n is only relevant if using a non default color palette and using more clones than the size of the palette. If n_clones is greater than the size of the palette, n should be the max size of the palette.
#'@return Displays stacked area plot in the current plot window.
#'@examples barcode_stacked(jd76,c("jd76_1m_grans.fastq","jd76_2m_grans.fastq","jd76_3m_grans.fastq"),c(1,2,3),20)
#'@export

barcode_stacked<-function(your_data,
                         samples,
                         time,
                         n_clones,
                         title=NULL,
                         colors=FALSE,
                         mode="PROP",
                         n=9){
  
  your_data=your_data[,samples]
  Sum=vector(length=nrow(your_data))
  list_of_combinded_files=NULL
  for(i in 1:nrow(your_data)){
    Sum[i]=sum(your_data[i,])
  }
  r=rank(-Sum)
  for(i in 1:nrow(your_data)){
    if(r[i] <= n_clones){
      temp=
        list_of_combinded_files[[n_clones-r[i]+1]]=as.data.frame(your_data[i,])
    }
  }
  titles=vector(length=n_clones);titles[]=seq(1,n_clones)
  for(i in 1:length(titles)){
    titles[i]=paste("Barcode ",toString(titles[i]))
  }
  
  time_points=length(samples)
  Libraries=rep(titles,times=time_points)
  Months=as.numeric(rep(time,each=n_clones))
  Barcode_count=vector(length=length(Libraries))
  for(i in 1:n_clones){
    for(j in 1:time_points){
      temp=list_of_combinded_files[[i]][,samples]
      Barcode_count[(j-1)*n_clones+i]=temp[,j]
    }
    
    stacked_data=(data.frame(Libraries,Months,Barcode_count))}
  colourCount=n_clones
  if(mode=="PROP"){
    prop=stacked_data$Barcode_count
    for(i in 1:time_points){
      prop[((i-1)*n_clones+1):((i-1)*n_clones+n_clones)]=prop[((i-1)*n_clones+1):((i-1)*n_clones+n_clones)]/sum(prop[((i-1)*n_clones+1):((i-1)*n_clones+n_clones)])
    }
    stacked_data$prop=prop
    if(colors==FALSE){
      require("viridis")
      ggplot(stacked_data, aes(x=stacked_data$Months, y=stacked_data$prop, fill=Libraries)) +
        geom_area()+
        scale_fill_viridis(discrete=TRUE)+
        theme(legend.position="none")+
        ggtitle(title)+
        xlab("Time (months)")+
        ylab("Percent Contribution of Barcode")
    }else{
      getPalette = colorRampPalette(RColorBrewer::brewer.pal(n, colors))
      ggplot(stacked_data, aes(x=stacked_data$Months, y=stacked_data$prop, fill=Libraries)) +
        geom_area()+
        scale_fill_manual(values = getPalette(colourCount))+
        theme(legend.position="none")+
        ggtitle(title)+
        xlab("Time (months)")+
        ylab("Percent Contribution of Barcode")
    }
  }
  else if(mode=="Plain"){
    if(colors==FALSE){
      require("viridis")
      ggplot(stacked_data, aes(x=stacked_data$Months, y=stacked_data$Barcode_count, fill=Libraries)) + 
        geom_area()+
        scale_fill_viridis(discrete=TRUE)+
        theme(legend.position="none")+
        ggtitle(title)+
        xlab("Time (months)")+
        ylab("Barcode Count")
    }else{
      getPalette = colorRampPalette(RColorBrewer::brewer.pal(n, colors))
      ggplot(stacked_data, aes(x=stacked_data$Months, y=stacked_data$Barcode_count, fill=Libraries)) + 
        geom_area()+
        scale_fill_manual(values = getPalette(colourCount))+
        theme(legend.position="none")+
        ggtitle(title)+
        xlab("Time (months)")+
        ylab("Barcode Count")
    }
  }
}
