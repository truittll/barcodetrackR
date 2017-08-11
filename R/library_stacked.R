#'@title Library Stacked area plot
#'
#'@description Creates a stacked area plot over time for the total counts for all barcodes in a given library in your data set.
#'
#'@param list_of_libraries A list of data frames. Data frames usually have individual barcodes in rows and samples in columns.
#'@param library_titles The names of each library used
#'@param samples A vector of the sample names you wish to include, in time order.
#'@param time The numeric time points associated with samples, in ascending order.
#'@param title The title.
#'@param colors The color palette of the figure. Default is from the viridis package.
#'@param mode The type of stacked area plot. "PROP" is a proportioned stacked area plot, displaying the proportion of each barcode at the time points. "Plain" displays the pure counts of each barcode.
#'@param n n is only relevant if using a non default color palette and using more clones than the size of the palette. If n_clones is greater than the size of the palette, n should be the max size of the palette.
#'@param Direction The direction of the color palette used. Appropriate inputs are +1 or -1.
#'@return Displays stacked area plot in the current plot window.
#'@examples library_stacked(list(lib7_jd76,lib11_jd76),c("Library 7","Library 11"),c("jd76_1m_grans.fastq","jd76_2m_grans.fastq","jd76_3m_grans.fastq"),c(1,2,3),20)
#'@export

library_stacked=function(list_of_libraries,
                         library_titles,
                         samples,
                         time,
                         title=NULL,
                         colors=FALSE,
                         mode="PROP",
                         n=9,
                         Direction=1){
  time_points=length(samples)
  numb_lib=length(list_of_libraries)
  Libraries=rep(library_titles,times=time_points)
  Months=as.numeric(rep(time,each=numb_lib))
  Barcode_count=vector(length=length(Libraries))
  for(i in 1:numb_lib){
    for(j in 1:time_points){
      temp=list_of_libraries[[i]][,samples]
      Barcode_count[(j-1)*numb_lib+i]=sum(temp[,j])
    }
    your_data=(data.frame(Libraries,Months,Barcode_count))
  }
  
  if(mode=="PROP"){
    prop=your_data$Barcode_count
    for(i in 1:time_points){
      prop[((i-1)*numb_lib+1):((i-1)*numb_lib+numb_lib)]=prop[((i-1)*numb_lib+1):((i-1)*numb_lib+numb_lib)]/sum(prop[((i-1)*numb_lib+1):((i-1)*numb_lib+numb_lib)])
    }
    your_data$prop=prop
    if(colors==FALSE){
      ggplot(your_data, aes(x=your_data$Months, y=your_data$prop, fill=Libraries)) +
        geom_area()+
        scale_fill_viridis(discrete=TRUE)+
        xlab("Time (months)")+
        ylab("Percent Library of Total Barcodes")+
        ggtitle(title)
      
    }else{
      ggplot(your_data, aes(x=your_data$Months, y=your_data$prop, fill=Libraries)) +
        geom_area()+
        scale_fill_brewer(palette = colors,direction=Direction)+
        ggtitle(title)+
        xlab("Time (months)")+
        ylab("Percent Library of Total Barcodes")
    }
  }
  else if(mode=="Plain"){
    if(colors==FALSE){
      ggplot(your_data, aes(x=your_data$Months, y=your_data$Barcode_count, fill=Libraries)) + 
        geom_area()+
        scale_fill_viridis(discrete=TRUE)+
        xlab("Time (months)")+
        ylab("Total Barcodes per Library")+
        ggtitle(title)
      
    }else{
      ggplot(your_data, aes(x=your_data$Months, y=your_data$Barcode_count, fill=Libraries)) + 
        geom_area()+
        scale_fill_brewer(palette = colors,direction=Direction)+
        ggtitle(title)+
        xlab("Time (months)")+
        ylab("Total Barcodes per Library")
    }
  }
}