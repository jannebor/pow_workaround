library(stringr)
library(taxize)
library(rvest)

#species name
#type one of "Native", "Introduced", "Uncertain"
pow_wgsrpd <-function(species, type){
  
  ppow<-get_pow(species, accepted = TRUE, rows = 1, messages=FALSE)
  
  if(!is.na(ppow[1])){
    
    ppow_data<-pow_lookup(ppow[1])
    suppressWarnings(url<-html(paste("http://plantsoftheworldonline.org/taxon/",ppow[1],sep="")))

    selector.type<-"#distribution-listing > h4:nth-child(1)"
    fype<-html_nodes(x=url, css=selector.type) %>%
      html_text(trim=TRUE)
    if((length(grep(tolower(type),tolower(fype)))>0)){
      selector.name<-"#distribution-listing > p:nth-child(2)"
      
      fnames<-html_nodes(x=url, css=selector.name) %>%
        html_text(trim=TRUE)
    } else { 
      selector.type<-"#distribution-listing > h4:nth-child(3)"
      fype<-html_nodes(x=url, css=selector.type) %>%
      html_text(trim=TRUE)
    if((length(grep(tolower(type),tolower(fype)))>0)){
      selector.name<-"#distribution-listing > p:nth-child(4)"
      
      fnames<-html_nodes(x=url, css=selector.name) %>%
        html_text(trim=TRUE)
    } else { selector.type<-"#distribution-listing > h4:nth-child(5)"
    fype<-html_nodes(x=url, css=selector.type) %>%
      html_text(trim=TRUE)
    if((length(grep(tolower(type),tolower(fype)))>0)){
      selector.name<-"#distribution-listing > p:nth-child(6)"
      
      fnames<-html_nodes(x=url, css=selector.name) %>%
        html_text(trim=TRUE)
    }
    }
    }
    
    if(length(fnames)>0){
      
      
      #distribution-listing > p:nth-child(2)
      fnames<-gsub("\n","", fnames)
      fnames<-gsub(" ","", fnames)
      fnames<-str_split(fnames, pattern=",")
      fnames<-unlist(fnames)
      
      if(length(fnames)>0){
        for (t in 1:length(fnames)) {
          
          if (fnames[t]=="Panamá"){
            fnames[t]<-"Panama"
            
          }
          if (fnames[t]=="NorthwestTerritorie"){
            fnames[t]<-"NorthwestTerritori"
            
          }
          
          
          
        }
        
        return(fnames)
        
      }
    }
  }
}
