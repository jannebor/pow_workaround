library(stringr)
library(taxize)
library(rvest)

# takes the output of pow_wgsrpd as input
# format defines to destination format during conversion:
# one of: "Continent", "Continent code", "Sub continent", "Sub continent code",
# "Region", "isocode5", "Country", "isocode2"
wgsrpd_conversion <-function(wgsrpd_regions, format){
  
  if(length(wgsrpd_regions)>0){
    for (t in 1:length(wgsrpd_regions)) {
      
      if (wgsrpd_regions[t]=="Panam???"){
        wgsrpd_regions[t]<-"Panama"
      }
    }
    
    tdwg<-read.csv("https://raw.githubusercontent.com/jannebor/supporting_files/master/tdwg.csv")
    tdwg$ISO<-as.character(tdwg$ISO)
    
    tdwg$Continent<-gsub(" ","", tdwg$Continent)
    tdwg$Continent<-substr(tdwg$Continent, 1, 18)
    
    tdwg$Sub_cont<-gsub(" ","", tdwg$Sub_cont)
    tdwg$Sub_cont<-substr(tdwg$Sub_cont, 1, 18)
    
    tdwg$Region<-gsub(" ","", tdwg$Region)
    tdwg$Region<-substr(tdwg$Region, 1, 18)
    
    tdwg$Country<-gsub(" ","", tdwg$Country)
    tdwg$Country<-substr(tdwg$Country, 1, 18)
    
    
    sub_df<-subset(tdwg, tdwg$Country==wgsrpd_regions[1])
    if(nrow(sub_df)==0){
      sub_df<-subset(tdwg, tdwg$Region==wgsrpd_regions[1])
      if(nrow(sub_df)==0){
        sub_df<-subset(tdwg, tdwg$Sub_cont==wgsrpd_regions[1])
        if(nrow(sub_df)==0){
          sub_df<-subset(tdwg, tdwg$Continent==wgsrpd_regions[1])
        }
      }
    }
    
    
    
    
    if(length(wgsrpd_regions)>1){
      for (l in 2:length(wgsrpd_regions)){
        sub_add<-NA
        sub_add<-subset(tdwg, tdwg$Country==wgsrpd_regions[l])
        
        if(nrow(sub_add)<1){
          
          sub_add<-subset(tdwg, tdwg$Region==wgsrpd_regions[l])
          
          if(nrow(sub_add)<1){
            sub_add<-subset(tdwg, tdwg$Sub_cont==wgsrpd_regions[l])
            
            if(nrow(sub_add)<1){
              sub_add<-subset(tdwg, tdwg$Continent==wgsrpd_regions[l])
            }
          }
        }
        
        sub_df<-rbind(sub_df, sub_add)
        
      }
      
    }
    
    if(nrow(sub_df)>0){
      
      for (k in 1:nrow(sub_df)){
        
        if (sub_df$ISO[k]=="Na"){
          sub_df$ISO[k]<-"NA"
          
        }
      }
    }
    
    if(tolower(format)=="continent code"){
      country_list<-unique(sub_df$Cont_code)
    }
    if(tolower(format)=="continent"){
      country_list<-unique(sub_df$Continent)
    }
    if(tolower(format)=="sub continent code"){
      country_list<-unique(sub_df$Sub_cont_code)
    }
    if(tolower(format)=="sub continent"){
      country_list<-unique(sub_df$Sub_cont)
    }
    if(tolower(format)=="region"){
      country_list<-unique(sub_df$Region)
    }
    if(format=="isocode5"){
      country_list<-unique(sub_df$X5Letter)
    }
    if(tolower(format)=="country"){
      country_list<-unique(sub_df$Country)
    }
    if(format=="isocode2"){
      country_list<-unique(sub_df$ISO)
    }
    
    
    return(country_list)
    
  }
}