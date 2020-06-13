######################################################
#Functions
######################################################

########################################
# Parse Vow data
########################################
Vowparse<- function(dat,dated,BR,DN = "",WR = "",SL,sqft = ""){
  
  ########
  #Variables use
  ########
  workingdat<-dat
  print("loaded dat vowparse")
  city <- "Toronto"
  
  ########
  #Output variables
  ########
  
  print(paste('Sale or Lease:', SL ))
  #  print(paste('The year:', YR ))
  #  print(paste('The Month:', MN ))
  print(paste('Bedrooms:', BR ))
  if(DN !="") {print(paste('Den:', DN ))}
  if(WR != ""){ print(paste('Washroom:', WR ))}
  if(sqft != ""){ print(paste('Square footage:',sqft)) }
  
  #print("The date", as.Date(dated[1],format = '%Y-%m-%d'),"to",as.Date(dated[2],format = '%Y-%m-%d') ,sep=" ")
  
  Group<-workingdat %>%
    {if (sqft!= "") mutate(.,Approx.Square.Footage = str_extract(Approx.Square.Footage,"[[:digit:]]*")) else .}%>%#replace sqft with first number
    {if (sqft!= "") mutate(.,Approx.Square.Footage = str_replace(Approx.Square.Footage,"^[0]","499")) else .}%>%#replace sqft 0 with 499
    {if (sqft!= "") mutate(.,Approx.Square.Footage = as.integer(Approx.Square.Footage)) else .}%>% #Change sqft to integer value
    mutate(Sold.Date = as.Date(Sold.Date))%>%#make R readable sold date
    mutate(Bedrooms.. = replace_na(Bedrooms..,0))%>%#remove NA from bedrooms+
    
    #Filter  
    filter(Area == city)%>%#filter based on city# show for sale only
    filter(.,Sold.Date >= dated[1] & Sold.Date <= dated[2]+month(1) - days(1))%>%#only show those between these two dates
    
    filter(Bedrooms== BR)%>%#filter based on number of bed rooms
    {if (sqft != "") filter(.,!is.na(Approx.Square.Footage)) else . }%>%#remove missing sqft
    {if (DN !="") filter(.,Bedrooms.. == DN) else .}%>%#filter based  on number of bedroom+
    {if (WR !="") filter(.,Washrooms == WR) else .}%>%#filter based  on number of washroom
    {if (sqft!= "") filter (., Approx.Square.Footage == sqft) else . }%>% #condition filter based on sqft
    filter(Sale.Lease == SL)%>%
    filter(!is.na(Community))%>%
    filter(!is.na(Sold.Price))%>%#remove unsold listings
    
    #Group data by neighbourhood
    
    group_by(Community) %>%#create group based on community
    summarise(avg_list = mean(List.Price),#get mean price of listing
              median_list = median(List.Price),#get median price of listing
              avg_sold = mean(Sold.Price),# get mean price sold
              median_sold = median(Sold.Price),#get median price sold
              avg_DOM = mean(Days.On.Market),#get mean days on market
              median_DOM = median(Days.On.Market),#get median days on market
              max = max(Sold.Price), #maximum sold price
              se = sd(Sold.Price)/sqrt(n()),
              min = min(Sold.Price), #Minimum sold price
              Total = n()) %>%#get total number of listing in each community
    arrange(desc(Total))%>%#arrange by number of listing
    inner_join(communitylist)
  
  return(Group)
}


########################################


