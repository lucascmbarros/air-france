library(readr)
Air_France_Case <- read_csv("R/Business Cases/Air France Case Spreadsheet Supplement.csv")
View(Air_France_Case)

as_df <- as.data.frame(Air_France_Case)

summary(as_df)
typeof(as_df$`Match Type`)




## creating category for Match Type
for (i in 1:nrow(as_df)) {
  if (as_df$`Match Type`[i] == "Advanced") {as_df$`Match Type`[i] <- 5}
  else if (as_df$`Match Type`[i] == "Broad") {as_df$`Match Type`[i] <- 2}
  else if (as_df$`Match Type`[i] == "Exact") {as_df$`Match Type`[i] <- 4}
  else if (as_df$`Match Type`[i] == "Standard") {as_df$`Match Type`[i] <- 3}
  else {as_df$`Match Type`[i] <- 1} 
}
as_df$`Match Type` <- as.numeric(as_df$`Match Type`)


## Creating category for Status
for (i in 1:nrow(as_df)) {
  if (as_df$Status[i] == "Unavailable") {as_df$Status[i] <- "1"}
  else if (as_df$Status[i] == "Paused") {as_df$Status[i] <- "2"}
  else if (as_df$Status[i] == "Live") { as_df$Status[i] <- "3"}
  else if (as_df$Status[i] == "Sent") { as_df$Status[i] <- "4"}
  else if (as_df$Status[i] == "Deactivated") { as_df$Status[i] <- "0"}
}
as_df$Status <- as.numeric(as_df$Status)


as_df$Amount <- gsub('[,]','',as_df$Amount)
as_df$Amount <- as.numeric(as_df$Amount)
typeof(as_df$Amount)
as_df$ROA<- as.numeric(as_df$ROA)


as_df$`Total Cost` <- gsub('[,]','',as_df$`Total Cost`)
as_df$`Total Cost`<- as.numeric(as_df$`Total Cost`)
typeof(as_df$`Total Cost`)

as_df$`Avg. Cost per Click` <- gsub('[$]','',as_df$`Avg. Cost per Click`)
as_df$`Avg. Cost per Click` <- as.numeric(as_df$`Avg. Cost per Click`)
typeof(as_df$`Avg. Cost per Click`)

as_df$`Trans. Conv. %` <- gsub('[%]','',as_df$`Trans. Conv. %`)
as_df$`Trans. Conv. %`<- as.numeric(as_df$`Trans. Conv. %`)
typeof(as_df$`Trans. Conv. %`)


as_df$`Search Engine Bid` <- as.numeric(as_df$`Search Engine Bid`)
typeof(as_df$`Search Engine Bid`)

numeric_cols<- data.frame(as_df$`Match Type`, as_df$Clicks, as_df$Amount, as_df$`Total Volume of Bookings`, as_df$Profit, as_df$`Total Cost`, 
                          as_df$`Trans. Conv. %`, as_df$`Avg. Cost per Click`, as_df$`Search Engine Bid`)
correlation_matrix<- cor(numeric_cols)
col<- colorRampPalette(c("yellow2","goldenrod","darkred"))(20)
heatmap(correlation_matrix, col=col, symm=TRUE)

summary(as_df$`Match Type`)

### Creating new datafromes by publisher name
google_global <- as_df[as_df$`Publisher Name` =="Google - Global", ]
google_us <- as_df[as_df$`Publisher Name` =="Google - US", ]
msn_global <- as_df[as_df$`Publisher Name` =="MSN - Global", ]
msn_us <- as_df[as_df$`Publisher Name` =="MSN - US", ]
overture_global <- as_df[as_df$`Publisher Name` =="Overture - Global", ]
overture_us <- as_df[as_df$`Publisher Name` =="Overture - US", ]
yahoo_us <- as_df[as_df$`Publisher Name` =="Yahoo - US", ]

google_global$ROA <- google_global$Amount/google_global$`Total Cost`
ROA_google_global<-mean(google_global$ROA)

google_us$ROA <- google_us$Amount/google_us$`Total Cost`
for (i in 1:nrow(google_us)) {
  if (google_us$ROA[i] == Inf) {google_us$ROA[i] <- 0}}
ROA_google_US<- mean(google_us$ROA)

msn_global$ROA <- msn_global$Amount/msn_global$`Total Cost`
ROA_msn_global<- mean(msn_global$ROA)

msn_us$ROA <- msn_us$Amount/msn_us$`Total Cost`
ROA_msn_us<-mean(msn_us$ROA)

overture_global$ROA <- overture_global$Amount/overture_global$`Total Cost`
ROA_overture_global<-mean(overture_global$ROA)

overture_us$ROA <- overture_us$Amount/overture_us$`Total Cost`
ROA_overture_us<-mean(overture_us$ROA)

yahoo_us$ROA <- yahoo_us$Amount/yahoo_us$`Total Cost`
ROA_yahoo_us<- mean(yahoo_us$ROA)

ROA_kayak <- (233694/3567.13)

ROA_per_publisher<- c(ROA_google_global,ROA_google_US, ROA_msn_global, ROA_msn_us, ROA_overture_global, ROA_overture_us, 
                      ROA_yahoo_us, ROA_kayak)

ROA_per_publisher2<- c(ROA_google_global,ROA_google_US, ROA_msn_global, ROA_msn_us, ROA_overture_global, ROA_overture_us, 
                      ROA_yahoo_us)
mean(ROA_per_publisher2)
Publisher_name<- c("Google Global", "Google US", "MSN Global", "MSN US", "Overture Global", 
                   "Overture US", "Yahoo US", "Kayak")
new_table<- data.frame(Publisher_name,ROA_per_publisher)




Convertion_per_publisher<- c(mean(google_global$`Trans. Conv. %`), mean(google_us$`Trans. Conv. %`), 
                             mean(msn_global$`Trans. Conv. %`), mean(msn_us$`Trans. Conv. %`),
                             mean(overture_global$`Trans. Conv. %`), mean(overture_us$`Trans. Conv. %`),
                             mean(yahoo_us$`Trans. Conv. %`))
Average_costclick_bypublisher<- c(mean(google_global$`Avg. Cost per Click`), mean(google_us$`Avg. Cost per Click`), 
                                  mean(msn_global$`Avg. Cost per Click`), mean(msn_us$`Avg. Cost per Click`),
                                  mean(overture_global$`Avg. Cost per Click`), mean(overture_us$`Avg. Cost per Click`),
                                  mean(yahoo_us$`Avg. Cost per Click`))

SE_bid_bypublisher<- c(mean(google_global$`Search Engine Bid`), mean(google_us$`Search Engine Bid`), 
                        mean(msn_global$`Search Engine Bid`), mean(msn_us$`Search Engine Bid`),
                        mean(overture_global$`Search Engine Bid`), mean(overture_us$`Search Engine Bid`),
                        mean(yahoo_us$`Search Engine Bid`))
SE_bid_bypublisher<- SE_bid_bypublisher*10


Probability_booking<- c(mean(google_global$`Trans. Conv. %`/google_global$Impressions), mean(google_us$`Trans. Conv. %`)/mean(google_us$Impressions[-which(google_us$Impressions == 0)]),
                        mean(msn_global$`Trans. Conv. %`/msn_global$Impressions), mean(msn_us$`Trans. Conv. %`/msn_us$Impressions),
                        mean(overture_global$`Trans. Conv. %`/overture_global$Impressions), mean(overture_us$`Trans. Conv. %`/overture_us$Impressions),
                        mean(yahoo_us$`Trans. Conv. %`/yahoo_us$Impressions))

Publisher_name2<- c("Google Global", "Google US", "MSN Global", "MSN US", "Overture Global", 
                   "Overture US", "Yahoo US")
colour<- c("rgba(230, 42, 56, 0.3)", "rgba(76, 175, 80, 0.3)",
           "rgba(32, 169, 242, 0.3)", "rgba(255, 193, 7, 0.3)", "rgba(230, 42, 56, 0.3)", "rgba(76, 175, 80, 0.3)")
new_table2<- data.frame(Publisher_name2,Probability_booking,Average_costclick_bypublisher, SE_bid_bypublisher)
new_table3<- data.frame(Publisher_name2,Convertion_per_publisher,Average_costclick_bypublisher, ROA_per_publisher2)
new_table4<- data.frame(Publisher_name2,Convertion_per_publisher,Average_costclick_bypublisher, SE_bid_bypublisher)

#per campaign 
western_europe_detinations <- as_df[as_df$Campaign == "Western Europe Destinations", ]
geo_targeted_dc <- as_df[as_df$Campaign == "Geo Targeted DC", ]
air_france_brand_french_destinations <- as_df[as_df$Campaign =="Air FRance Brand & French Destinations" , ]
air_france_global_campaign<- as_df[as_df$Campaign == "Air France Global Campaign", ]
unassigned <- as_df[as_df$Campaign == "Unassigned", ]
geo_targeted_san_francisco <- as_df[as_df$Campaign == "Geo Targeted San Francisco", ]
air_france_branded <- as_df[as_df$Campaign == "Air France Branded", ]
geo_targeted_new_york <- as_df[as_df$Campaign == "Geo Targeted New York", ]
geo_targeted_miami <- as_df[as_df$Campaign == "Geo Targeted Miami", ]
geo_targeted_detroit <- as_df[as_df$Campaign == "Geo Targeted Detroit", ]
geo_targeted_boston<- as_df[as_df$Campaign == "Geo Targeted Boston", ]
geo_targeted_houston <- as_df[as_df$Campaign  == "Geo Targeted Houston", ]
paris_france_terms <- as_df[as_df$Campaign == "Paris & France Terms", ]
google_yearlong_2006<- as_df[as_df$Campaign == "Google_Yearlong2006", ]
geo_targeted_philadelphia <- as_df[as_df$Campaign == "Geo Targeted Philadelphia", ]
geo_targeted_chicago <- as_df[as_df$Campaign == "Geo Targeted Chicago", ]
geo_targeted_seattle <- as_df[as_df$Campaign == "Geo Targeted Seattle", ]
geo_targeted_los_angeles <- as_df[as_df$Campaign == "Geo Targeted Los Angeles", ]
french_destinations <- as_df[as_df$Campaign == "French Destinations", ]
geo_targeted_atlanta <- as_df[as_df$Campaign == "Geo Targeted Atlanta", ]
general_terms <- as_df[as_df$Campaign == "General Terms", ]
business_class <- as_df[as_df$Campaign == "Business Class", ]
outside_western_europe <- as_df[as_df$Campaign == "Outside Westtern Europe", ]
geo_targeted_cincinnati <- as_df[as_df$Campaign == "Geo Targeted Cincinnati", ]

western_europe_detinations$ROA <- (western_europe_detinations$Amount/western_europe_detinations$`Total Cost`)
ROA_western_europe_destinations <- mean(western_europe_detinations$ROA[-which(is.na(western_europe_detinations$ROA))])


campaigns_ROAs <- list("western europe destinations" = ROA_western_europe_destinations) #creating a list with the values
View(campaigns_ROAs)

geo_targeted_dc$ROA <- (geo_targeted_dc$Amount/geo_targeted_dc$`Total Cost`)
ROA_geo_tageted_dc <- mean(geo_targeted_dc$ROA[-which(is.na(geo_targeted_dc$ROA))])
campaigns_ROAs[["geo targeted dc"]] <- ROA_geo_tageted_dc #adding values to the list

air_france_global_campaign$ROA <- (air_france_global_campaign$Amount/air_france_global_campaign$`Total Cost`)
ROA_air_france_global_campaign <- mean(air_france_global_campaign$ROA[-which(is.na(air_france_global_campaign$ROA))])
campaigns_ROAs[["air france global campaign"]] <- ROA_air_france_global_campaign #adding values to the list

unassigned$ROA <- (unassigned$Amount/unassigned$`Total Cost`)
ROA_unassigned <- mean(unassigned$ROA[-which(is.na(unassigned$ROA))])
campaigns_ROAs[["unassigned"]] <- ROA_unassigned #adding values to the list

geo_targeted_san_francisco$ROA <- (geo_targeted_san_francisco$Amount/geo_targeted_san_francisco$`Total Cost`)
ROA_geo_targeted_san_francisco <- mean(geo_targeted_san_francisco$ROA[-which(is.na(geo_targeted_san_francisco$ROA))])
campaigns_ROAs[["geo targeted san francisco"]] <- ROA_geo_targeted_san_francisco #adding values to the list

air_france_branded$ROA <- (air_france_branded$Amount/air_france_branded$`Total Cost`)
ROA_air_france_branded <- mean(air_france_branded$ROA[-which(is.na(air_france_branded$ROA))])
campaigns_ROAs[["air france branded"]] <- ROA_air_france_branded #adding values to the list

geo_targeted_new_york$ROA <- (geo_targeted_new_york$Amount/geo_targeted_new_york$`Total Cost`)
ROA_geo_targeted_new_york <- mean(geo_targeted_new_york$ROA[-which(is.na(geo_targeted_new_york$ROA))])
campaigns_ROAs[["geo targeted new york"]] <- ROA_geo_targeted_new_york #adding values to the list

geo_targeted_miami$ROA <- (geo_targeted_miami$Amount/geo_targeted_miami$`Total Cost`)
ROA_geo_targeted_miami <- mean(geo_targeted_miami$ROA[-which(is.na(geo_targeted_miami$ROA))])
campaigns_ROAs[["geo targeted miami"]] <- ROA_geo_targeted_miami #adding values to the list

geo_targeted_detroit$ROA <- (geo_targeted_detroit$Amount/geo_targeted_detroit$`Total Cost`)
ROA_geo_targeted_detroit <- mean(geo_targeted_detroit$ROA[-which(is.na(geo_targeted_detroit$ROA))])
campaigns_ROAs[["geo targeted detroit"]] <- ROA_geo_targeted_detroit #adding values to the list

geo_targeted_boston$ROA <- (geo_targeted_boston$Amount/geo_targeted_boston$`Total Cost`)
for (i in 1:nrow(geo_targeted_boston)) {
  if (geo_targeted_boston$ROA[i] == Inf) {geo_targeted_boston$ROA[i] <- 0}}
ROA_geo_targeted_boston <- mean(geo_targeted_boston$ROA[-which(is.na(geo_targeted_boston$ROA))])
campaigns_ROAs[["geo targeted boston"]] <- ROA_geo_targeted_boston

geo_targeted_houston$ROA <- (geo_targeted_houston$Amount/geo_targeted_houston$`Total Cost`)
ROA_geo_targeted_houston <- mean(geo_targeted_houston$ROA[-which(is.na(geo_targeted_houston$ROA))])
campaigns_ROAs[["geo targeted houston"]] <- ROA_geo_targeted_houston #adding values to the list

air_france_brand_french_destinations$ROA <- (air_france_brand_french_destinations$Amount/air_france_brand_french_destinations$`Total Cost`)
ROA_air_france_brand_french_destinations <- mean(air_france_brand_french_destinations$ROA[-which(is.na(air_france_brand_french_destinations$ROA))])
campaigns_ROAs[["air france brand french destinations"]] <- ROA_air_france_brand_french_destinations #adding values to the list

paris_france_terms$ROA <- (paris_france_terms$Amount/paris_france_terms$`Total Cost`)
ROA_paris_france_terms <- mean(paris_france_terms$ROA[-which(is.na(paris_france_terms$ROA))])
campaigns_ROAs[["Paris & france terms"]] <- ROA_paris_france_terms #adding values to the list

google_yearlong_2006$ROA <- (google_yearlong_2006$Amount/google_yearlong_2006$`Total Cost`)
ROA_google_yearlong_2006 <- mean(google_yearlong_2006$ROA[-which(is.na(google_yearlong_2006$ROA))])
campaigns_ROAs[["google yearlong 2006"]] <- ROA_google_yearlong_2006 #adding values to the list

geo_targeted_philadelphia$ROA <- (geo_targeted_philadelphia$Amount/geo_targeted_philadelphia$`Total Cost`)
ROA_geo_targeted_philadelphia <- mean(geo_targeted_philadelphia$ROA[-which(is.na(geo_targeted_philadelphia$ROA))])
campaigns_ROAs[["geo targeted Philadelphia"]] <- ROA_geo_targeted_philadelphia #adding values to the list

geo_targeted_chicago$ROA <- (geo_targeted_chicago$Amount/geo_targeted_chicago$`Total Cost`)
ROA_geo_targeted_chicago <- mean(geo_targeted_chicago$ROA[-which(is.na(geo_targeted_chicago$ROA))])
campaigns_ROAs[["geo targeted chicago"]] <- ROA_geo_targeted_chicago #adding values to the list

geo_targeted_seattle$ROA <- (geo_targeted_seattle$Amount/geo_targeted_seattle$`Total Cost`)
ROA_geo_targeted_seattle <- mean(geo_targeted_seattle$ROA[-which(is.na(geo_targeted_seattle$ROA))])
campaigns_ROAs[["geo targeted seattle"]] <- ROA_geo_targeted_seattle #adding values to the list

geo_targeted_los_angeles$ROA <- (geo_targeted_los_angeles$Amount/geo_targeted_los_angeles$`Total Cost`)
ROA_geo_targeted_los_angeles <- mean(geo_targeted_los_angeles$ROA[-which(is.na(geo_targeted_los_angeles$ROA))])
campaigns_ROAs[["geo targeted los angeles"]] <- ROA_geo_targeted_los_angeles #adding values to the list

french_destinations$ROA <- (french_destinations$Amount/french_destinations$`Total Cost`)
ROA_french_destinations <- mean(french_destinations$ROA[-which(is.na(french_destinations$ROA))])
campaigns_ROAs[["french Destinations"]] <- ROA_french_destinations #adding values to the list

geo_targeted_atlanta$ROA <- (geo_targeted_atlanta$Amount/geo_targeted_atlanta$`Total Cost`)
ROA_geo_targeted_atlanta <- mean(geo_targeted_atlanta$ROA[-which(is.na(geo_targeted_atlanta$ROA))])
campaigns_ROAs[["geo targeted atlanta"]] <- ROA_geo_targeted_atlanta #adding values to the list

general_terms$ROA <- (general_terms$Amount/general_terms$`Total Cost`)
ROA_general_terms <- mean(general_terms$ROA[-which(is.na(general_terms$ROA))])
campaigns_ROAs[["general terms"]] <- ROA_general_terms#adding values to the list

business_class$ROA <- (business_class$Amount/business_class$`Total Cost`)
ROA_business_class <- mean(business_class$ROA[-which(is.na(business_class$ROA))])
campaigns_ROAs[["business class"]] <- ROA_business_class#adding values to the list

outside_western_europe$ROA <- (outside_western_europe$Amount/outside_western_europe$`Total Cost`)
ROA_outside_western_europe <- mean(outside_western_europe$ROA[-which(is.na(outside_western_europe$ROA))])
campaigns_ROAs[["outside western europe"]] <- ROA_outside_western_europe#adding values to the list

geo_targeted_cincinnati$ROA <- (geo_targeted_cincinnati$Amount/geo_targeted_cincinnati$`Total Cost`)
ROA_geo_targeted_cincinnati <- mean(geo_targeted_cincinnati$ROA[-which(is.na(geo_targeted_cincinnati$ROA))])
campaigns_ROAs[["geo targeted cincinnati"]] <- ROA_geo_targeted_cincinnati #adding values to the list

df_campaigns <- as.data.frame(campaigns_ROAs)
df_campaigns <- df_campaigns*100




library(plotly)

plot <- plot_ly(data = new_table2) %>% 
  add_trace(x = ~Convertion_per_publisher,
            y = ~Average_costclick_bypublisher,
            mode = 'markers',
            type = 'scatter',
            color = ~Publisher_name2,
            marker = list(
              color = ~colour,               
              opacity = 1,
              showlegend=T),
            size = ~size)


new_table2<- data.frame(Publisher_name2,Probability_booking,Average_costclick_bypublisher, SE_bid_bypublisher)
new_table2$SE_bid_bypublisher[3] <- 1
new_table2$SE_bid_bypublisher[4] <- 1






library(plotly)

new_table2<- data.frame(Publisher_name2,Probability_booking,Average_costclick_bypublisher, SE_bid_bypublisher)
new_table2$SE_bid_bypublisher[3] <- 1
new_table2$SE_bid_bypublisher[4] <- 1

p <- plot_ly(new_table2, x= ~Probability_booking, y= ~Average_costclick_bypublisher, text= ~Publisher_name2,type='scatter',
             mode= 'markers', marker=list(size= ~SE_bid_bypublisher, opacity= 0.7))%>%
            layout(title= 'Title',
                   xaxis= list(showgrid= FALSE),
                   yaxis= list(showgrid = FALSE))
p

p2 <- plot_ly(new_table2, x= ~Probability_booking, y= ~Average_costclick_bypublisher, text= ~Publisher_name2, type = 'scatter', mode = 'markers', color = ~SE_bid_bypublisher, colors = 'Greens',
             marker = list(size = ~SE_bid_bypublisher, opacity = 0.5)) %>%
         layout(title = 'Probability of booking VS Average Cost Per Click by Pubisher',
         xaxis = list(showgrid = T),
         yaxis = list(showgrid = T))
p2

p3<- plot_ly(new_table4, x= ~Convertion_per_publisher, y= ~Average_costclick_bypublisher, text= ~Publisher_name2, type = 'scatter', mode = 'markers', color = ~SE_bid_bypublisher, colors = 'Greens',
                   marker = list(size = ~SE_bid_bypublisher, opacity = 0.5)) %>%
  layout(title = 'Probability of booking VS Average Cost Per Click by Pubisher',
         xaxis = list(showgrid = T),
         yaxis = list(showgrid = T))
p3


new_table<- data.frame(Publisher_name,ROA_per_publisher)
kayak_barchart <- ggplot(data=new_table, aes(x=reorder(Publisher_name, ROA_per_publisher), y=ROA_per_publisher, fill=Publisher_name)) +
  geom_bar(stat="identity") +
  coord_flip()+
  theme_minimal() +
  scale_fill_brewer(palette="Set2")+
  ggtitle("ROA by Search Engine")+
  xlab("Search Engine") + ylab("ROA")
  
kayak_barchart


