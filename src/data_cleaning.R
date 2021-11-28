setwd("~/myProjects/210601_ostadkar_r1400_2/feature_selection/")


library(dplyr)
library(ggplot2)
library(viridis)
library(data.table)
library(chron)


#setwd("~/myProjects/210601_ostadkar_r1400_2/")

mydata <-
  read.csv("~/myProjects/211128_ostadkar_fullfillment/push-up/data/Cleaning_data_extracted-r1400-02_B.csv")

head(mydata)

table(mydata$previous_status)


table(mydata$order_status)


df_status_count <-
  mydata %>%
  group_by(previous_status, order_status )%>%
  summarise(n=n())
df_status_count <-
  as.data.frame(df_status_count)

df_status_count$previous_status <- as.factor(df_status_count$previous_status)
df_status_count$order_status <- as.factor(df_status_count$order_status)

df_status_count$freq <-
  df_status_count$n / (sum(df_status_count$n))
ggplot(df_status_count, aes(x=previous_status, y= order_status)) +
  geom_tile(aes(fill=n), color='gray50') +
  geom_text(aes(label = n)) +
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_fill_viridis()


#ggsave('status_progression_heatmap.png', dpi = 500, width = 6, height = 4)



ggplot(df_status_count, aes(x=previous_status, y= order_status)) +
  geom_tile(aes(fill=freq), color='gray50') +
  geom_text(aes(label = format(freq, digits = 2,scientific = TRUE)),
            color = 'gray50', size =3) +
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_fill_viridis()

#ggsave('status_progression_heatmap_freq.png', dpi = 500, width = 8, height = 8)

####################################
dim(mydata)
length(unique(mydata$order_number))

#some orders (10%) have repetitive entries, 

df_rep <- mydata[duplicated(mydata$order_number) | duplicated(mydata$order_number, fromLast=TRUE),]
df_rep[order(df_rep$order_number),]
#we will keep the first occurance


mydata_f <-
  mydata %>%
  group_by(order_number) %>%
  arrange(desc(created_at)) %>%
  filter(row_number()==1 )

dim(mydata_f)
mydata <-
  mydata_f
mydata <- as.data.frame(mydata)
mydata$wq_to_qa_time_h <-
  mydata$wq_to_qa_time/(3600)


#we need to label the rows:
#I) remained at "need quote" stage 
#II) proceeded to "with quote" stage
#II) unclear


df_not_wquote <-
  mydata[mydata$previous_status == 'NeedQuote' &
           mydata$order_status == 'Canceled', ]


df_wquote <-
  mydata[mydata$previous_status == 'withQuote' |
           mydata$previous_status == 'Done' |
           mydata$previous_status == 'Started' |
           mydata$previous_status == 'QuoteAccepted' |
           mydata$previous_status == 'QuoteRejected' |
           mydata$previous_status == 'Finished' , ]

arrow = arrow(angle=15, type = "closed")
n_censored <- nrow(df_wquote[df_wquote$wq_to_qa_time>600,])
n_shown <- nrow(df_wquote[df_wquote$wq_to_qa_time<=600,])

ggplot(df_wquote[df_wquote$wq_to_qa_time>-200&
                   df_wquote$wq_to_qa_time<60*10,], aes(x=wq_to_qa_time)) +
  geom_histogram(bins =100)+
  theme_bw()+
  annotate("segment", x = 520, xend = 600, y =270, yend = 270, arrow=arrow)+
  annotate("text", label = paste0(n_censored, " data points censored\n\n", n_shown, " data points shown"), 
           x = 540, y = 270) +
  geom_vline(xintercept = 15, linetype=2, color = "#0072B2")+
  annotate("text", label = 'automatic qa generation\nthreshold time = 15 seconds', 
           x = 60, y = 1500, color = "#0072B2") +
  geom_vline(xintercept = 200, linetype=2, color = "#D55E00")+
  annotate("text", label = 'automatic qa generation\nthreshold time = 200 seconds', 
           x = 240, y = 1000, color = "#D55E00")

#ggsave('distribution_wq_to_wq_time.png', dpi=500, width = 8, height = 6)

nrow(df_wquote[is.na(df_wquote$wq_to_qa_time),])

nrow(df_wquote[df_wquote$wq_to_qa_time <0 ,])

df_wquote_operator <-
  df_wquote[df_wquote$wq_to_qa_time<200,]
df_wquote_organic <-
  df_wquote[df_wquote$wq_to_qa_time>=200,]


df_wquote_operator2 <-
  df_wquote[df_wquote$wq_to_qa_time<15,]
df_wquote_organic2 <-
  df_wquote[df_wquote$wq_to_qa_time>=15,]


dim(df_not_wquote)
dim(df_wquote_operator)
dim(df_wquote_organic)
################################################################
df_wquote1 <-df_wquote_organic
df_not_wquote1 <- rbind(df_not_wquote, df_wquote_operator)
df_wquote2 <-df_wquote_organic2
df_not_wquote2 <- rbind(df_not_wquote, df_wquote_operator2)

dim(df_wquote1)
dim(df_not_wquote1)

dim(df_wquote2)
dim(df_not_wquote2)


df_not_wquote1$label_wn_quote <- 'nq'
df_wquote1$label_wn_quote <- 'wq'

df_not_wquote2$label_wn_quote <- 'nq'
df_wquote2$label_wn_quote <- 'wq'


#conversion rate

nrow(df_not_wquote1) / (nrow(df_not_wquote1) + nrow(df_wquote1))

nrow(df_not_wquote2) / (nrow(df_not_wquote2) + nrow(df_wquote2))


mydf2 <- rbind(df_not_wquote1, df_wquote1)
mydf2b <- rbind(df_not_wquote2, df_wquote2)

mydf2 <- mydf2[,c('label_wn_quote', 'is_delivery_at_holiday', 'is_created_at_holiday', 
                  'duration', 'gender', 'area_cleaning', 'day_of_week_delivery',
                  'day_of_week_created','total_cost', 'all_offers', 'corporate_offers',
                  'individual_offers', 'offers_busy', 'district',
                  'past_day_all_orders', 'past_day_wq_1h', 'past_day_wq_2h',
                  'past_day_wq_3h')]

mydf2b <- as.data.table(mydf2b)
mydf2c <-
  mydf2b[mydf2b$wq_to_qa_time>0|is.na(mydf2b$wq_to_qa_time),]




(nrow(mydf2b) - nrow(mydf2c))/nrow(mydf2b)
mydf2c <- mydf2c[,c('label_wn_quote', 'is_delivery_at_holiday', 'is_created_at_holiday', 
                    'duration', 'gender', 'area_cleaning', 'day_of_week_delivery',
                    'day_of_week_created','total_cost', 'all_offers', 'corporate_offers',
                    'individual_offers', 'offers_busy', 'district',
                    'past_day_all_orders', 'past_day_wq_1h', 'past_day_wq_2h',
                    'past_day_wq_3h')]




mydf2c$cost_to_area <- mydf2c$total_cost / mydf2c$area_cleaning
mydf2c$cost_times_area <- mydf2c$total_cost * mydf2c$area_cleaning
mydf2c$duration_to_area <- mydf2c$duration / mydf2c$area_cleaning
mydf2c$duration_times_area <- mydf2c$duration * mydf2c$area_cleaning
mydf2c$indiv_offers_to_all_offers <- mydf2c$individual_offers / mydf2c$all_offers
mydf2c$corp_offers_to_all_offers <- mydf2c$corporate_offers / mydf2c$all_offers
mydf2c$busy_offers_to_all_offers <- mydf2c$offers_busy / mydf2c$all_offers



mydf2c$label_wn_quote <- as.factor(mydf2c$label_wn_quote)
mydf2c$is_delivery_at_holiday <- as.factor(mydf2c$is_delivery_at_holiday)
mydf2c$is_created_at_holiday <- as.factor(mydf2c$is_created_at_holiday)
mydf2c$district <- as.factor(mydf2c$district)

mydf2c$label_wn_quote <- as.numeric(mydf2c$label_wn_quote) -1
mydf2c$is_delivery_at_holiday <- as.numeric(mydf2c$is_delivery_at_holiday) -1
mydf2c$is_created_at_holiday <- as.numeric(mydf2c$is_created_at_holiday) -1
###############################
mydf2c <- as.data.table(mydf2c)

mydf2c[is.na(mydf2c$gender),'gender'] <- 0

mydf2c$gender <- as.factor(mydf2c$gender )


mydf2c[is.na(mydf2c$area_cleaning),'area_cleaning'] <- -1

mydf2c[is.na(mydf2c$total_cost), 'total_cost'] <- -10000


mydf2c[mydf2c$total_cost > 500000, 'total_cost'] <- 500000



mydf2c$past_day_wq_3h <- as.numeric(mydf2c$past_day_wq_3h)
mydf2c$past_day_wq_2h <- as.numeric(mydf2c$past_day_wq_2h)
mydf2c$past_day_wq_1h <- as.numeric(mydf2c$past_day_wq_1h)
mydf2c$past_day_all_orders <- as.numeric(mydf2c$past_day_all_orders)

mydf2c$day_of_week_created <- as.factor(mydf2c$day_of_week_created)
mydf2c$day_of_week_delivery <- as.factor(mydf2c$day_of_week_delivery)


mydf2c <- mydf2c[!is.na(mydf2c$duration), ]

mydf2c <- as.data.frame(mydf2c)

########################################################################


set.seed(101) # Set Seed so that same sample can be reproduced in future also
mydf2c <- mydf2c[sample(nrow(mydf2c)),]



samplec <- sample.int(n = nrow(mydf2c), size = floor(.75*nrow(mydf2c)), replace = F)



mydf2c_sub <- mydf2c[samplec, ]

####################################
#determining label counts

dim(mydf2c)

nrow(mydf2c[mydf2c$label_wn_quote==1,])/ (nrow(mydf2c))


nrow(mydf2c[mydf2c$label_wn_quote==0,])/ (nrow(mydf2c))

mydf2c <- as.data.table(mydf2c)

mydf2c[mydf2c$district == 'out_of_range', 'district'] <- '-1'
mydf2c$district <- as.numeric(mydf2c$district)


###########################
write.csv(mydf2c,'Cleaning_data_extracted-r1400-02_cleaned.csv', row.names = F )
