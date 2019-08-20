library(dplyr)
library(reshape2)
library(ggplot2)
library(caret)
library(e1071)
library(bpa)
library(rminer)
library(ROCR)

df.retail <- read.csv("Online Retail.csv")
names(df.retail)[3] <- "Product"

######################
##Nomor 1 Visualisasi#
######################
#Note : Untuk setelah preprocessing harus dilakukan cleaning data dahulu

#1a Visualisasi Group Contry Total Transaksi
#sebelum preprocessing
df.vis.customer_price <- df.retail %>% group_by(CustomerID) %>% 
                          summarise(total_transaction_customer =sum(UnitPrice*Quantity))

#setelah preprocessing
unique(df.retail.select$CustomerID)
df.vis.customer_price <- df.retail.select %>% group_by(CustomerID) %>% 
  summarise(total_transaction_customer =sum(UnitPrice*Quantity))

ggplot(df.vis.customer_price, aes(df.vis.customer_price$CustomerID, df.vis.customer_price$total_transaction_customer, 
                                  fill=total_transaction_customer)) + 
  geom_bar(stat="identity", position="identity", na.rm = TRUE) +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(0, 50000)+
  labs(title = "Customer Unik Berdasarkan Total Nilai Transaksi", x = "Customer Unik", y = "Nilai Transaksi", fill = "Nilai Transaksi")

#Country
#sebelum preprocessing
df.vis.country_price <- df.retail %>% group_by(Country) %>% 
  summarise(total_transaction_country =sum(UnitPrice*Quantity))

#setelah preprocessing
df.vis.country_price <- df.retail.select %>% group_by(Country) %>% 
  summarise(total_transaction_country =sum(UnitPrice*Quantity))

# plot(df.vis.country_price$Country, df.vis.country_price$total_price, ylim(5000))

# top_n(df.vis.country_price, 10, total_transaction_country)
# df.country_price.2.to.10 <- df.vis.country_price %>%
#                 arrange(desc(total_transaction_country)) %>%
#                 slice(2:10) 
#Color bisa ganti fill
unique(df.vis.country_price$CustomerID)
str(df.vis.country_price$CustomerID)
df.vis.country_price$CustomerID <- as.factor(df.vis.country_price$CustomerID)

ggplot(df.vis.country_price, aes(df.vis.country_price$Country, df.vis.country_price$total_transaction_country, 
                                  fill=Country)) + 
  geom_bar(stat="identity", position="identity", na.rm = TRUE) +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Country Unik Berdasarkan Total Nilai Transaksi", x = "Country Unik", y = "Nilai Transaksi", fill = "Nilai Transaksi")

#1b 
df.vis.country_costumer <- df.retail %>% group_by(Country) %>% 
  summarise(total_unique_customer = n_distinct(CustomerID))

#setelah preprocessing
df.vis.country_costumer <- df.retail.select %>% group_by(Country) %>% 
  summarise(total_unique_customer = n_distinct(CustomerID))

ggplot(df.vis.country_costumer, aes(df.vis.country_costumer$Country, df.vis.country_costumer$total_unique_customer, 
                                 fill=total_unique_customer)) + 
  geom_bar(stat="identity", position="identity", na.rm = TRUE) +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Country Berdasarkan Kustomer yang Unik", x = "Country", y = "Jumlah Kustomer", fill = "Jumlah Kustomer")


#1c
# df.vis.product_price <- df.retail %>% group_by(Product) %>% 
#           summarise(total_price_product =sum(UnitPrice), total_quantity = sum(Quantity))

#item harga termahal, terumurah
h_rendah <- min(df.retail$UnitPrice)
nominal_rendah <- which(df.retail$UnitPrice == min(df.retail$UnitPrice))
h_tinggi <- max(df.retail$UnitPrice)
nominal_tinggi <- which(df.retail$UnitPrice == max(df.retail$UnitPrice))

print(c(paste0("Item dengan Harga tertinggi adalah ",
               df.retail$Product[nominal_tinggi] ," seharga ",h_tinggi),
        paste0("Item dengan Harga terendah adalah ", df.retail$Product[nominal_rendah]
               ," seharga ",h_rendah)))

h_rendah <- min(df.retail.select$UnitPrice)
nominal_rendah <- which(df.retail.select$UnitPrice == min(df.retail.select$UnitPrice))
h_tinggi <- max(df.retail.select$UnitPrice)
nominal_tinggi <- which(df.retail.select$UnitPrice == max(df.retail.select$UnitPrice))

print(c(paste0("Item dengan Harga tertinggi adalah ",
               df.retail.select$Product[nominal_tinggi] ," seharga ",h_tinggi),
        paste0("Item dengan Harga terendah adalah ", df.retail.select$Product[nominal_rendah]
               ," seharga ",h_rendah)))

# top_n(df.vis.product_price, 10, total_price_product)
# df.product_price <- df.vis.product_price %>%
#   arrange(desc(total_price_product)) 

#mean, stdev
summary(df.product_price)
summary(df.retail$UnitPrice)
sd(df.retail$UnitPrice)
#seleah preprocessing
summary(df.retail.select$UnitPrice)
sd(df.retail.select$UnitPrice)

# df.product_price.2.to.10$Product <-as.numeric(as.factor(df.product_price.2.to.10$Product))
# plot.product_price <- ggplot(data=df.product_price, aes(x= Product, y=total_price_product))
# plot.product_price + geom_histogram(binwidth = 300)

#histogram
hist(df.retail$UnitPrice, freq = TRUE, 
     main="Histogram Distribusi Harga Produk", xlab = "Harga Produk")
#setelah preprocessing 
hist(df.retail.select$UnitPrice, freq = TRUE, 
     main="Histogram Distribusi Harga Produk", xlab = "Harga Produk")

#1D Visualisasi Distribusi Total Quantity
df.vis.product_quantity <- df.retail.select %>% group_by(Product, Country, Month) %>%
  summarise(total_price_product =sum(UnitPrice*Quantity), total_quantity = sum(Quantity))
summary(df.retail.select)
summary(df.vis.product_quantity)

tail.quantity <- df.vis.product_quantity$total_quantity[1:4074]
tail.product <- df.vis.product_quantity$Product[1:4074]

str(df.vis.product_quantity)
df.vis.product_quantity$total_quantity <- as.integer(df.vis.product_quantity$total_quantity)

plot.product_price <- ggplot(data=df.vis.product_quantity, aes(x= tail.product, y=tail.quantity))
plot.product_price + geom_histogram(binwidth = 300)

hist(tail.quantity)
df.aus <- df.vis.product_quantity[df.vis.product_quantity[, 2]=="United Kingdom", ]

#1E Eksplorasi
hh <- table(df.retail.select[df.predict$Total_quantity == 'Low',]$Total_quantity)["Low"]
hh<- c(hh, table(df.predict[df.predict$Total_quantity == 'Medium',]$Total_quantity)["Medium"])
hh <- c(hh, table(df.predict[df.predict$Total_quantity == 'High',]$Total_quantity)["High"])
hh <- as.numeric(hh)
bpa(df.predict$Total_quantity, unique_only =TRUE)
hh
ggplot(df.predict, aes(x = , y = hh, fill = Total_quantity)) +
  geom_bar(stat = "identity") +
  ggtitle('Income Level with Years of Education')

#####################
####PREPROCESSING####
#####################

#Function
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colSort <- function(data, ...) sapply(data, sort, ...)

#Mengubah nama kolom
names(df.retail)[3] <- "Product"

#Memilih Variabel yang mau dipakai untuk preprocessing
df.retail.select <- df.retail %>%
                      select(Product, Country, Quantity, UnitPrice, InvoiceDate, CustomerID)

df.retail.select[c("Month", "Date", "Year")]  <- colsplit(
df.retail.select$InvoiceDate,"\\/",c("Month", "Date", "Year"))
df.retail.select[c("Year", "Hour")]  <- colsplit(df.retail.select$Year," ",c("Year", "Hour"))
df.retail.select[c("Hour", "Minute")]  <- colsplit(df.retail.select$Hour,":",c("Hour", "Minute"))

#Remove NA (Quantity banyak yg 0, description ?)
summary(df.retail.select)
basic_pattern_analysis(df.retail.select$InvoiceDate, unique_only = TRUE)

#Mencari patter quantity = 0
df.retail.select[!grepl("0", df.retail.select$Quantity),]

#Mencari pattern minus
colMax(df.retail.select)
sort(df.retail.select$Quantity, decreasing = FALSE)

#Cleaning
grepl(pattern= "-", x = df.retail.select$Quantity) 

#Produk
trimws(df.retail.select$Product)
df.retail.select$Product <- gsub("[^A-Za-z.,0-9 ]", "", df.retail.select$Product)
df.retail.select$Product <- gsub("^\\d+$", "", df.retail.select$Product)
df.retail.select <-df.retail.select[!(is.na(df.retail.select$Product) | 
                                        df.retail.select$Product==""), ]

#Kuantitas
summary(df.retail.select$Quantity)
df.retail.select <- df.retail.select[!grepl("-", df.retail.select$Quantity),]
df.retail.select$Quantity <- gsub("[^0-9]", "", df.retail.select$Quantity)
df.retail.select$Quantity <- as.numeric(df.retail.select$Quantity)
summary(df.retail.select$Quantity)
df.retail.select <-df.retail.select[!(is.na(df.retail.select$Quantity) | 
                                        df.retail.select$Quantity==""), ]

#UnitPRice
df.retail.select <- df.retail.select[!grepl("-", df.retail.select$UnitPrice),]
summary(df.retail.select)
df.retail.select <- df.retail.select[!grepl("0", df.retail.select$UnitPrice),]

# write.csv(df.retail.select, "clean_before_transform.csv")
######################
####Transformation####
######################
df.retail.clean <- df.retail.select %>% filter(df.retail.select$Country == "United Kingdom") %>% 
  group_by(Product, Country, Month, Hour) %>%
  summarise(Total_price_product =sum(UnitPrice*Quantity), Total_quantity = sum(Quantity))

head(df.retail.clean$Total_quantity)

str(df.retail.select$Month)
df.retail.clean <-  df.retail.clean %>%
                              mutate(
                                Season = case_when(
                                  Month %in% 9:11 ~ "Autumn",
                                  Month %in%  12  ~ "Winter",
                                  Month %in%  1:2  ~ "Winter",
                                  Month %in%  3:5  ~ "Spring",
                                  TRUE ~ "Summer")
                              )
str(df.retail.clean$Season)

df.retail.clean$Hour = cut(df.retail.clean$Hour, 
                               breaks = c(0, 12, 24), right=FALSE,
                               labels = c("AM", "PM"))

df.retail.transform <- df.retail.clean %>%  
  group_by(Product, Country, Season, Hour) %>%
  summarise(Total_price_product =sum(Total_price_product), Total_quantity = sum(Total_quantity))

hist(df.retail.transform$Total_quantity)
summary(df.retail.transform)

# mean(df.retail.transform$Total_quantity)
# summary(df.retail.transform)

df.retail.transform$Total_quantity <- cut(df.retail.transform$Total_quantity, 
                                       breaks = c(0, 500, 1500, 10000), right=FALSE,
                                       labels = c("Low", "Medium", "High"))

# df.retail.transform$Total_quantity <-c("Low", "Medium", "High")[
#   findInterval(df.retail.transform$Total_quantity , c(0, 1500, Inf) ) ]
unique(df.retail.transform$Product)

df.predict <- df.retail.transform %>% ungroup(Country)  %>%  
                select(Product, Season, Hour, Total_quantity)
# write.csv(df.retail.transform, "clean_transform_retail.csv")
# write.csv(df.predict, "final_selected_column_retail.csv")

#*************#
#GROUP PRODUCT# Untuk memprediksi grup produk, tetapi tidak ditulis dan dipakai di laporan
#*************#
library(stringdist)

#Inisialisai variable untuk hasil.akhir
hasil.akhir <- NULL

#Inisialiasi variable grouping_no dengan nilai 1
grouping_no <- 1

Product <- unique(df.retail.clean$Product)
# Product <- head(df.distinct.Product$Product,11182)

while(length(Product)>0)
{
  referensi <- Product[1]
  jarak.teks <- stringdist(referensi, Product, method="jw")
  
  produk.hasil <- Product[jarak.teks <= 0.25]
  var.temp <- data.frame(Product_group=grouping_no, Product=produk.hasil)
  
  #Menggabungkan row dengan iterasi sebelumnya
  hasil.akhir <- rbind(hasil.akhir, var.temp)  
  
  #Mengambil porsi data yang bukan di dalam threshold dengan menggunakan simbol ! yang mewakili operator not (bukan)
  Product <- Product[!(jarak.teks <= 0.25)]
  
  #Menambahkan nilai grouping untuk diambil pada iterasi selanjutnya
  grouping_no <- grouping_no + 1
}

hasil.akhir
df.retail.transform_group <- merge(x=df.retail.clean, y=hasil.akhir, by.x = "Product", by.y = "Product", all = TRUE)

df.retail.transform_group <- df.retail.transform_group %>%  
  group_by(Product_group, Country, Season) %>%
  summarise(Total_price_product =sum(Total_price_product), Total_quantity = sum(Total_quantity))

df.retail.transform_group$Product_group <- as.factor(df.retail.transform_group$Product_group)
df.retail.transform_group$Total_quantity <- as.factor(df.retail.transform_group$Total_quantity)
df.retail.transform_group$Season <- as.factor(df.retail.transform_group$Season)

df.retail.transform_group$Total_quantity <-c("Low", "Medium", "High")[
  findInterval(df.retail.transform_group$Total_quantity , c(-Inf, 1500, Inf) ) ]

##########---------------------------##############
##############  Membuat Model #####################
##########---------------------------##############
df.predict$Product <- as.factor(df.predict$Product)
df.predict$Season <- as.factor(df.predict$Season)
df.predict$Hour <- as.factor(df.predict$Hour)
df.predict$Total_quantity <- as.factor(df.predict$Total_quantity)

#split data
set.seed(100)
par <- createDataPartition(df.predict$Total_quantity,p=0.66,list=F)
# count(par)
retail.training <- df.predict[par,] 
retail.testing <- df.predict[-par,]
summary(df.predict)

###############---------------------------###############
# Naive Bayes
###############---------------------------###############

nbmodel <- naiveBayes(Total_quantity~., data=retail.training)
# print(nbmodel)
# plot(nbmodel)

nbpredict <- predict(nbmodel, retail.testing)
table(nbpredict, retail.testing$Total_quantity)
confusionMatrix(nbpredict, retail.testing$Total_quantity)

#10 FOLD CROSS VALIDATION (terlalu lama)
# train_control <- caret::trainControl(method="cv", number=10)
# nbmodel <- caret::train(Total_quantity~., data=df.predict, trControl=train_control, method="nb")
# print(nbmodel)

# nbmodel$results
# Evaluasi Accuracy, Precision, Recall, dan F-Measure
# evaluation(nbmodel,retail.testing, "raw")

# Evaluasi Accuracy, Precision, Recall, dan F-Measure Menggunakan 50 Repeated Holdout
# Function Evaluate Accuracy, Precision, Recall, F-Measure
evaluation <- function(model, data, atype) {
  hasilPrediciton <- predict(model, data, type=atype)
  table_evaluation <- table(hasilPrediciton, data$Total_quantity)
  accuracy <- sum(diag(table_evaluation))/sum(table_evaluation)
  precision <- table_evaluation[1,1]/sum(table_evaluation[ ,1])
  recall <- table_evaluation[1,1]/sum(table_evaluation[1,])
  f <- 2* (precision*recall)/(precision+recall)
  
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}

#50 REPEATED HOLD-OUT
accuracyALL <- 0

precision.1ow <- 0
recall.low <- 0
f.1ow <- 0

precision.med <- 0
recall.med <- 0
f.med <- 0

precision.high <- 0
recall.high <- 0
f.high <- 0
for(i in 1:50){
  H1 = holdout(df.predict$Total_quantity, ratio=2/3, mode="random",seed=NULL)
  nbmodel2 <- naiveBayes(Total_quantity~., data = retail.training[H1$tr,])
  nbpredict2 <- predict(nbmodel2, newdata = df.predict[H1$ts,])
  result <- confusionMatrix(nbpredict2, df.predict[H1$ts,]$Total_quantity)
  
  table(nbpredict2, df.predict[H1$ts,]$Total_quantity)
  confusionMatrix(nbpredict2, df.predict[H1$ts,]$Total_quantity)
  
  accuracy <- result$overall['Accuracy']
  accuracyALL <- accuracyALL + accuracy
  
  precision <- table_evaluation[1,1]/sum(table_evaluation[ ,1])
  recall <- table_evaluation[1,1]/sum(table_evaluation[1,])
  f <- 2* (precision*recall)/(precision+recall)
  
  precision.1ow <- precision.1ow + precision
  recall.low <- recall.low + recall
  f.1ow <- f.1ow + f
  recall.low
  precision2 <- table_evaluation[2,2]/sum(table_evaluation[ ,2])
  recall2 <- table_evaluation[2,2]/sum(table_evaluation[2,])
  f.med <- 2* (precision2*recall2)/(precision2+recall2)
  
  precision.med <- precision.med + precision2
  recall.med <- recall.med + recall2
  f.med <- 2* (precision.med*recall.med)/(precision.med+recall.med)
  
  precision3 <- table_evaluation[3,3]/sum(table_evaluation[ ,3])
  recall3 <- table_evaluation[3,3]/sum(table_evaluation[3,])
  f.high <- 2* (precision3*recall3)/(precision3+recall3)
  
  precision.high <- precision.high + precision3
  recall.high <- recall.high + recall3
  f.high <- 2* (precision.high*recall.high)/(precision.high+recall.high)
}
accuracyALL <- accuracyALL/50
accuracyALL

precision.1ow <- precision.1ow/50
precision.1ow
precision.med <- precision.med/50
precision.med
precision.high <- precision.high/50
precision.high

recall.low <- recall.low/50
recall.low
recall.med <- recall.med/50
recall.med
recall.high <- recall.high/50
recall.high

f.1ow <- f.1ow/50
f.1ow
f.med <- f.med/50
f.med
f.high <- f.high/50
f.high

#ROC
# print(nbpredict)
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# range01(as.numeric(retail.testing$Total_quantity))
# 
# prb <- prediction(as.numeric(nbpredict), as.numeric(retail.testing$Total_quantity))
# prfb <- performance(prb, measure = "tpr", x.measure = "fpr")
# ddb <- data.frame(FP = prfb@x.values[[1]], TP = prfb@y.values[[1]])
