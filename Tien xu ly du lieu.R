# Để tạo một bảng dữ liệu chứa tên các biến
Dataset_da_thay_the_du_lieu <- Dataset_raw
# Tạo một dataframe với số lượng giá trị NA cho mỗi biến
na_count_Dataset_raw <- data.frame(
  Variable = colnames(Dataset_raw),
  NA_Count = colSums(is.na(Dataset_raw))
)

# In ra biến nào có bao nhiêu giá trị NA
print(na_count_Dataset_raw)

# Kiểm tra giá trị bị thiếu trong biến 'Income'
na_Ic <- which(is.na(Dataset_raw$Income))
cat("Các giá trị bị thiếu trong biến 'Income':\n")
print(na_Ic)

# Tính giá trị trung vị của biến 'Income'
median_Income <- median(Dataset_raw$Income, na.rm = TRUE)

# Thay thế giá trị bị thiếu bằng giá trị trung vị trong Data_da_thay_the_du_lieu
Dataset_da_thay_the_du_lieu$Income[na_Ic] <- median_Income

# In giá trị sau khi thay thế
cat("Các giá trị sau khi thay thế:", 'Income', "\n")
print(Dataset_da_thay_the_du_lieu$Income[na_Ic])

View(Dataset_da_thay_the_du_lieu)



# Cài đặt và kích hoạt gói dplyr
install.packages("dplyr")
library(dplyr)

# Tạo một DataFrame mới từ Dataset_da_thay_the_du_lieu
Dataset_da_bo_ngoai_lai <- Dataset_da_thay_the_du_lieu

# Tạo một danh sách các cột số
numeric_columns <- Dataset_da_thay_the_du_lieu %>% select_if(is.numeric)

# Xác định giá trị ngoại lai và xóa dòng chứa nó
for (col in colnames(numeric_columns)) 
{
  col_mean <- mean(Dataset_da_bo_ngoai_lai[[col]])
  col_sd <- sd(Dataset_da_bo_ngoai_lai[[col]])
  # Tính Z-Score và loại bỏ giá trị ngoại lai
  Dataset_da_bo_ngoai_lai <- Dataset_da_bo_ngoai_lai %>%
    mutate(Z_Score = (!!sym(col) - col_mean) / col_sd) %>%
    filter(abs(Z_Score) <= 3) %>%
    select(-Z_Score)
}

# Xem toàn bộ dataframe Data_da_bo_ngoai_lai
View(Dataset_da_bo_ngoai_lai)


#Kiểm tra tính nhất quán của dữ liệu
#Kiểm tra định dạng dữ liêu
str(Dataset_da_bo_ngoai_lai)
# Chuyển cột "Dt_Customer" thành dạng ngày tháng (Date)
Dataset_da_bo_ngoai_lai$Dt_Customer <- as.Date(Dataset_da_bo_ngoai_lai$Dt_Customer, format = "%m/%d/%Y")


#Tóm tắt dữ liệu
summary(Dataset_da_bo_ngoai_lai)


# Loại bỏ các giá trị trùng lặp
Dataset_da_bo_trung_lap <- unique(Dataset_da_bo_ngoai_lai)
# Xem toàn bộ dataframe Data_da_bo_trung_lap
View(Dataset_da_bo_trung_lap)

# Để xóa cột "ID," "Dt_Customer," và "Complain" từ khung dữ liệu Dataset_da_bo_trung_lap
Dataset_da_chon_loc <- Dataset_da_bo_trung_lap %>%
  select(-Id, -Dt_Customer, -Complain)

#Xem khung dữ liệu sau khi xóa cột
View(Dataset_da_chon_loc)

# Loại bỏ các giá trị trùng lặp sau khi đã xóa cột
Dataset_prepared <- unique(Dataset_da_chon_loc)

# Xem toàn bộ dataframe Dataset_prepared
View(Dataset_prepared)




#Cân bằng dữ liệu
library(devtools)
devtools::install_github("dalpozz/unbalanced")
install.packages("unbalanced")
library(unbalanced)

Dataset_prepared$Response<-as.factor(Dataset_prepared$Response) # convert class to factor
Dataset_prepared$Year_Birth<-as.factor(Dataset_prepared$Year_Birth)
Dataset_prepared$Education<-as.factor(Dataset_prepared$Education)
Dataset_prepared$Marital_Status<-as.factor(Dataset_prepared$Marital_Status)
Dataset_prepared$Kidhome<-as.factor(Dataset_prepared$Kidhome)
Dataset_prepared$Teenhome<-as.factor(Dataset_prepared$Teenhome)
Dataset_prepared$Education<-as.factor(Dataset_prepared$Education)
Dataset_prepared$Recency<-as.factor(Dataset_prepared$Recency)
Dataset_prepared$NumDealsPurchases<-as.factor(Dataset_prepared$NumDealsPurchases)
Dataset_prepared$NumWebPurchases<-as.factor(Dataset_prepared$NumWebPurchases)
Dataset_prepared$NumCatalogPurchases<-as.factor(Dataset_prepared$NumCatalogPurchases)
Dataset_prepared$NumStorePurchases<-as.factor(Dataset_prepared$NumStorePurchases)
Dataset_prepared$NumWebVisitsMonth<-as.factor(Dataset_prepared$NumWebVisitsMonth)

levels(Dataset_prepared$Response) <- c('0', '1')

predictor_variables <- Dataset_prepared[,-19] 
response_variable <- Dataset_prepared$Response   
levels(response_variable) <- c('0', '1')

Smote_data <- ubBalance(predictor_variables, 
                        response_variable, 
                        type='ubSMOTE',    
                        positive = 1,
                        k = 10,              
                        percOver = 600,  
                        percUnder = 850,   
                        verbose = TRUE)

Smote_combined <- cbind(Smote_data$X,       
                        Smote_data$Y)
names(Smote_combined)[names(Smote_combined) == "Smote_data$Y"] <- "Response"
levels(Smote_combined$Response) <- c('0', '1')
Dataset_prepared <- Smote_combined

Dataset_prepared<-unique(Dataset_prepared)

View (Dataset_prepared)
write.csv(Dataset_prepared,"Dataset_prepared.csv",row.names = FALSE)

