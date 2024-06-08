# 資料彙整 -----
library(tidyverse)

# 設定檔案路徑
file_path <- "data"

# 讀取所有資料
data_list <- list(
  read_csv(file.path(file_path, "11004.csv")),
  read_csv(file.path(file_path, "11005.csv")),
  read_csv(file.path(file_path, "11006.csv")),
  read_csv(file.path(file_path, "11007.csv")),
  read_csv(file.path(file_path, "11008.csv")),
  read_csv(file.path(file_path, "11009.csv")),
  read_csv(file.path(file_path, "11010.csv")),
  read_csv(file.path(file_path, "11011.csv")),
  read_csv(file.path(file_path, "11012.csv")),
  read_csv(file.path(file_path, "11101.csv")),
  read_csv(file.path(file_path, "11102.csv")),
  read_csv(file.path(file_path, "11103.csv")),
  read_csv(file.path(file_path, "11104.csv"))
)

# 合併資料並加總遊客人次
total <- data_list %>%
  bind_rows() %>%
  group_by(遊憩區名稱) %>%
  summarise(`總遊客人次` = sum(`遊客人次（人次）`, na.rm = TRUE)) %>%
  ungroup()

# 繪製長條圖 -----
library(ggplot2)

# 繪製總遊客人次的長條圖
ggplot(total, aes(x = 遊憩區名稱, y = `總遊客人次`)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "各遊憩區總遊客人次",
       x = "遊憩區名稱",
       y = "總遊客人次") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 旋轉x軸標籤以避免重疊

# 建立 trend 資料框 -----

# 從每個 data frame 中提取總計的遊客人次
monthly_totals <- map_dbl(data_list, ~ .x %>% filter(遊憩區名稱 == "-") %>% pull(`遊客人次（人次）`))

# 創建 trend 資料框
trend <- tibble(
  月份 = c("11004", "11005", "11006", "11007", "11008", "11009", "11010", "11011", "11012", "11101", "11102", "11103", "11104"),
  總人數 = monthly_totals
)


# 繪製折線圖 -----
library(ggplot2)

# 繪製總人數的折線圖
ggplot(trend, aes(x = 月份, y = 總人數, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "每月總遊客人次趨勢",
       x = "月份",
       y = "總人數") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 旋轉x軸標籤以避免重疊



