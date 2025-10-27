# 设置主文件夹路径
main_directory <- "C:/Users/taowu/Desktop/Learning Files/mortality/单位根检验"  # 替换为实际的大文件夹路径

# 获取所有子文件夹
subfolders <- list.dirs(main_directory, recursive = FALSE)

# 获取每个子文件夹中的数据文件路径
file_paths <- sapply(subfolders, function(folder) {
  list.files(folder, pattern = "\\.txt$", full.names = TRUE)
})

# 如果数据文件保存在子文件夹中，但我们只需要某种文件格式的文件，可以使用正则表达式过滤
file_paths <- unlist(file_paths)

# 初始化结果列表
results <- list()

# 批量处理
for (file in file_paths) {
  result <- process_mortality_data(file)
  results[[file]] <- result
}

# 将结果转化为数据框
results_df <- do.call(rbind, lapply(names(results), function(x) cbind(file = x, results[[x]])))

# 打印结果
print(results_df)

# 保存结果到 CSV 文件
write.csv(results_df, "7results.csv", row.names = TRUE)
