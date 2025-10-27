# %% [markdown]
# # KAN

# %% [markdown]
# ### 1.导入需要的库

# %%
import pandas as pd
import torch
import matplotlib.pyplot as plt
from neuralforecast import NeuralForecast
from neuralforecast.models import KAN
from neuralforecast.losses.pytorch import DistributionLoss

# %% [markdown]
# ### 2.导入数据

# %%
# 导入数据

file_path = "oil_lagx.csv"
Y_df = pd.read_csv(file_path)


# 显示数据
print(Y_df.head())

# 检查数据基本信息
print(Y_df.info())


# %% [markdown]
# ### 3.数据处理

# %%
# 删去最后五列
Y_df1 = Y_df.drop(Y_df.columns[-5:], axis = 1)

# 删去缺失值的行
Y_df2 = Y_df1.dropna()

# 显示数据
print(Y_df2.head())

# %%
# 转化为d
Y_df2['date'] = pd.to_datetime(Y_df2['date'], errors='coerce')  # 将 'date' 转换为 datetime 类型
Y_df2.loc[:, 'date'] = Y_df2['date'].dt.date  # 现在可以使用 .dt 访问器
# 对齐时间

# 按日期排序
Y_df2 = Y_df2.sort_values(by='date')

# 获取从数据第一行到最后一行的日期顺序
Y_df2['date'] = pd.date_range(start=Y_df2['date'].min(), periods=len(Y_df2), freq='D')

# 查看结果
print(Y_df2)


# %%
# 插入新列unique_id
Y_df2.insert(0, 'unique_id', 1)

# 将date换名ds，value换成y
Y_df2 = Y_df2.rename(columns={'date': 'ds', 'value': 'y'}).reset_index(drop=True)

# 查看结果
print(Y_df2)



# %%
Y_df3 = Y_df2# 特征仅lag使用

# %%
# 特征滞后一个时间单位以保证本期特征预测下一期（有其他外生变量用）

# 提取y列
col = Y_df2['y']

# 删除第一个观测值
col_1 = col.drop(col.index[0])

# 将下面的观测值往上递补一位
col_2 = col_1.reset_index(drop=True)

# 将处理后的列重新赋值给DataFrame
Y_df2['y'] = col_2

# 删除最后一行
Y_df3 = Y_df2.drop(Y_df2.index[-1])


# %%
# 划分训练测试集，90% 用于训练集，10% 用于测试集
train_size = int(len(Y_df3) * 0.59-6)

# 划分训练集和测试集
train_Y_df3 = Y_df3[:train_size]
test_Y_df3 = Y_df3[train_size:].reset_index(drop=True)

# 打印训练集和测试集的大小
print(f"训练集大小: {len(train_Y_df3)}")
print(f"测试集大小: {len(test_Y_df3)}")

# %% [markdown]
# ### 4.初始化模型

# %%
futr_exog_list = Y_df3.columns[3:].tolist() # 历史外生变量列名列名

# %%
models = [KAN(h=2,
            input_size=24,
            futr_exog_list=futr_exog_list, 
            n_hidden_layers=2,
            hidden_size=256,  
            loss=DistributionLoss(distribution='Normal'),
            max_steps=150,
            learning_rate=0.001,
            num_lr_decays=3,
            batch_size=32
                )]

# %% [markdown]
# ###  5.滚动预测（拓展非拓展在于train_Y_df3 = Y_df3.iloc[i:train_size + i]）

# %%
import time
start_time = time.time()

predictions = []

for i in range(0, len(test_Y_df3), 2):
    train_Y_df3 = Y_df3.iloc[i:train_size + i]
    test_Y_df3 = Y_df3.iloc[train_size + i :train_size + i+2 ]
    nf = NeuralForecast(models=models, freq='D')
    nf.fit(df=train_Y_df3)
    Y_hat_df = nf.predict(futr_df=test_Y_df3)
    prediction = Y_hat_df['KAN-median'].values.flatten()
    predictions.append(prediction)

end_time = time.time()
print(f" Code running time: {end_time - start_time} seconds")

# %%
# 将预测结果转换为DataFrame
print(predictions)
predicted_df = pd.DataFrame(predictions)

print(predicted_df)

# %%
# 将三列数据按行顺序转换为一列
Y_pred = predicted_df.values.flatten()  # 将 DataFrame 转换为一维数组
Y_pred = pd.DataFrame(Y_pred, columns=['Values'])  # 转换为 DataFrame

# 输出结果
print(Y_pred)

# %% [markdown]
# ### 6.计算评估指标及绘图

# %%
import os
os.environ["KMP_DUPLICATE_LIB_OK"]="TRUE"

# %%
test_Y_df3 = Y_df3[train_size:].reset_index(drop=True)
print(test_Y_df3 )

# %%
import pandas as pd

# 设置目标日期
end_date = pd.to_datetime('2023-07-30')

# 生成往前256天的日期
dates = pd.date_range(end=end_date, periods=512, freq='D').date

# 打印结果
print(dates)


# %%
plt.figure(figsize=(12, 6))

# 绘制真实值（测试集）
plt.plot(dates, test_Y_df3['y'], label='real', color='blue', marker='o')

# 绘制预测值
plt.plot(dates, Y_pred, label='pred', color='red', linestyle='--', marker='x')

# 添加图例
plt.legend()

# 设置标题和标签
plt.title('KAN')
plt.xlabel('date')
plt.ylabel('y/y_pred')
 #Fit of out-of-sample predicted and true values
# 旋转 x 轴的标签，防止日期显示不清楚
plt.xticks(rotation=45)
# 设置 y 轴范围，调整 y 轴最小值和最大值
plt.ylim(0.8,9.7)  # 将 y 轴范围缩小到 [-0.5, 0.5]
# 显示图形
plt.tight_layout()
plt.show()

# %%
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error
import numpy as np
Y = test_Y_df3['y']
# 计算均方误差 (MSE)
mse = mean_squared_error(Y_pred, Y)

# 计算均方根误差 (RMSE)
rmse = np.sqrt(mse)
mae = mean_absolute_error(Y_pred, Y)
print("RMSE:", rmse)
print("MAE:", mae)


# %% [markdown]
# ### 7.基准模型har-rv

# %%
import pandas as pd
import numpy as np
import statsmodels.api as sm

# Set random seed for reproducibility
np.random.seed(123)

# Read in data
rv_daily = Y_df1

# Filter out rows with missing values
rv_daily = rv_daily.dropna()

# Store predictions in a list
pred = []

# Length of the dataset minus the number of training observations（后面减去的是训练集个数）
lengthr = len(rv_daily['value']) - 722

# Rolling forecast
for i in range(lengthr):
    # Training set range
    train_start = i  # Start at the very beginning
    train_end = 721+ i  # i加上的是训练集个数减一

    # Extract training and test data
    train_data = rv_daily.iloc[train_start:train_end + 1]
    test_data = rv_daily.iloc[train_end + 1 ]

    # Ensure that train_data has lag variables

    # Define training predictors and response
    X_train = train_data[['lag1', 'lag5', 'lag22']]
    y_train = train_data['value']

    # Add constant to the model (intercept)
    X_train = sm.add_constant(X_train)

    # Fit the model
    har_rv = sm.OLS(y_train, X_train).fit()

    # Prepare test data for prediction (same predictors as the training data)
    X_test = test_data[['lag1', 'lag5', 'lag22']].values.reshape(1, -1)  # Ensure it's 2D
    X_test = sm.add_constant(X_test, has_constant='add')  # Add constant to test data

    # Predicting the next value
    forecast_value = har_rv.predict(X_test)

    # Store the predicted value
    pred.append(forecast_value[0])

# Output predictions
print(pred)


# %%
# 转换为 DataFrame 一列
pred = pd.DataFrame(pred, columns=["Pred"])

# 查看结果
print(pred)

# %% [markdown]
# ### 8.绘制harrv测试集图像并计算基于harrv的nbeatsx的R方

# %%
plt.figure(figsize=(12, 6))

# 绘制真实值（测试集）
plt.plot(dates, test_Y_df3['y'], label='real', color='blue', marker='o')

# 绘制预测值
plt.plot(dates, pred['Pred'], label='pred', color='red', linestyle='-.', marker='x')

# 添加图例
plt.legend()

# 设置标题和标签
plt.title('harrv')
plt.xlabel('date')
plt.ylabel('y/y_pred')

# 旋转 x 轴的标签，防止日期显示不清楚
plt.xticks(rotation=45)
# 设置 y 轴范围，调整 y 轴最小值和最大值
plt.ylim(0.8, 9.7)
# 显示图形
plt.tight_layout()
plt.show()

# %%
molecular = np.sum((Y_pred['Values'] - Y)**2)
print(molecular)
denominator = np.sum((pred['Pred'] - Y)**2)
print(denominator)

# %%
r2 = 1 - molecular/denominator
print(r2)

# %%
#rmse mae of harrv
mse_har = mean_squared_error(pred, Y)

rmse_har = np.sqrt(mse_har)

mae_har = mean_absolute_error(pred, Y)

print(rmse_har)
print(mae_har )

# %%


# %% [markdown]
# 

# %%
import pandas as pd

# 假设 Y_pred 和 Pred 是 pandas DataFrame 或 Series
Y_pred_df = pd.DataFrame(Y_pred, columns=['Values'])  # 将 Y_pred 转换为 DataFrame
Pred_df = pd.DataFrame(pred, columns=['Pred'])  # 将 Pred 转换为 DataFrame

# 合并为一个文件，或者分别保存
result_df = pd.concat([Y_pred_df, Pred_df], axis=1)  # 按列合并

# 保存为 CSV 文件
result_df.to_csv('predictions_KAN_har_512_noexpanding.csv', index=False)



# %%


# %%


# %%


# %%


# %%


# %%


# %%


# %%


# %%


# %%


# %%



