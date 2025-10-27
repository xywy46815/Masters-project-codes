# %% [markdown]
# # autoformer调参

# %% [markdown]
# ### 1.导入需要的库

# %%
import pandas as pd
import torch
import matplotlib.pyplot as plt
from neuralforecast import NeuralForecast
from neuralforecast.models import Autoformer
from neuralforecast.losses.pytorch import MQLoss
import itertools
from sklearn.metrics import mean_squared_error
import numpy as np

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
# 删去最后五列（最后五列缺失过多，影响数据结构）
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
# 特征滞后一个时间单位以保证本期特征预测下一期

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
print(Y_df3)

# %%
# 划分训练测试集
train_size = int(len(Y_df3) * 0.7)

# 划分训练集和测试集
train_Y_df3 = Y_df3[:train_size]
test_Y_df3 = Y_df3[train_size:].reset_index(drop=True)

# 打印训练集和测试集的大小
print(f"训练集大小: {len(train_Y_df3)}")
print(f"测试集大小: {len(test_Y_df3)}")

# %% [markdown]
# ### 4.初始化模型

# %%
futr_exog_list = Y_df2.columns[3:].tolist() # 历史外生变量列名列名

# %%
# 参数搜索范围
param_grid = {
    "input_size": [48,60],
    "hidden_size":[128,256],
    "conv_hidden_size":[32,64],
    "n_head":[2,3,4],
    "dropout": [0.2,0.3],
    "learning_rate": [1e-3, 1e-4],
    "max_steps":[150,200]
}

# 展开参数组合
param_combinations = list(itertools.product(*param_grid.values()))

# 保存最佳参数和最小误差
best_params = None
min_avg_rmse = float('inf')

# %%
import time
start_time = time.time()
# 保存所有参数组合的结果
param_results = []

for params in param_combinations:
    input_size,hidden_size,conv_hidden_size,n_head, dropout, learning_rate, max_steps = params
    models = [
        Autoformer(
            h=1,
            input_size=input_size,
            hidden_size=hidden_size,
            conv_hidden_size=conv_hidden_size,
            n_head=n_head,
            futr_exog_list=futr_exog_list,
            loss=MQLoss(),
            scaler_type="robust",
            dropout=dropout,
            max_steps=max_steps,
            learning_rate=learning_rate,
            num_lr_decays=3,
            batch_size=64
        )
    ]

    # 滚动预测部分
    rmses = []
    predictions = []

    for i in range(0, len(test_Y_df3)):
        train_Y_df3 = Y_df3.iloc[i:train_size + i]  # 扩展训练集
        test_Y_df3 = Y_df3.iloc[train_size + i : train_size + i +1]  # 测试当前两步

        nf = NeuralForecast(models=models, freq='D')
        nf.fit(df=train_Y_df3)

        Y_hat_df = nf.predict(futr_df=test_Y_df3)
        prediction = Y_hat_df['Autoformer-median'].values.flatten()
        Y_pred = pd.DataFrame(prediction, columns=['y_pred'])

        # 计算每一步预测的 RMSE
        rmse = np.sqrt(mean_squared_error(test_Y_df3['y'], Y_pred['y_pred']))
        rmses.append(rmse)
        predictions.extend(prediction)  # 保存预测值

    # 计算当前参数的整体平均 RMSE
    avg_rmse = np.mean(rmses)
    # 保存当前参数结果
    param_results.append((params, avg_rmse))

end_time = time.time()
print(f" Code running time: {end_time - start_time} seconds")

# 输出所有参数的表现
for params, avg_rmse in param_results:
    print(f"Params: {params}, Avg RMSE: {avg_rmse}")



# 找到最优参数
best_params, min_avg_rmse = min(param_results, key=lambda x: x[1])
print(f"Best Parameters: {best_params}, Min Avg RMSE: {min_avg_rmse}")



