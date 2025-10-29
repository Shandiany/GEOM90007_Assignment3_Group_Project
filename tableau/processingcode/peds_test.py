import pandas as pd

# 加载原始数据
raw = pd.read_csv("pedestrian_with_suburb.csv")
raw["Sensing_Date"] = pd.to_datetime(raw["Sensing_Date"], format="%d/%m/%Y")
raw["Month"] = raw["Sensing_Date"].dt.to_period("M").astype(str)

# 选取一个 suburb 验算，比如 Melbourne 的 2024-01
mask = (raw["Suburb"]=="Southbank") & (raw["Month"]=="2024-01")
print(raw.loc[mask, "Total_of_Directions"].mean())