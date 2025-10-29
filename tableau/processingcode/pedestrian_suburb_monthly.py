import pandas as pd

# 读取原始数据
df = pd.read_csv("pedestrian_with_suburb.csv")

# 转换日期列为 datetime 格式（原格式是 DD/MM/YYYY）
df["Sensing_Date"] = pd.to_datetime(df["Sensing_Date"], format="%d/%m/%Y")

# 提取年月、日期字段
df["Month"] = df["Sensing_Date"].dt.to_period("M").astype(str)   # 例如 '2024-03'
df["Date"] = df["Sensing_Date"].dt.date

# ---- 聚合：每月、每区的平均人流量 ----
monthly_avg = (
    df.groupby(["Suburb", "Month"])["Total_of_Directions"]
      .mean()
      .reset_index()
      .rename(columns={"Total_of_Directions": "Avg_Pedestrians"})
)

# ---- 保存结果 ----
monthly_avg.to_csv("pedestrian_monthly_by_suburb.csv", index=False)
