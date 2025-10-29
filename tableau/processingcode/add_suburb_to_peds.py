import pandas as pd
# 读入主表和映射表
df = pd.read_csv("pedestrian.csv")
sensors = pd.read_csv("unique_sensors_suburb.csv")

# 根据 Sensor_Name 合并
merged = df.merge(sensors[["Sensor_Name", "Suburb"]], on="Sensor_Name", how="left")

merged.to_csv("pedestrian_with_suburb.csv", index=False)
print("✅ 已为每条记录添加 Suburb")