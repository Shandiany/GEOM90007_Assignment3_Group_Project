import pandas as pd

df = pd.read_csv("pedestrian.csv")

# 提取每个 sensor 的唯一经纬度
sensor_coords = df[["Sensor_Name", "Latitude", "Longitude"]].drop_duplicates()
sensor_coords.to_csv("unique_sensors.csv", index=False)

print(f"✅ 提取完成，共 {len(sensor_coords)} 个 sensor")