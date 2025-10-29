import pandas as pd

# === 1️⃣ 读取两个原始文件 ===
landmarks = pd.read_csv("landmarks_new.csv")
restaurants = pd.read_csv("restaurant_new.csv")

# === 2️⃣ 为景点表补充缺失列 ===
landmarks["RankingPosition"] = None
landmarks["PriceLevel"] = None
landmarks["Rating"] = None
landmarks["Address"] = None
landmarks["NumberOfReviews"] = None

# === 3️⃣ 为餐厅表补充缺失列（SubType）===
restaurants["SubType"] = None

# === 4️⃣ 统一字段顺序 ===
columns = [
    "Name", "Type", "SubType", "Suburb",
    "Rating", "PriceLevel", "RankingPosition",
    "Address", "Latitude", "Longitude", "NumberOfReviews"
]

landmarks = landmarks[columns]
restaurants = restaurants[columns]

# === 5️⃣ 合并 ===
combined = pd.concat([landmarks, restaurants], ignore_index=True)

# === 6️⃣ 导出 ===
combined.to_csv("landmarks_restaurants.csv", index=False)
print("✅ 合并完成！输出文件：combined_places.csv")