import pandas as pd
from geopy.geocoders import Nominatim
from geopy.extra.rate_limiter import RateLimiter

# 读入CSV，修改“xx.csv”为原表名
df = pd.read_csv("unique_sensors.csv")

# 初始化 geolocator
geolocator = Nominatim(user_agent="melb_vizrest")

# 使用 RateLimiter 控制请求频率，防止超时
reverse = RateLimiter(geolocator.reverse, min_delay_seconds=1)

# 对每一行执行反向查询
def get_suburb(lat, lon):
    try:
        location = reverse((lat, lon), language='en')
        if location and "suburb" in location.raw["address"]:
            return location.raw["address"]["suburb"]
        elif location and "city" in location.raw["address"]:
            return location.raw["address"]["city"]
        else:
            return None
    except:
        return None

# 用经纬度生成 Suburb
df["Suburb"] = df.apply(lambda r: get_suburb(r["Latitude"], r["Longitude"]), axis=1)

# 保存结果，修改“xx.csv”为新表名
df.to_csv("unique_sensors_suburb.csv", index=False)
print("✅ 已生成包含 Suburb 的新文件")
