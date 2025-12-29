import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

iris = sns.load_dataset('iris')
sns.scatterplot(
    data=iris,
    x='sepal_width',
    y='petal_width',
    hue='species' # color by species, hue is the seaborn equivalent of color aesthetic in ggplot2
)
plt.show()

import plotly.express as px
df = px.data.iris()

fig = px.scatter(
    df,
    x="sepal_width",
    y="petal_width",
    color="species"
)

fig.show()

# with more chat options
df = px.data.iris()
fig = px.scatter(
    df,
    x="sepal_width",
    y="petal_width",
    color="species",
    title="Iris Sepal Width vs Petal Width",
    labels={
        "sepal_width": "Sepal Width (cm)",
        "petal_width": "Petal Width (cm)",
        "species": "Iris Species"
    },
)
fig.show()

# equivalent with leaflet
import folium
from folium.plugins import MarkerCluster

import folium

m = folium.Map(
    location=[47.6062, -122.3321],  # Seattle
    zoom_start=11,
    tiles="CartoDB Positron"
)

# show map
m
m.save("seattle_map.html")

# get the same map for victoria, BC, Canada
m_victoria = folium.Map(
    location=[48.4284, -123.3656],  # Victoria, BC
    zoom_start=13,
    tiles="CartoDB Positron"
)
m_victoria.save("victoria_map.html")

# add some markers with clustering
import folium
from folium.plugins import MarkerCluster

# base map (you already have this)
m_victoria = folium.Map(
    location=[48.4284, -123.3656],
    zoom_start=13,
    tiles="CartoDB Positron"
)

# locations to add
locations = [
    {"name": "University of Victoria", "lat": 48.4634, "lon": -123.3117},
    {"name": "BC Legislature", "lat": 48.4194, "lon": -123.3656},
    {"name": "Inner Harbour", "lat": 48.4246, "lon": -123.3680},
    {"name": "Royal BC Museum", "lat": 48.4197, "lon": -123.3677},
]

# marker cluster layer
cluster = MarkerCluster(name="Key Locations").add_to(m_victoria)

for loc in locations:
    folium.CircleMarker(
        location=[loc["lat"], loc["lon"]],
        radius=8,
        popup=loc["name"],
        fill=True,
        fill_opacity=0.7,
        color="blue"
    ).add_to(cluster)

# layer toggle
folium.LayerControl().add_to(m_victoria)

# save to HTML
m_victoria.save("victoria_map.html")

# %% [code]
# # Interactive visualization in an attempt to evaluate the claim that “The City of Seattle is changing”

# load data /Users/haoxi-home/Library/CloudStorage/OneDrive-UniversityofWaterloo/book-exercises-erwin/chapter-17-exercises/Building_Permits_20251228.csv
all_permits = pd.read_csv('/Users/haoxi-home/Library/CloudStorage/OneDrive-UniversityofWaterloo/book-exercises-erwin/chapter-17-exercises/Building_Permits_20251228.csv')

# Filter for permits for new buildings issued in 2010 or later
all_permits = pd.read_csv(
    "Building_Permits_20251228.csv",
    dtype={
        "PermitClass": "string",
        "PermitTypeDesc": "string"
    }
)
all_permits["IssuedDate"] = pd.to_datetime(all_permits["IssuedDate"])

# Filter for permits for new buildings issued in 2010 or later
new_buildings = all_permits[
    (all_permits["PermitTypeDesc"] == "New") &
    (all_permits["PermitClass"] != "N/A") &
    (all_permits["IssuedDate"] >= pd.Timestamp("2010-01-01"))
]

# create a new column sorting the year the permit was issued
new_buildings["year"] = new_buildings["IssuedDate"].dt.year

by_year = (
    new_buildings
        .assign(year=new_buildings["IssuedDate"].dt.year)
        .groupby("year")
        .size()
        .reset_index(name="n")
)

# %%
# # Use plotly to create an interactive visualization of the data
fig = px.bar(
    by_year,
    x="year",
    y="n",
    title="Number of new building permits per year in Seattle",
    labels={
        "year": "Year",
        "n": "Number of Permits"
    },
    opacity=0.7
)

fig.show()

# %%
# create a leaflect map adding map tiles and circle markers
# base map
m = folium.Map(
    location=[47.6062, -122.3321],  # lat, lng
    zoom_start=10,
    tiles="CartoDB Positron"
)

# ensure valid coordinates for mapping
new_buildings["Latitude"] = pd.to_numeric(new_buildings["Latitude"], errors="coerce")
new_buildings["Longitude"] = pd.to_numeric(new_buildings["Longitude"], errors="coerce")
new_buildings = new_buildings.dropna(subset=["Latitude", "Longitude"])

# add circles from dataframe
for _, row in new_buildings.iterrows():
    folium.CircleMarker(
        location=[row["Latitude"], row["Longitude"]],
        radius=6,
        stroke=False,
        fill=True,
        fill_opacity=0.7,
        popup=row["Description"]
    ).add_to(m)

m.save("seattle_building_permits_map.html")

# %%
from branca.colormap import StepColormap
from branca.element import Element
# %%
# add more features to the map
# =============================================================================
# Part 5: Leaflet Map with Categorical Colors + Legend
# =============================================================================
new_buildings["Latitude"] = pd.to_numeric(new_buildings["Latitude"], errors="coerce")
new_buildings["Longitude"] = pd.to_numeric(new_buildings["Longitude"], errors="coerce")
new_buildings = new_buildings.dropna(subset=["Latitude", "Longitude"])

new_buildings["PermitClass"] = new_buildings["PermitClass"].astype("category")
classes = new_buildings["PermitClass"].cat.categories.tolist()

set3_colors = [
    "#8DD3C7", "#FFFFB3", "#BEBADA",
    "#FB8072", "#80B1D3", "#FDB462",
    "#B3DE69", "#FCCDE5", "#D9D9D9",
    "#BC80BD", "#CCEBC5", "#FFED6F"
]

palette = dict(zip(classes, set3_colors[:len(classes)]))

m = folium.Map(
    location=[47.6062, -122.3321],
    zoom_start=10,
    tiles="CartoDB Positron"
)

for _, row in new_buildings.iterrows():
    folium.CircleMarker(
        location=[row["Latitude"], row["Longitude"]],
        radius=4,
        stroke=False,
        fill=True,
        fill_opacity=0.5,
        color=palette[row["PermitClass"]],
        popup=row["Description"]
    ).add_to(m)

legend_html = """
<div style="
position: fixed;
bottom: 30px;
right: 30px;
width: 230px;
background-color: white;
border: 2px solid grey;
z-index: 9999;
font-size: 14px;
padding: 10px;
">
<b>New Buildings in Seattle</b><br>
"""
for cls, col in palette.items():
    legend_html += f"""
    <i style="background:{col};
              width:18px;
              height:18px;
              float:left;
              margin-right:8px;
              opacity:0.7"></i>
    {cls}<br>
    """
legend_html += "</div>"

m.get_root().html.add_child(Element(legend_html))
m.save("seattle_building_permits_by_class.html")

# %%
