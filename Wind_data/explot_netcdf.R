
library(ncdf4)
library(raster)

#Exemple file CEP0125.YYYYMMDDHH00.nc. Weather forecast data made at HH on YYYYMMDD date.
#It contains 48 steps, from HH+1h until HH+48h (intraday and Day+1) 
file="Wind_data/HRES_France/2018/CEP0125.201801010000.nc"

#read netcdf file
nc=nc_open(file , write = FALSE)
print(nc)

# the file contains 4 variables: 
# u10: zonal component of 10 meter wind speed (m/s)
# v10: meridional component of 10 meter wind speed (m/s)
# u100: zonal component of 100 meter wind speed (m/s)
# v100: meridional component of 100 meter wind speed (m/s)
# the wind speed can be calculated at each level (10m or 100m) as the module of the wind vector: sqrt( u**2 + v**2)

# the variables are saved in the netcdf data as a function of [longitude,latitude,step]
# for exemple to extract the longitude we can use the function ncvar_get ("netcdf_file" , "variable_name")

longitude=ncvar_get(nc , "longitude") # longitudes ranging between -5° and 8.375° East by 0.125° step
latitude=ncvar_get(nc , "latitude")   # latitudes ranging between 51.250° and 42.250° north by 0.125° step
step=ncvar_get(nc , "step")           # step starts from 1 to 48, and stands for the forecast hours starting from the YYYYMMDD date


# we can use the same function ncvar_get to extract u10 map for exemple at step = 1
u10=ncvar_get(nc , "u10")
s=1 #step
rast=raster(t(u10[,,s]), xmn=-5, xmx=8.375, ymn=42.25, ymx=51.25)
crs(rast) <- "+proj=lonlat +lat_1=52 +lon_0=-10 +ellps=WGS84"
plot(rast)

# to extract a timeserie of two days forecats (from step 1 to 48) at a specific location, for exemple 0° longitude and 44° latitude
index_lon=match(-5,longitude)   #longitude[17]
index_lat=match(51.125, latitude)  #latitude[21]

u=ncvar_get(nc , "u10" , c(index_lon,index_lat,1), c(1,1,length(step)))
v=ncvar_get(nc , "v10" , c(index_lon,index_lat,1), c(1,1,length(step)))
WindSpeed= sqrt(u**2 + v**2) # m/s




