import sys
import numpy as np
import scipy as sp
import xarray as xr

FILL_VALUE = -9999.0


# Checking argv entries
if len(sys.argv) < 2:
    print("Uso: python3 recordSWE.py <file.nc>")
    sys.exit(1)

nameF = sys.argv[1]


# Loading datased end reading it
dat = xr.open_dataset(nameF)
if "Time" in dat and "time" in dat.dims and "time" not in dat.coords:
    dat = dat.assign_coords(time=dat["Time"]).drop_vars("Time")

swe = dat["SWE"]
timeSWE = dat["time"].values


# Studying SWE evolution on a daily basis
evoSWE = []
for t in timeSWE:
    maptoS = swe.sel(time=t)

    maptoS = maptoS.astype(np.float32)
    maptoS = maptoS.where(maptoS > FILL_VALUE)

    daySWE = maptoS.sum(skipna=True)
    evoSWE += [daySWE.item()*250/1e9]
    print(f"Total sum is: {daySWE.item()*250/1e9} km^3 we")


# Printing values to file
out_txt = nameF.replace(".nc", "_evoSWE.txt")

with open("appo.dat", "w") as f:
    for v in evoSWE:
        print(v, file=f)

print(f"Saved evolution series!")
