import sys
import numpy as np
import scipy as sp
import xarray as xr

FILL_VALUE = -9999.0


# Faccio un check sulla lista di comandi passata nel momento in cui è stato 
# lanciato il programma in questione
if len(sys.argv) < 2:
    print("Uso: python3 recordSWE.py <file.nc>")
    sys.exit(1)

nameF = sys.argv[1]


# Carico il dataset e mi preparo a leggere il contenuto
dat = xr.open_dataset(nameF)
if "Time" in dat and "time" in dat.dims and "time" not in dat.coords:
    dat = dat.assign_coords(time=dat["Time"]).drop_vars("Time")

swe = dat["SWE"]
timeSWE = dat["time"].values


# Studio l'evoluzione dello snow water equivalent ciclando sui giorni contenuti all'interno del mese
evoSWE = []
for t in timeSWE:
    maptoS = swe.sel(time=t)

    maptoS = maptoS.astype(np.float32)
    maptoS = maptoS.where(maptoS > FILL_VALUE)

    daySWE = maptoS.sum(skipna=True)
    evoSWE += [daySWE.item()*250/1e9]
    print(f"La somma totale del contenuto è: {daySWE.item()*250/1e9} km^3 we")


# Stampo a file per far sì che siano fruibili anche allo script bash
out_txt = nameF.replace(".nc", "_evoSWE.txt")

with open("appo.dat", "w") as f:
    for v in evoSWE:
        print(v, file=f)

print(f"Salvata serie d'evoluzione!")
