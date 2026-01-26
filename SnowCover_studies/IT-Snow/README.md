# README

## Overview

This repository contains data processing scripts and results derived from the **IT-SNOW** dataset, a multi-year snow reanalysis for Italy. IT-SNOW provides daily maps of:

* Snow Water Equivalent (SWE, in mm w.e.)
* Snow depth (HS, in cm)
* Bulk-snow density (RhoS, in kg/m³)
* Liquid water content (Theta_W, in %)

Data are organized in monthly NetCDF files with time and latitude/longitude information for georeferencing. Maps correspond to snapshots at 11 AM UTC, assumed representative of daily conditions.

The IT-SNOW dataset integrates modeling, in-situ observations from snow-depth sensors and automatic weather stations, as well as remote sensing products (Sentinel-2, MODIS, H-SAF), producing a high-quality snow reanalysis suitable for hydrological and civil protection applications. For full details, see:

> Francesco Avanzi et al., *IT-SNOW: a snow reanalysis for Italy blending modeling, in-situ data, and satellite observations (2009-2021)*, Earth System Science Data, 2022.

---

## Purpose of this Repository

All scripts, analyses, and results in this repository are **based on IT-SNOW data**. This includes:

* Processing monthly SWE maps
* Stacking daily or monthly snow reanalysis into annual or seasonal products
* Any derived visualizations or statistical analyses

This repository **does not redistribute the original IT-SNOW data**. All results depend on the original dataset, which must be obtained separately according to its license terms.

---

## Data Source and Citation

**Dataset:** IT-SNOW (2009–2021)
**License:** CC BY-NC 4.0 ([link](https://creativecommons.org/licenses/by-nc/4.0/deed.en))

When using this repository, please cite the source as follows:

> Avanzi, F., et al. (2022). IT-SNOW: a snow reanalysis for Italy blending modeling, in-situ data, and satellite observations (2009–2021). *Earth System Science Data*.

---

## License

The scripts and materials in this repository are provided under a permissive license for **non-commercial use**. All IT-SNOW data remain the property of their respective copyright holders.

**Note:** IT-SNOW data are provided *as is*. The repository author is not responsible for errors, omissions, or misuse of the data.

---

## References

* IT-SNOW project website: [link if available]
* Original publication: Avanzi, F. et al., 2022, *Earth System Science Data*
* IT-SNOW License: [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/deed.en)

