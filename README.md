An R package to access NOAA NCEI Blended Seawinds (NBS v2)
================

Provides for access to [NBS
2.0](https://coastwatch.noaa.gov/cwn/products/noaa-ncei-blended-seawinds-nbs-v2.html)
from [NOAA Coastwatch](https://coastwatch.noaa.gov/cwn/index.html) which
replaces the first version called [Blended Sea
Winds](https://www.ncei.noaa.gov/products/blended-sea-winds) online.

This package provides tolls to querying, downloading and archiving
blended sea wind raster files. Data are served on THREDDS catalog using
OPeNDAP services. We hide all of the details and provide a simplified
interface to download rasters (saved as GeoTIFF) and simple database
organization.

### [Citation](https://doi.org/10.3389/fmars.2022.935549)

> Saha, K.; Huai-Min, Z. Hurricane and Typhoon Storm Wind Resolving NOAA
> NCEI Blended Sea Surface Wind (NBS) Product. Frontiers in Marine
> Sciences – Ocean Observation 2022, 9, 1–12.
> <https://doi.org/10.3389/fmars.2022.935549>.

## Requirements

From CRAN…

- [R v4+](https://www.r-project.org/)
- [rlang](https://CRAN.R-project.org/package=rlang)
- [dplyr](https://CRAN.R-project.org/package=dplyr)
- [tidyr](https://CRAN.R-project.org/package=tidyr)
- [stars](https://CRAN.R-project.org/package=stars)
- [sf](https://CRAN.R-project.org/package=sf)
- [R6](https://CRAN.R-project.org/package=R6)
- [thredds](https://CRAN.R-project.org/package=thredds)
- [readr](https://CRAN.R-project.org/package=readr)
- [rerddap](https://CRAN.R-project.org/package=rerddap)

## Installation

    remotes::install_github("BigelowLab/nbs")

## Usage

``` r
suppressPackageStartupMessages({
  library(nbs)
  library(dplyr)
  library(sf)
  library(stars)
})
```

### Set your root data path

Our workflow requires a path where you will save your requested data. We
suggest that you store your root data path in a hidden configuration
file (defaults to `~/.nbsdata`). This is a fix-and-forget step, but you
can override or change it later. Here is how we set up my data path.

``` r
my_data_path = "/Users/ben/Library/CloudStorage/Dropbox/data/noaa/nbs"
ok = dir.create(my_data_path, showWarnings = FALSE, recursive = TRUE)
nbs::set_root_path(my_data_path)
```

That’s it. When you later download data for some region, say for the
Mediterranean, you can specify a subfolder, `med`, and store regional
data there.

#### Organization of Blended Seawinds

Blended Sea Winds are organized by product and interval. The two primary
product types are wind velocities and wind stresses. Each product type
is organized further by aggregation interval `[6h, daily, monthly]`. In
addition, NOAA/NCEI serves up both “near real time” (NRT) and
science-digest versions (SCI). These are served as “aggregate” or
“per-file” OPeNDAP resources on the THREDDS server. We find that
accessing the “per-file” resources is more responsive than the otherwise
more convenient “aggregate” resources.

Here we tally the the 10 resources available to us. *Caveat* - we don’t
use the near real time (NRT) resources thus we haven’t tested them.

``` r
nbs_tally()
```

    ##  [1] "Global Six-Hour UVComp" "Global Six-Hour Stress" "Global Daily UVComp"   
    ##  [4] "Global Daily Stress"    "Global Monthly UVComp"  "Global Monthly Stress" 
    ##  [7] "Global Six-Hour UVComp" "Global Six-Hour Stress" "Global Daily UVComp"   
    ## [10] "Global Daily Stress"

#### Query for a product

Use `nbs_query()` to search for product URLs.

``` r
uris = query_nbs(period = "monthly",
                 dates = as.Date(c("1995-01-01", "1995-12-31")),
                 product = "uvcomp") |>
  dplyr::glimpse()
```

    ## Rows: 12
    ## Columns: 3
    ## $ date   <date> 1995-01-01, 1995-02-01, 1995-03-01, 1995-04-01, 1995-05-01, 19…
    ## $ dateid <chr> "199501", "199502", "199503", "199504", "199505", "199506", "19…
    ## $ url    <chr> "https://www.star.nesdis.noaa.gov/thredds/dodsC/uvcompNCEIBlend…

### Fetching data

When data are fetched they are automatically stored in GeoTIFF format in
a database-like directory structure in the path of your own choosing.
Each GeoTIFF file contains one parameter, at one depth and one time.

We’ll download just a portion of the western South Atlantic. First we
need to make the destination path for the data, and then define the
bounding box. Then we simply pass the table of URLs and the bounding box
to `fetch_nbs()`. We also provide the bounding box and path.
`param = "all"` is shorthand for fetching
`c("u_wind", "v_wind", "windspeed")` from the `uvcomp` product.

In return we’ll receive a small table (*aka* database) of the downloaded
files.

``` r
path = nbs_path("wsa")
ok = dir.create(path, recursive = TRUE, showWarnings = FALSE)
bb = c(xmin = 290, ymin = -60, xmax = 360, ymax = 10)
db = fetch_nbs(uris, param = 'all', bb = bb, path = path, verbose = FALSE)
```

### The database

The database contains identifying information about the stored files
**except** for the path to the storage location. This adds a little
extra work for the end user (keeping track of the data path), but that
extra effort is offset by improved portability and ease of use.

``` r
db
```

    ## # A tibble: 36 × 7
    ##    source                 date       hour  param per   nrt   time               
    ##    <chr>                  <date>     <chr> <chr> <chr> <lgl> <dttm>             
    ##  1 uvcomp_SCIMonthlyGlob… 1995-01-15 0000… u_wi… month FALSE 1995-01-15 00:00:00
    ##  2 uvcomp_SCIMonthlyGlob… 1995-01-15 0000… v_wi… month FALSE 1995-01-15 00:00:00
    ##  3 uvcomp_SCIMonthlyGlob… 1995-01-15 0000… wind… month FALSE 1995-01-15 00:00:00
    ##  4 uvcomp_SCIMonthlyGlob… 1995-02-15 0000… u_wi… month FALSE 1995-02-15 00:00:00
    ##  5 uvcomp_SCIMonthlyGlob… 1995-02-15 0000… v_wi… month FALSE 1995-02-15 00:00:00
    ##  6 uvcomp_SCIMonthlyGlob… 1995-02-15 0000… wind… month FALSE 1995-02-15 00:00:00
    ##  7 uvcomp_SCIMonthlyGlob… 1995-03-15 0000… u_wi… month FALSE 1995-03-15 00:00:00
    ##  8 uvcomp_SCIMonthlyGlob… 1995-03-15 0000… v_wi… month FALSE 1995-03-15 00:00:00
    ##  9 uvcomp_SCIMonthlyGlob… 1995-03-15 0000… wind… month FALSE 1995-03-15 00:00:00
    ## 10 uvcomp_SCIMonthlyGlob… 1995-04-15 0000… u_wi… month FALSE 1995-04-15 00:00:00
    ## # ℹ 26 more rows

We provide functions, `write_database()` and `read_database()`, to
simplify the input-output.

``` r
write_database(db, path)
```

### Reading in data

We can filter the database using standard tools, and then read in the
data as rasters.

``` r
db2 = dplyr::filter(db, param %in% c("u_wind", "v_wind"), 
                        date > as.Date("1995-10-01"),
                        date <= as.Date("1995-12-31"))
x = read_nbs(db2, path)
x
```

    ## stars object with 3 dimensions and 2 attributes
    ## attribute(s):
    ##               Min.   1st Qu.      Median      Mean  3rd Qu.      Max.  NA's
    ## u_wind  -10.904867 -4.461770  0.05806307 0.1607201 4.617272 11.819275 61099
    ## v_wind   -6.077292 -1.241886 -0.10956497 0.2898438 1.383320  8.025926 61099
    ## dimension(s):
    ##      from  to offset delta  refsys point
    ## x       1 281  289.6  0.25  WGS 84 FALSE
    ## y       1 282  10.12 -0.25  WGS 84 FALSE
    ## time    1   3     NA    NA POSIXct    NA
    ##                                              values x/y
    ## x                                              NULL [x]
    ## y                                              NULL [y]
    ## time 1995-10-15 UTC, 1995-11-15 UTC, 1995-12-15 UTC

Note that the object has two attributes (“u_wind” and “v_wind”) and
three layers (October, November and December).
