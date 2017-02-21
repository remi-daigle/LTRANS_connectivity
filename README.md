Tutorial to extract connectivity matrices from raw LTRANS output
================

Before we begin, if you want to follow along, this tutorial assumes you have already installed [R](http://www.r-project.org/) and [RStudio](http://www.rstudio.com/). Additionally, you will need the data contained in the project folder. You can go to the GitHub [repository](https://github.com/remi-daigle/LTRANS_connectivity) to download (or fork, etc) the whole project folder including the R markdown file used to generate this website, or you can [click here](https://github.com/remi-daigle/LTRANS_connectivity/archive/gh-pages.zip) to download the project folder as a zipped file.

I recommend opening using RStudio and opening the `LTRANS_connectivity.Rproj` inside the project directory, since that sets the R working directory to wherever you have downloaded the project. That way R will know where to find the data!

Let's start by loading our packages, if you haven't installed these previously, you will need to install them manually (e.g. `install.packages("tidyverse")`)

``` r
require(data.table)
require(tidyverse)
require(readr)
require(rgdal)
require(rgeos)
require(maptools)
```

Processing all the data in one pass will make your computer burst into flames, so lets first pick a `year` and a `pld`.

``` r
year <- 1999
pld <- 120
```

Loading data
============

Release Grid
------------

Let's load up and plot the grid we used to generate the particle releases. It's a hexagonal grid that DFO Maritimes regions is using in their MPA planning MARXAN analysis. I've taken the liberty to expand that grid beyond the DFO Maritimes region to cover the entire model domain from 0-250 m depth.

We don't really need this, but might be useful for visualization and indexing.

``` r
grid250 <- readOGR("data/shapefiles","grid250")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "data/shapefiles", layer: "grid250"
    ## with 77678 features
    ## It has 1 fields

``` r
plot(grid250)
```

![](index_files/figure-markdown_github/loading%20grid-1.png)

Release locations
-----------------

Let's load up the release locations files that I generated using the above grid. First, list the files

``` r
# list the files
filenames <- list.files(path="data/LTRANS_input", pattern="rl.", full.names=TRUE,recursive=T)

# load all files into a list, read_csv is much faster than read.csv
rllist <- lapply(filenames, read_csv,
                 col_names = c("long0","lat0","Z0","delay","site0"),
                 col_types = cols("d","d","i","i","i")
)

# set the names of the items in the list, so that you know which file it came from
rllist <- setNames(rllist,filenames)

# rbind the list
rl <- rbindlist(rllist, idcol="filename")

# The `site0` column was not set in the release location file. But in our case (you'll have to trust me) they correspond to the `UNIT_ID` in `grid250`
rl$site0 <- grid250$UNIT_ID
head(rl)
```

    ##                      filename     long0     lat0 Z0 delay site0
    ## 1: data/LTRANS_input/rl_1.txt -67.79184 42.84886  0     0     1
    ## 2: data/LTRANS_input/rl_1.txt -67.79434 42.88113  0     0     2
    ## 3: data/LTRANS_input/rl_1.txt -67.79684 42.91340  0     0     3
    ## 4: data/LTRANS_input/rl_1.txt -67.79935 42.94566  0     0     4
    ## 5: data/LTRANS_input/rl_1.txt -67.75014 42.80205  0     0     5
    ## 6: data/LTRANS_input/rl_1.txt -67.75262 42.83431  0     0     6

The filename is a little clunky to work with. We just need the number to match with the directory structure I set for the raw data (more on this below).

``` r
rl$bin <- gsub(".*rl_|.txt.*", "",rl$filename)
head(rl)
```

    ##                      filename     long0     lat0 Z0 delay site0 bin
    ## 1: data/LTRANS_input/rl_1.txt -67.79184 42.84886  0     0     1   1
    ## 2: data/LTRANS_input/rl_1.txt -67.79434 42.88113  0     0     2   1
    ## 3: data/LTRANS_input/rl_1.txt -67.79684 42.91340  0     0     3   1
    ## 4: data/LTRANS_input/rl_1.txt -67.79935 42.94566  0     0     4   1
    ## 5: data/LTRANS_input/rl_1.txt -67.75014 42.80205  0     0     5   1
    ## 6: data/LTRANS_input/rl_1.txt -67.75262 42.83431  0     0     6   1

LTRANS data
-----------

Before we start, I need to explain how I've organized the output in different directories indicating the `year`, `day`, and `bin`. However, I've preceded `year` with the letter `E` to indicate east, so I don't mix up my coasts! So if you look at the files in [`data/LTRANS_output/E1999/152/1/`](https://github.com/remi-daigle/LTRANS_connectivity/tree/master/data/LTRANS_output/E1999/152/1); That is the output of one model run which used the east coast model, using current velocities from the year 1999. In this case particles were released on the 152nd day of the year (June 1st). Lastly the `bin` number `1` relates to which release location file was used, in this case it was [rl\_1.txt](https://github.com/remi-daigle/LTRANS_connectivity/blob/master/data/LTRANS_input/rl_1.txt). This type of directory structure is not typical of LTRANS output, but rather how I've chosen to organize my data.

Getting back to the files that are produced from one model run, I ignore the `endfile.csv`, and focus on the `para    XXXX.csv` files. I ran the particle tracking for 120 days and you'll see that the files are all sequentially numbered 1002 to 1121 (and that there are 120 of them). For reasons beyond my understanding, the file that has the positions of the larvae at the end of 'day 1' is `para    1002.csv', 'day 2' is`para 1003.csv', and so on until 'day 120' is \`para 1121.csv'

``` r
# list the particle tracking files for that particular year and pld
filenames <- list.files(path=paste0("data/LTRANS_output/E",year), pattern=glob2rx(paste0("*para    1",formatC(pld+1, width = 3, format = "d", flag = "0"),"*")), full.names=TRUE,recursive=T)

# load all files into a list, read_csv is much faster than read.csv
datalist <- lapply(filenames, read_csv,
                 col_names = c("long","lat","Z","Out","site"),
                 col_types = cols("d","d","d","i","i")
)

# set the names of the items in the list, so that you know which file it came from
datalist <- setNames(datalist,filenames)

# rbind the list
dataset <- rbindlist(datalist, idcol="filename")
dataset$site <- NA
rm(datalist)

# extract all the useful information out of filename
dataset <- dataset %>%
    mutate(temp=substr(filename,20,nchar(filename))) %>%
    separate(temp,c("temp_type_year","rday","bin","time"),"/",convert=TRUE) %>% 
    separate(temp_type_year,c("type","year"),sep=1,convert=TRUE) %>% 
    mutate(time=as.integer(substr(time,9,12)))
    
# link release data to output by bin, not elegant, but it works!

for(i in unique(dataset$bin)){
    x <- rl$bin==i
    y <- dataset$bin==i
    dataset$long0[y] <- rl$long0[x]
    dataset$lat0[y] <- rl$lat0[x]
    dataset$Z0[y] <- rl$Z0[x]
    dataset$delay[y] <- rl$delay[x]
    dataset$site0[y] <- rl$site0[x]
}
head(dataset)
```

    ##                                          filename    long    lat       Z
    ## 1 data/LTRANS_output/E1999/152/1/para    1121.csv -67.279 43.310 -150.70
    ## 2 data/LTRANS_output/E1999/152/1/para    1121.csv -62.191 39.489   -0.78
    ## 3 data/LTRANS_output/E1999/152/1/para    1121.csv -62.935 40.919  -36.99
    ## 4 data/LTRANS_output/E1999/152/1/para    1121.csv -59.752 42.041  -16.69
    ## 5 data/LTRANS_output/E1999/152/1/para    1121.csv -69.477 41.781  -47.26
    ## 6 data/LTRANS_output/E1999/152/1/para    1121.csv -67.265 41.138  -49.39
    ##   Out site type year rday bin time     long0     lat0 Z0 delay site0
    ## 1   0   NA    E 1999  152   1 1121 -67.79184 42.84886  0     0     1
    ## 2   0   NA    E 1999  152   1 1121 -67.79434 42.88113  0     0     2
    ## 3   0   NA    E 1999  152   1 1121 -67.79684 42.91340  0     0     3
    ## 4   0   NA    E 1999  152   1 1121 -67.79935 42.94566  0     0     4
    ## 5   0   NA    E 1999  152   1 1121 -67.75014 42.80205  0     0     5
    ## 6   0   NA    E 1999  152   1 1121 -67.75262 42.83431  0     0     6

Calculating connectivity
========================

Determine settlement location
-----------------------------

Now, we know what site we released each particle from (`site0`) and the latitude/longitude of where each particle was after a pld of 120 days. However, we don't know the `site` that corresponds to the latitude/longitude. We could fire up R's GIS type tools (e.g. `rgeos::gContains`), but we don't need anything that fancy for this type a grid. We only need to know which release location (which are the center of each grid hexagon).

Now with this example `dataset` we could likely calculate all of

``` r
dataset$site <- NA

n <- 1000
for(i in seq(1,nrow(dataset),n)){
    i2 <- i+n-1
    if(i2>nrow(dataset)) i2 <- nrow(dataset)
    
    dataset$site[i:i2] <- apply(dataset[i:i2,],1,
                                function(x) which.min(
                                    abs(
                                        (rl$lat0-as.numeric(x[names(dataset)=="lat"]))
                                    )+
                                        abs(
                                            (rl$long0-as.numeric(x[names(dataset)=="long"]))
            )))
}
head(dataset)    
```

    ##                                          filename    long    lat       Z
    ## 1 data/LTRANS_output/E1999/152/1/para    1121.csv -67.279 43.310 -150.70
    ## 2 data/LTRANS_output/E1999/152/1/para    1121.csv -62.191 39.489   -0.78
    ## 3 data/LTRANS_output/E1999/152/1/para    1121.csv -62.935 40.919  -36.99
    ## 4 data/LTRANS_output/E1999/152/1/para    1121.csv -59.752 42.041  -16.69
    ## 5 data/LTRANS_output/E1999/152/1/para    1121.csv -69.477 41.781  -47.26
    ## 6 data/LTRANS_output/E1999/152/1/para    1121.csv -67.265 41.138  -49.39
    ##   Out  site type year rday bin time     long0     lat0 Z0 delay site0
    ## 1   0   424    E 1999  152   1 1121 -67.79184 42.84886  0     0     1
    ## 2   0  9230    E 1999  152   1 1121 -67.79434 42.88113  0     0     2
    ## 3   0  8080    E 1999  152   1 1121 -67.79684 42.91340  0     0     3
    ## 4   0 13945    E 1999  152   1 1121 -67.79935 42.94566  0     0     4
    ## 5   0 36122    E 1999  152   1 1121 -67.75014 42.80205  0     0     5
    ## 6   0 22217    E 1999  152   1 1121 -67.75262 42.83431  0     0     6

Generate sampling grid
----------------------

Now the release grid has 77678 cells. If we tried to calculate a matrix using those, the resulting matrix would be over 45 GB, big problem! Don't believe me? Try it!

``` r
matrix(0,ncol=length(grid250),nrow=length(grid250))
```

Anyway, the point is, we need a bigger cell size grid so we can simplify this computationally.

``` r
uniongrid <- unionSpatialPolygons(grid250,rep(1,length(grid250)))

proj <- "+proj=utm +zone=20 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
uniongrid <- spTransform(uniongrid,CRS(proj))

bb <- Spatial(bbox(gBuffer(uniongrid,width=100000)), proj4string = CRS(proj))
             
biggridbb <- HexPoints2SpatialPolygons(spsample(bb, type="hexagonal", n=500))
plot(biggridbb)

plot(uniongrid,add=T,col='blue')
```

![](index_files/figure-markdown_github/generate%20sampling%20grid-1.png)

Grid tyding
-----------

So, now we have an OK grid we can use for demonstration purposes, but let's remove the cells that don't overlap with `grid250` and let's create a key so that we can figure our which cells from `grid250` overlap with which cells in `biggrid`

``` r
biggrid <- biggridbb[gIntersects(biggridbb,uniongrid,byid=T)[1,],]
biggrid <- spChFIDs(biggrid,as.character(1:length(biggrid)))
plot(biggrid)
plot(uniongrid,add=T,col='blue')
```

![](index_files/figure-markdown_github/cleanup%20new%20grid-1.png)

``` r
# before we keep going let's change the projection on `biggrid` back to what we have for `grid250`
biggrid <- spTransform(biggrid,CRS(proj4string(grid250)))
```

Make a key

``` r
key <- gIntersects(biggrid,grid250,byid=T)
key <- unlist(lapply(apply(key,1,which),'[[',1)) #remove the ones that stradle the border

# plot and example to use the key
plot(biggrid[19,])
plot(grid250[key==19,],add=T)
```

![](index_files/figure-markdown_github/making%20a%20key-1.png)

Use the key to set determine to which `biggrid` cell the sites correspond.

``` r
dataset$grid0 <- key[dataset$site0]
dataset$grid <- key[dataset$site]
head(dataset)
```

    ##                                          filename    long    lat       Z
    ## 1 data/LTRANS_output/E1999/152/1/para    1121.csv -67.279 43.310 -150.70
    ## 2 data/LTRANS_output/E1999/152/1/para    1121.csv -62.191 39.489   -0.78
    ## 3 data/LTRANS_output/E1999/152/1/para    1121.csv -62.935 40.919  -36.99
    ## 4 data/LTRANS_output/E1999/152/1/para    1121.csv -59.752 42.041  -16.69
    ## 5 data/LTRANS_output/E1999/152/1/para    1121.csv -69.477 41.781  -47.26
    ## 6 data/LTRANS_output/E1999/152/1/para    1121.csv -67.265 41.138  -49.39
    ##   Out  site type year rday bin time     long0     lat0 Z0 delay site0
    ## 1   0   424    E 1999  152   1 1121 -67.79184 42.84886  0     0     1
    ## 2   0  9230    E 1999  152   1 1121 -67.79434 42.88113  0     0     2
    ## 3   0  8080    E 1999  152   1 1121 -67.79684 42.91340  0     0     3
    ## 4   0 13945    E 1999  152   1 1121 -67.79935 42.94566  0     0     4
    ## 5   0 36122    E 1999  152   1 1121 -67.75014 42.80205  0     0     5
    ## 6   0 22217    E 1999  152   1 1121 -67.75262 42.83431  0     0     6
    ##   grid0 grid
    ## 1    24   34
    ## 2    24   29
    ## 3    24   28
    ## 4    24   40
    ## 5    24   16
    ## 6    24   12

Calculate connectivity
----------------------

``` r
# caculate a frequency table
con_mat_table <- table(factor(dataset$grid0,levels=1:length(biggrid)),
                     factor(dataset$grid,levels=1:length(biggrid))
)

# I find the `table` objects in R a bit awkward to handle, so you can examin it via a `data.frame`. In this format, `Var1` is equivalent to `grid0`; your source cells. While `Var2` is equivalent to `grid`; your settlement cells
con_mat_df <- as.data.frame(con_mat_table)
head(con_mat_df)
```

    ##   Var1 Var2 Freq
    ## 1    1    1    0
    ## 2    2    1    1
    ## 3    3    1    0
    ## 4    4    1    0
    ## 5    5    1    0
    ## 6    6    1   21

``` r
# ... or a matrix. In this format, the rownames are equivalent to `grid0`; your source cells. While the column names are equivalent to `grid`; your settlement cells
con_mat <- as.matrix(con_mat_table)
head(con_mat)
```

    ##    
    ##       1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   1   0   0   1   0   0   1   0   1   0   1   0   0   1   0   1   1
    ##   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   0   0   0   1   0   0   3   3  21   3   3   5   2   0   0   5   5
    ##   6  21   4   5   3  11  11  35  23   7   9  11  16  18   4   5  26  13
    ##    
    ##      18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   0   0   0   0   0   1   1   0   0   2   2   2   1   0   0   0   1
    ##   3   0   0   0   0   0   0   0   0   0   1   3   2   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   4   3   5   0   3   2   3   1   0   8  19   7   9   0   1   2   3
    ##   6   7   3  24   0   4   7   8  11   0  41 135  35  31   0   3   3  23
    ##    
    ##      35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   2   0   0   0   0   2   3   0   0   0   0   0   1   0   0   0   0
    ##   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   3   0   0   0   0   4   3   0   0   0   0   1   0   0   0   0   0
    ##   6   9   0   0   0   0  45  21   1   0   0   0   2   5   0   0   0   0
    ##    
    ##      52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   3   0   0   0   3   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   0   0   0   3   0   0   0   0   0   0   0   0   0   0   0   0   1
    ##   6   0   0   3  11   0   0   0   0   0   0   0   0   0   1   0   0  20
    ##    
    ##      69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   6  14   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##    
    ##      86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##    
    ##     103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##    
    ##     120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##    
    ##     137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##    
    ##     154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##    
    ##     171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187
    ##   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##    
    ##     188 189 190 191 192 193
    ##   1   0   0   0   0   0   0
    ##   2   0   0   0   0   0   0
    ##   3   0   0   0   0   0   0
    ##   4   0   0   0   0   0   0
    ##   5   0   0   0   0   0   0
    ##   6   0   0   0   0   0   0

However, you'll notice that these are frequencies, not probabilities. First let's determine how many larvae were released from each cell

``` r
# caculate a frequency table
n <- as.data.frame(table(factor(dataset$grid0,levels=1:length(biggrid))))

# now we can divide the frequency by n to get probabilities. (what I did below only works if the levels of the factors in `n` and con_mat_df$Var1 are the same!)
con_mat_df$percent <- con_mat_df$Freq/n$Freq
head(con_mat_df)
```

    ##   Var1 Var2 Freq    percent
    ## 1    1    1    0        NaN
    ## 2    2    1    1 0.03703704
    ## 3    3    1    0 0.00000000
    ## 4    4    1    0        NaN
    ## 5    5    1    0 0.00000000
    ## 6    6    1   21 0.03030303

``` r
# and of course we can do the same operation in matrix form.
con_mat_percent <- con_mat/n$Freq
head(con_mat_percent)
```

    ##    
    ##               1           2           3           4           5
    ##   1                                                            
    ##   2 0.037037037 0.000000000 0.000000000 0.037037037 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.007352941 0.000000000
    ##   6 0.030303030 0.005772006 0.007215007 0.004329004 0.015873016
    ##    
    ##               6           7           8           9          10
    ##   1                                                            
    ##   2 0.000000000 0.037037037 0.000000000 0.037037037 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.022058824 0.022058824 0.154411765 0.022058824
    ##   6 0.015873016 0.050505051 0.033189033 0.010101010 0.012987013
    ##    
    ##              11          12          13          14          15
    ##   1                                                            
    ##   2 0.037037037 0.000000000 0.000000000 0.037037037 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.022058824 0.036764706 0.014705882 0.000000000 0.000000000
    ##   6 0.015873016 0.023088023 0.025974026 0.005772006 0.007215007
    ##    
    ##              16          17          18          19          20
    ##   1                                                            
    ##   2 0.037037037 0.037037037 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.036764706 0.036764706 0.029411765 0.022058824 0.036764706
    ##   6 0.037518038 0.018759019 0.010101010 0.004329004 0.034632035
    ##    
    ##              21          22          23          24          25
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.037037037 0.037037037 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.022058824 0.014705882 0.022058824 0.007352941
    ##   6 0.000000000 0.005772006 0.010101010 0.011544012 0.015873016
    ##    
    ##              26          27          28          29          30
    ##   1                                                            
    ##   2 0.000000000 0.074074074 0.074074074 0.074074074 0.037037037
    ##   3 0.000000000 0.111111111 0.333333333 0.222222222 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.058823529 0.139705882 0.051470588 0.066176471
    ##   6 0.000000000 0.059163059 0.194805195 0.050505051 0.044733045
    ##    
    ##              31          32          33          34          35
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.037037037 0.074074074
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.007352941 0.014705882 0.022058824 0.022058824
    ##   6 0.000000000 0.004329004 0.004329004 0.033189033 0.012987013
    ##    
    ##              36          37          38          39          40
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.074074074
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.029411765
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.064935065
    ##    
    ##              41          42          43          44          45
    ##   1                                                            
    ##   2 0.111111111 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.022058824 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.030303030 0.001443001 0.000000000 0.000000000 0.000000000
    ##    
    ##              46          47          48          49          50
    ##   1                                                            
    ##   2 0.000000000 0.037037037 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.007352941 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.002886003 0.007215007 0.000000000 0.000000000 0.000000000
    ##    
    ##              51          52          53          54          55
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.037037037 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.333333333
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.022058824
    ##   6 0.000000000 0.000000000 0.000000000 0.004329004 0.015873016
    ##    
    ##              56          57          58          59          60
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##              61          62          63          64          65
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.001443001
    ##    
    ##              66          67          68          69          70
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.007352941 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.028860029 0.020202020 0.005772006
    ##    
    ##              71          72          73          74          75
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##              76          77          78          79          80
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##              81          82          83          84          85
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##              86          87          88          89          90
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##              91          92          93          94          95
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##              96          97          98          99         100
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             101         102         103         104         105
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             106         107         108         109         110
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             111         112         113         114         115
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             116         117         118         119         120
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             121         122         123         124         125
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             126         127         128         129         130
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             131         132         133         134         135
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             136         137         138         139         140
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             141         142         143         144         145
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             146         147         148         149         150
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             151         152         153         154         155
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             156         157         158         159         160
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             161         162         163         164         165
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             166         167         168         169         170
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             171         172         173         174         175
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             176         177         178         179         180
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             181         182         183         184         185
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             186         187         188         189         190
    ##   1                                                            
    ##   2 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   4                                                            
    ##   5 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
    ##    
    ##             191         192         193
    ##   1                                    
    ##   2 0.000000000 0.000000000 0.000000000
    ##   3 0.000000000 0.000000000 0.000000000
    ##   4                                    
    ##   5 0.000000000 0.000000000 0.000000000
    ##   6 0.000000000 0.000000000 0.000000000

Dispersal Kernels
=================

Instead of calculating the distance between each grid cell, I'm going to represent the grid via row and column numbers. This will ignore the actual 'in water' distance travelled, but it is a lot quicker for demonstration purposes.

First, we'll extract the projected coordinates of the center of each cell.

``` r
biggrid$row <- round(coordinates(spTransform(biggrid,CRS(proj))))[,2]
biggrid$col <- round(coordinates(spTransform(biggrid,CRS(proj))))[,1]
```

Then, we can standardize those coordinates into row and column numbers.

``` r
rowsize <- sort(unique(biggrid$row))[2]-sort(unique(biggrid$row))[1]
biggrid$row <- round((biggrid$row-min(biggrid$row))/rowsize)

colsize <- sort(unique(biggrid$col))[2]-sort(unique(biggrid$col))[1]
biggrid$col <- round((biggrid$col-min(biggrid$col))/colsize)
```

Then, since we already know which grid cell each particle originated `grid0` and where each particle settled `grid`, we can now calculate the difference in column/row numbers. We multiply that difference by the `rowsize` or `colsize` to get the vertical (`NS`) distance and horizontal (`WE`) distance. Finally we can also plot the dispersal kernel.

``` r
dataset$NS <- (biggrid$row[dataset$grid]-biggrid$row[dataset$grid0])*rowsize
dataset$WE <- (biggrid$col[dataset$grid]-biggrid$col[dataset$grid0])*colsize

hist(dataset$NS, col='#d95f0280',
     breaks=seq(-1.5*10^6,1.5*10^6,10^5),
     xlim=c(
         min(c(dataset$NS,dataset$WE),na.rm=T),
         max(c(dataset$NS,dataset$WE),na.rm=T)
     )
)
hist(dataset$WE, col='#7570b380', add=T,
     breaks=seq(-1.5*10^6,1.5*10^6,10^5)
)
```

![](index_files/figure-markdown_github/generate%20histogram-1.png)
