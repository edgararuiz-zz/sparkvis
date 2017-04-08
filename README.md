sparkvis
================

**sparkvis** adds the necessary methods to **ggvis** to allow it to handle **sparklyr's** *tbl\_spark* class data sets.

New **layer**:

-   layer\_raster

New **compute**:

-   compute\_raster

Currently implemented **layers**:

-   layer\_bars

-   layer\_boxplots

-   layer\_histograms

**Limitation**: The functions only works with straigth column names not formulas, so while *~mpg* will work, *~factor(am)* will not. There is a documented workaround for that in the example document.

To see an R Markdown with samples of everything that is implemented click here: [sparkvis R Markdown](http://colorado.rstudio.com:3939/content/431/README-publish.html)
