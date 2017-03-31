sparkvis
================

**sparkvis** adds the necessary methods to **ggvis** to allow it to handle **sparklyr's** *tbl\_spark* class data sets.

One of the methods is a **compute\_bin** variance to calculate a histogram's bins inside Spark and then collect results, thus reducing the amount of data brought over into memory. **Limitation**: The function only works with straigth column names not formulas, so while *~mpg* will work, *~factor(am)* will not.

Future plans are to create more compute methods that will open other layers to *tbl\_spark* data sets.

To see an example click here: [sparkvis R Markdown](http://colorado.rstudio.com:3939/content/429/README-publish.html)
