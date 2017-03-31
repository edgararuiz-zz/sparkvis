sparkvis
================

**sparkvis** adds the necessary methods to **ggvis** to allow it to handle **sparklyr's** *tbl\_spark* class data sets.

One of the methods is a **compute\_bin** variance to calculate a histogram's bins inside Spark and then collect results, thus reducing the amount of data brought over into memory. **Limitation**: The function only works with straigth column names not formulas, so while *~mpg* will work, *~factor(am)* will not.

Future plans are to create more compute methods that will open other layers to *tbl\_spark* data sets.

### Installation

You can install **sparkvis** from GitHub

``` r
devtools::install_github("edgararuiz/sparkvis")
```

### Load needed libraries

``` r
library(sparklyr)
library(ggvis)
library(sparkvis)
```

### Create a new Spark connection

``` r
conf <- spark_config()
conf$`sparklyr.shell.driver-memory` <- "8G"
sc <- spark_connect(master="local", config = conf, version = "2.1.0")
```

### Copying mtcars to the Spark environment

``` r
spark_mtcars <- copy_to(sc, mtcars)
```

### Comparison

#### Using a standard data frame

``` r
mtcars %>% 
  ggvis(~mpg) %>% 
  layer_histograms(width = 5, stack = FALSE)
```

<!--html_preserve-->

<nav class="ggvis-control"> <a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: <a id="plot_id163770039_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id163770039" data-renderer="svg">SVG</a> | <a id="plot_id163770039_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id163770039" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id163770039_download" class="ggvis-download" data-plot-id="plot_id163770039">Download</a>
</li>
</ul>
</nav>

<script type="text/javascript">
var plot_id163770039_spec = {
  "data": [
    {
      "name": ".0/bin1",
      "format": {
        "type": "csv",
        "parse": {
          "xmin_": "number",
          "xmax_": "number",
          "count_": "number"
        }
      },
      "values": "\"xmin_\",\"xmax_\",\"count_\"\n7.5,12.5,2\n12.5,17.5,10\n17.5,22.5,11\n22.5,27.5,5\n27.5,32.5,3\n32.5,37.5,1"
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n6\n39"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n0\n11.55"
    }
  ],
  "scales": [
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "rect",
      "properties": {
        "update": {
          "stroke": {
            "value": "#000000"
          },
          "fill": {
            "value": "#333333"
          },
          "x": {
            "scale": "x",
            "field": "data.xmin_"
          },
          "x2": {
            "scale": "x",
            "field": "data.xmax_"
          },
          "y": {
            "scale": "y",
            "field": "data.count_"
          },
          "y2": {
            "scale": "y",
            "value": 0
          }
        },
        "ggvis": {
          "data": {
            "value": ".0/bin1"
          }
        }
      },
      "from": {
        "data": ".0/bin1"
      }
    }
  ],
  "legends": [],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "layer": "back",
      "grid": true,
      "title": "mpg"
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "layer": "back",
      "grid": true,
      "title": "count"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 672,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id163770039").parseSpec(plot_id163770039_spec);
</script>
<!--/html_preserve-->
#### Now using a Spark table

``` r
spark_mtcars %>% 
  ggvis(~mpg) %>% 
  layer_histograms(width = 5, stack = FALSE)
```

<!--html_preserve-->

<nav class="ggvis-control"> <a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: <a id="plot_id501839264_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id501839264" data-renderer="svg">SVG</a> | <a id="plot_id501839264_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id501839264" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id501839264_download" class="ggvis-download" data-plot-id="plot_id501839264">Download</a>
</li>
</ul>
</nav>

<script type="text/javascript">
var plot_id501839264_spec = {
  "data": [
    {
      "name": ".0/bin1",
      "format": {
        "type": "csv",
        "parse": {
          "xmin_": "number",
          "xmax_": "number",
          "count_": "number"
        }
      },
      "values": "\"xmin_\",\"xmax_\",\"count_\"\n7.590625,12.590625,2\n12.590625,17.590625,10\n17.590625,22.590625,11\n22.590625,27.590625,5\n27.590625,32.590625,3\n32.590625,37.590625,1"
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n6.090625\n39.090625"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n0\n11.55"
    }
  ],
  "scales": [
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "rect",
      "properties": {
        "update": {
          "stroke": {
            "value": "#000000"
          },
          "fill": {
            "value": "#333333"
          },
          "x": {
            "scale": "x",
            "field": "data.xmin_"
          },
          "x2": {
            "scale": "x",
            "field": "data.xmax_"
          },
          "y": {
            "scale": "y",
            "field": "data.count_"
          },
          "y2": {
            "scale": "y",
            "value": 0
          }
        },
        "ggvis": {
          "data": {
            "value": ".0/bin1"
          }
        }
      },
      "from": {
        "data": ".0/bin1"
      }
    }
  ],
  "legends": [],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "layer": "back",
      "grid": true,
      "title": "mpg"
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "layer": "back",
      "grid": true,
      "title": "count"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 672,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id501839264").parseSpec(plot_id501839264_spec);
</script>
<!--/html_preserve-->
### Closing connection

``` r
spark_disconnect(sc)
```
