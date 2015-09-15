neurone
=======

`neurone` is an [R-package](https://www.r-project.org/) for reading physiologic data recorded with a [neurOne device](http://www.megaemg.com/products/neurone-eeg-system/).

Installation
------------
To install the `neurone` package in R, proceed as follows in R.

First install the `devtools`-package
```
   install.packages("devtools")
```

You can now install the `neurone` package:
```
   install_github("bwrc/neurone-r")
```

Usage
-----
To read data data recorded with a neurOne device into R:

```
library(neurone)
datapath <- "/tmp/my_recording/"
recording <- read.neurone(datapath)
```

The variable `datapath` here refers to the neurone directory containing the files `Protocol.xml`, `Session.xml` etc.
