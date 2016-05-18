# great-lakes-microplastics
Microplastics in the Great Lakes


Visualization Preparation Pipeline
----------------------------------

Data become figures by flowing along this path:

source `-1->` data `-2->` munge `-3->` visualize

The functions implementing each arrow are in the `scripts` folder and/or in the VIZLAB R package. For example, the function used to implement `-1->` is `readData()`, defined in `scripts/lib/readData.R`. The functions used to implement `-2->` include `mungeRelativeAbundance`, defined in `scripts/1_munge/mungeRelativeAbundance.R`.

The overall flow is controlled by the `remake::make()` function, which uses three remake yaml files, each of which controls one of the above arrows. One arrow / remake yaml describes one processing stage for all products at once. The remake yaml files describe which products need to be made at that stage and which functions are required to make each product. The three yaml files are:

`-1->`: data.yaml (e.g., `remake::make(remake_file='data.yaml')`)

`-2->`: munge.yaml (e.g., `remake::make(remake_file='munge.yaml')`)

`-3->`: figures_R.yaml (non-R figures will be described elsewhere) (e.g., `remake::make(remake_file='figures_R.yaml')`)

`-4->`: layout.yaml (e.g., `remake::make(remake_file='layout.yaml')`)

Each remake script also points backward to the preceding remake script, which means that a single call to `remake::make(remake_file='figures_R.yaml')` is enough to ensure that the entire pipeline is up to date.

To start a simple local server, navigate to the target folder in a shell, and run the command:

```
python -m SimpleHTTPServer
```


Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey  (USGS), an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."