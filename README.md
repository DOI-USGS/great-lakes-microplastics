# Microplastics in our Nation's Waterways

> _A newer version of the software may be available. See https://github.com/DOI-USGS/great-lakes-microplastics/releases to view all releases._

This repo contains the source for a data visualization website exploring microplastics in U.S. waterways. Microplastics are the miniscule plastic fragments (smaller than 0.04 inch) that fall off of decomposing plastic bottles and bags, and are intentionally manufactured into some toothpastes and lotions. The site summarizes where microplastics have been found (freshwater fish, shellfish, river water and sediment, and the Great Lakes), their known risks, and what USGS and SUNY Fredonia scientists learned by sampling 29 Great Lakes tributaries: fibers, not microbeads, make up most of the microplastics in rivers, and urban watersheds tend to have higher concentrations of fragments, films, and foams.

**The data visualization website can be viewed at [https://water.usgs.gov/vizlab/microplastics](https://water.usgs.gov/vizlab/microplastics).**

The site was originally published on 2016-09-08 at `owi.usgs.gov/vizlab/microplastics` (later `labs.waterdata.usgs.gov/visualizations/microplastics`) and developed at [github.com/USGS-VIZLAB/great-lakes-microplastics](https://github.com/USGS-VIZLAB/great-lakes-microplastics), which has since moved to this repository.

## Repository contents

The repository has two parts: the **deployed static site** in `dist/`, and the **original build pipeline** at the root, which is retained for the historical record but can no longer be run (see [How the site was built](#how-the-site-was-built)).

### `dist/` – the deployed website

`dist/` is a snapshot of the site as served in production. It is plain HTML/CSS/JS with no build step:

* `dist/index.html` – the full visualization page
* `dist/relativeAbundanceFig-*.svg` – the interactive "relative abundance of microplastic types" figure (desktop, mobile, and IE variants)
* `dist/landUseFig-*.svg` – the interactive "microplastics vs. land use" figure (desktop, mobile, and IE variants)
* `dist/js/` – application code (`app.js` for the ScrollMagic section pinning, `index.js` for section analytics events) and vendored libraries (jQuery, Handlebars, USWDS)
* `dist/css/` – the site's own styles (`main.css`, `normalize.css`)
* `dist/stylesheets/` – the USGS header/footer styles (`common.css`, `custom.css`), the USWDS stylesheet, and `vizlab-template.css`, which ports the banner, pre-footer, layout, and typography from `vue3-template`
* `dist/fonts/` – Font Awesome 4.7 web fonts for the header, footer, and share icons
* `dist/images/` – section banner photos (`keynotes/`), silhouettes and photos used in the page, the social media thumbnail, and logos
* `dist/img/` – USWDS banner icons

Changes to the live site should be made directly to the files in `dist/`.

### Root – the original build pipeline

The remaining top-level files and folders (`viz.yaml`, `*.yaml`, `scripts/`, `layout/`, `data/`, `images/`, `Dockerfile`, `Makefile`) are the R-based pipeline that originally generated the site. They are kept for provenance and are not maintained. `target/` is the pipeline's build output as last committed in 2016; it predates the USWDS banner and analytics changes that were later made to the live site directly, so `dist/` is the authoritative copy.

## Viewing the website locally

Because the site is plain HTML/CSS/JS, no dependencies need to be installed. Clone the repo and serve the `dist/` directory with any static file server, for example:

```sh
cd dist

# Python 3
python3 -m http.server 8000

# or with Node
npx serve .
```

Then open [http://localhost:8000](http://localhost:8000) in your browser. Opening `index.html` directly from the filesystem will not work, because the interactive SVG figures are loaded via HTTP requests.

## How the site was built

The site was built in 2016 with [remake](https://github.com/richfitz/remake) and an early version of the USGS Vizlab tooling that became the [vizlab](https://github.com/USGS-VIZLAB/vizlab) R package. `viz.yaml` at the root of this repo describes the data, figures, and page sections; the four remake files (`data.yaml` → `munge.yaml` → `figures_R.yaml` → `layout.yaml`) chain the pipeline stages, with the corresponding R scripts in `scripts/`. Running `remake::make(remake_file = "layout.yaml")` (or the `Dockerfile` and `Makefile`, which wrap that call) executed the pipeline, which:

1. **Fetched** the Great Lakes tributaries sampling data (Baldwin and others, 2016) from a ScienceBase item, along with the hand-compiled statistics in `data/envEffectsData.csv` and the narrative text in `data/siteText.yaml`.
2. **Processed** the data into the relative-abundance and land-use summaries the figures are drawn from.
3. **Visualized** the results by rendering the two interactive SVG figures in R with [gsplot](https://github.com/USGS-R/gsplot) and [dinosvg](https://github.com/jread-usgs/dinosvg), in desktop, mobile, and IE variants, and assembling the page from the Mustache templates in `layout/templates/`.
4. **Published** the assembled site to `target/`, which was then synced to the web server.

The pipeline depended on the ScienceBase item it fetched from (no longer public), on pinned 2016 versions of its R packages, and on the USGS-hosted CRAN mirror at `owi.usgs.gov/R`, none of which are still available. **It cannot be re-run**, which is why the built output is now committed in `dist/`.

## Data sources

* Baldwin, A.K., Corsi, S.R., and Mason, S.A., 2016, Plastic Debris in 29 Great Lakes Tributaries: Relations to Watershed Attributes and Hydrology: Environmental Science & Technology, v. 50, no. 19, p. 10377–10385. [https://doi.org/10.1021/acs.est.6b02917](https://doi.org/10.1021/acs.est.6b02917)

The statistics cited in the page's introduction are drawn from the studies listed in its References section.

## Citation

DeCicco, L., Read, J., Wernimont, M., Walker, J., Carr, L., Appling, A., Read, E., and Nell, C. 2016. Microplastics in our Nation's Waterways. U.S. Geological Survey software release. Reston, VA. https://github.com/DOI-USGS/great-lakes-microplastics

## Contributors

Laura DeCicco, Jordan Read, Marty Wernimont, Jordan Walker, Lindsay Carr, Alison Appling, Emily Read, and Cee Nell.

## Point of contact

Cee Nell ([cnell@usgs.gov](mailto:cnell@usgs.gov)), USGS Vizlab

## Additional information
* We welcome contributions from the community. See the [guidelines for contributing](CONTRIBUTING.md) to this repository.
* [Disclaimer](DISCLAIMER.md)
* [License](LICENSE.md)
