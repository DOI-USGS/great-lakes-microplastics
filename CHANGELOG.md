# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

### Added
- Added a change log
- Added the deployed static site to `dist/`, as the original remake/vizlab build pipeline can no longer be run

### Changed
- Replaced Google Tag Manager and the legacy USGS analytics script with the Vizlab GA4 tag and the federal DAP tag, matching `vue3-template`; converted the section-scroll `ga()` event calls in `js/index.js` (which threw `ReferenceError` on the live site) to `gtag()`
- Updated the USWDS banner, USGS header, and USGS footer markup and links to match `vue3-template`; upgraded bundled USWDS assets from v2.7 to v3.13 and deferred the USWDS script so it runs after the page body exists
- Added the Vizlab pre-footer links (See more visualizations / Get the code) above the footer
- Replaced the collapsible References box at the end of the page with stacked Data Collection, References, and USGS Vizlab authorship sections, matching other Vizlab sites, with published and last-updated dates; references keep their numbering so the in-text superscripts still resolve, and now link through `https://doi.org`
- Added the full 2016 project team and the 2026 update to the page's authorship section, structured data, and the README citation and contributors
- Switched the page font from Source Sans Pro to Source Sans 3, made headings bold, and unified the narrative text into one 700px column with a single type scale (48px banner titles, 28px section headings, 18px body text, 15px captions)
- Stacked the three text-beside-photo sections (known risks, microplastics in rivers, what's next) into the single 700px text column, with each photo as a full-column figure beneath its text, so every narrative section shares one width; only the statistics grid and the land-use chart use the wider 960px figure column
- Moved the "Where in our waterways are microplastics found?" heading above the introductory paragraph, which now ends with the lead-in to the statistics
- Rebuilt the "where microplastics are found" statistics as a compact six-card grid with a silhouette for every data point: the oyster and mussel silhouettes were split out of the shared image, and a new river-sediment silhouette (`images/sediment.svg`) was drawn in the same style
- Added captions: photo credits beneath the brook trout, microplastic sample, and piping plover photos (moved out of `title` tooltips), location labels on the full-bleed banner photos, and explanatory captions under both charts, which also describe the interactions in place of the standalone "hover over the graph" sentences
- Gave both chart SVGs the page font and a clearer hierarchy: bold chart and axis titles with lighter units, quieter tick and legend labels, and bold particle-type names in the relative abundance figure
- Removed the translucent dark-red background behind the land-use chart (an `rgb(100,0,0,.5)` rule that older browsers ignored and modern ones render pink); the section is now white like the rest of the page
- Added a "Sort rivers" toggle above the land-use chart (West to east / Least to most urban) that calls the chart's existing sort animations, in place of the scroll-triggered re-sort that users never saw; removed the ScrollMagic pinning hack that drove it
- Replaced the Font Awesome kit script with vendored Font Awesome 4.7 webfonts in `dist/fonts/`; the share bar drops Google+ and uses an X icon in place of the Twitter bird, and its links point at the new site URL
- Updated `og:url`, share links, and social media image URLs in `dist/index.html` for the new site URL and S3 image hosting, and added `twitter:title`/`twitter:url` tags
- Rewrote `README.md` to describe the `dist/` site, how to serve it locally, and how the site was originally built
- Updated `code.json` and `CONTRIBUTING.md` to point at the `DOI-USGS` GitHub organization and the new site URL, https://water.usgs.gov/vizlab/microplastics

- Made the sampling-location labels on the land-use chart smaller
- Made the macro photo of sampled microplastic particles the opening banner, in place of the Bad River landscape, and removed it from the "Microplastics in rivers" section; its photo credit moved to the banner caption, and a web-sized copy (`images/mpsampling-hero.jpg`, 179 KB) is used in place of the 2 MB original
- Enlarged the opening banner title and added a teal accent rule beneath it, a gradient scrim so the title stays legible over the pale particles, and a hover state on the share links; the opening banner is now sized to the space below the USGS header rather than a full `100vh`, which had pushed the share links and photo credit off the first screen
- Accessibility: allowed pinch-zoom (the viewport tag disabled it), gave both chart `<object>`s accessible names, descriptions, and text fallbacks, made the pre-footer links a `<nav>` landmark, reduced the page to one `h1` with the other banner titles as `h2` and in-section headings as `h3`, labelled the reference superscript links ("Reference 1"), moved the `<noscript>` notice into the body, increased the contrast of the banner captions and the dimming behind banner titles, and turned off the banner parallax under `prefers-reduced-motion`

### Fixed
- Fixed the page's structured data, which still carried the headline and subtitle of an unrelated Vizlab site ("Shifts in fish habitat under climate change") and had a trailing comma that made the JSON invalid
- Fixed the "Skip to main content" link, which pointed at an anchor with a literal `#` in its id; the content is now wrapped in `<main id="main-content">`
- Fixed the `og:title`, which read "Nations's"

## [1.0.0] - 2016-09-08

### Added
- Initial public release of the Microplastics in our Nation's Waterways data visualization at https://owi.usgs.gov/vizlab/microplastics
