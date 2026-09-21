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
- Switched the page font from Source Sans Pro to Source Sans 3, made headings bold, and unified the narrative text into one 700px column with a single type scale
- Centered the stat blocks beside their silhouettes with flexbox in place of the fixed-height absolute positioning, which the larger text overflowed
- Replaced the Font Awesome kit script with vendored Font Awesome 4.7 webfonts in `dist/fonts/`; the share bar drops Google+ and uses an X icon in place of the Twitter bird, and its links point at the new site URL
- Updated `og:url`, share links, and social media image URLs in `dist/index.html` for the new site URL and S3 image hosting, and added `twitter:title`/`twitter:url` tags
- Rewrote `README.md` to describe the `dist/` site, how to serve it locally, and how the site was originally built
- Updated `code.json` and `CONTRIBUTING.md` to point at the `DOI-USGS` GitHub organization and the new site URL, https://water.usgs.gov/vizlab/microplastics

### Fixed
- Fixed the page's structured data, which still carried the headline and subtitle of an unrelated Vizlab site ("Shifts in fish habitat under climate change") and had a trailing comma that made the JSON invalid
- Fixed the "Skip to main content" link, which pointed at an anchor with a literal `#` in its id; the content is now wrapped in `<main id="main-content">`
- Fixed the `og:title`, which read "Nations's"

## [1.0.0] - 2016-09-08

### Added
- Initial public release of the Microplastics in our Nation's Waterways data visualization at https://owi.usgs.gov/vizlab/microplastics
