$(document).ready(function(){

// Land-use chart: sort the rivers with the toggle above the chart. The
// SVG exposes sortLU (least to most urban) and sortLUrev (back to west to
// east) on its document once it has loaded; each animates for ~1.5 s.
var landUse = document.getElementById('landUseSVG');
var toggles = $('.chart-toggle');
var sorting = false;

function landUseDoc() {
  var doc = landUse && landUse.contentDocument;
  return (doc && typeof doc.sortLU === 'function') ? doc : null;
}

toggles.on('click', function () {
  var btn = $(this);
  var doc = landUseDoc();
  if (sorting || btn.hasClass('is-active') || !doc) { return; }
  sorting = true;
  toggles.removeClass('is-active').attr('aria-pressed', 'false');
  btn.addClass('is-active').attr('aria-pressed', 'true');
  if (btn.data('sort') === 'urban') { doc.sortLU(); } else { doc.sortLUrev(); }
  if (typeof gtag === 'function') {
    gtag('event', 'sort_rivers', { 'order': btn.data('sort') });
  }
  setTimeout(function () { sorting = false; }, 1700);
});

});