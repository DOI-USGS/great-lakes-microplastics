$(document).ready(function(){

  // init controller
  var controller = new ScrollMagic.Controller();

  // Send a GA4 event the first time each section scrolls into view
  var sections = ['intro', 'environmentalEffects', 'environmentalHazards', 'beadBan', 'relativeAbundance', 'landUse', 'conclusion', 'extras'];
  var seen = {};
  $.each(sections, function(i, id) {
    new ScrollMagic.Scene({
      triggerElement: "#" + id
    })
    .on('enter', function() {
      if (!seen[id] && typeof gtag === 'function') {
        seen[id] = true;
        gtag('event', 'section', {
          'section': id
        });
      }
    })
    .addTo(controller);
  });
});
