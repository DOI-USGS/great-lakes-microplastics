$(document).ready(function(){
// init controller
var controller = new ScrollMagic.Controller();
var triggers = {};
var triggerOnce = function(event) {
  if (!triggers.hasOwnProperty(event)) {
    triggers[event] = false;
  }
  if (!triggers[event]) {
      var e = new Event(event);
      document.dispatchEvent(e);
      triggers[event] = true;
    }
}

  new ScrollMagic.Scene({
    triggerElement: "#landUseFig"
  })
  .on("enter", function() {
    triggerOnce("landUseTrigger");
  })
  .addIndicators()
  .addTo(controller);

});