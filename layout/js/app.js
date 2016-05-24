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
	triggerElement: "#figure2"
	})
	.setClassToggle("#fish", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);
					
new ScrollMagic.Scene({
	triggerElement: "#figure2",
	offset:500
	})
	.setClassToggle("#oyster", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);
	
new ScrollMagic.Scene({
	triggerElement: "#figure2",
	offset:1000
	})
	.setClassToggle("#stream", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);
	
new ScrollMagic.Scene({
	triggerElement: "#figure2",
	offset:1500
	})
	.setClassToggle("#lakes", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

  new ScrollMagic.Scene({
    triggerElement: "#landUseFig"
  })
  .on("enter", function() {
    triggerOnce("landUseTrigger");
  })
  .addIndicators()
  .addTo(controller);

});