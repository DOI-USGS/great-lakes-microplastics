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

// show pin state
function updateBox (e) {
	if (e.type === "enter") {
		$("#pin p").text("Pinned.");
	} else {
		$("#pin p").text("Unpinned.");
		}
}

//Figure3 is pinned	
new ScrollMagic.Scene({
	triggerElement: "#landUse",
	duration:50,
	offset:100
	})
	.setPin('#landUse')
	.triggerHook(0)
	.on("enter leave", updateBox)
	.addTo(controller);
	
//Sorting	
new ScrollMagic.Scene({
	triggerElement: "#landUse",
	duration:50,
	offset:150
	})
	.setPin('#landUse')
	.triggerHook(0)
	.on("enter leave", updateBox)
	.on("enter", function() {
    	triggerOnce("landUseTrigger");
  	})
	.addTo(controller);

$('#referenceTitle').on('click', function(){
	$('#referenceTitle').toggleClass('corners');
	$('#arrow').toggleClass('rotate');
	$('#referencePanels').slideToggle('slow');
});

});