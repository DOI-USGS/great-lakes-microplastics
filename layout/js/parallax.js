
function parallax(){
	
// init controller
var controller = new ScrollMagic.Controller();

// create a scene
new ScrollMagic.Scene({
        duration:400,
		offset:50
    })
    .setPin("#pinMe") // pins the element for the the scene's duration
	.addIndicators()
	.triggerHook(0)
    .addTo(controller); // assign the scene to the controller	
}