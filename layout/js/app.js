$(document).ready(function(){
	
// init controller
var controller = new ScrollMagic.Controller();
	
//FISH
new ScrollMagic.Scene({
		triggerElement: "#fishPic",
        duration: 600,
		offset:100
    })
    .setPin("#fishPic")
	.triggerHook(0.5)
	.addIndicators()
    .addTo(controller);
	
//OYSTER
new ScrollMagic.Scene({
		triggerElement: "#fishStat",
        duration: 300,
		offset:200
    })
    .setPin("#fishStat")
	.triggerHook(0.5)
	.addIndicators()
    .addTo(controller);

	

	
});