$(document).ready(function(){
// init controller
var controller = new ScrollMagic.Controller();
	
//FISH
  new ScrollMagic.Scene({
		  triggerElement: "#fishPicContainer",
      duration: 800,
      offset:200
    })
    .setPin("#fishPic")
    .triggerHook(0.5)
	  .addIndicators()
    .addTo(controller);
//OYSTER
  new ScrollMagic.Scene({
		triggerElement: "#oysterPicContainer",
      duration: 500,
	    offset:200
    })
    .setPin("#oysterPic")
    .triggerHook(0.5)
    .addIndicators()
    .addTo(controller);

});