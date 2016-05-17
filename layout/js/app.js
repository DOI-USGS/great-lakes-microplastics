$(document).ready(function(){
// init controller
var controller = new ScrollMagic.Controller();

//FISH
  new ScrollMagic.Scene({
		  triggerElement: "#fishPicContainer",
      duration: "30%",
      offset:100
    })
    .setPin("#fishPic")
    .triggerHook(0.5)
	.addIndicators()
    .addTo(controller);
	
//OYSTER
  new ScrollMagic.Scene({
		triggerElement: "#oysterPicContainer",
      duration: "95%",
	    offset:100
    })
    .setPin("#oysterPic")
    .triggerHook(0.5)
    .addIndicators()
    .addTo(controller);
	
//Stream
  new ScrollMagic.Scene({
		triggerElement: "#streamPicContainer",
      duration: "80%",
	    offset:200
    })
    .setPin("#streamPic")
    .triggerHook(0.5)
    .addIndicators()
    .addTo(controller);
	
//Lake
  new ScrollMagic.Scene({
		triggerElement: "#lakePicContainer",
      duration: "30%",
	    offset:100
    })
    .setPin("#lakePic")
    .triggerHook(0.5)
    .addIndicators()
    .addTo(controller);

});