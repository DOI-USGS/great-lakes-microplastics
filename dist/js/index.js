$(document).ready(function(){

  // init controller
  var controller = new ScrollMagic.Controller();

  new ScrollMagic.Scene({
	  triggerElement: "#header"
	})
	.on('enter', function() {
	  ga('send', 'event', 'section', 'header');
	})
	.addTo(controller);
  new ScrollMagic.Scene({
	  triggerElement: "#intro"
	})
	.on('enter', function() {
	  ga('send', 'event', 'section', 'intro');
	})
	.addTo(controller);
  new ScrollMagic.Scene({
	  triggerElement: "#relativeAbundance"
	})
	.on('enter', function() {
	  ga('send', 'event', 'section', 'relativeAbundance');
	})
	.addTo(controller);
  new ScrollMagic.Scene({
	  triggerElement: "#environmentalEffects"
	})
	.on('enter', function() {
	  ga('send', 'event', 'section', 'environmentalEffects');
	})
	.addTo(controller);
  new ScrollMagic.Scene({
	  triggerElement: "#landUse"
	})
	.on('enter', function() {
	  ga('send', 'event', 'section', 'landUse');
	})
	.addTo(controller);
  new ScrollMagic.Scene({
	  triggerElement: "#footer"
	})
	.on('enter', function() {
	  ga('send', 'event', 'section', 'footer');
	})
	.addTo(controller);
  // templating javascript is iffy (this line needed to end curly-brace)
});