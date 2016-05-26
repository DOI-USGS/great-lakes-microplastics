$(document).ready(function(){

//Accordion
function close_accordion_section(){
	$('.accordion .accordion-section-title').removeClass('active');
	$('.accordion .accordion-section-content').slideUp(300).removeClass('open');
}

$('.accordion-section-title').on('click', function(e){
	// Grab current anchor value
    var currentAttrValue = $(this).attr('href');
	
	if($(e.target).is('.active')) {
		close_accordion_section();
	}else {
     	close_accordion_section();
		
		// Add active class to section title
        $(this).addClass('active');
        // Open up the hidden content panel
        $('.accordion ' + currentAttrValue).slideDown(300).addClass('open'); 
	}
	
	e.preventDefault();
});

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

//Fish
new ScrollMagic.Scene({
	triggerElement: "#figure2"
	})
	.setClassToggle("#fish", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//Oyster					
new ScrollMagic.Scene({
	triggerElement: "#figure2",
	offset:500
	})
	.setClassToggle("#oyster", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//Stream		
new ScrollMagic.Scene({
	triggerElement: "#figure2",
	offset:1000
	})
	.setClassToggle("#stream", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//Lakes	
new ScrollMagic.Scene({
	triggerElement: "#figure2",
	offset:1500
	})
	.setClassToggle("#lakes", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);
	
// show pin state
function updateBox (e) {
	if (e.type === "enter") {
		$("#pin p").text("Pinned.");
	} else {
		$("#pin p").text("Unpinned.");
		}
}
	
//Figure3 appears	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	offset:100
	})
	.setClassToggle("#landUseFig", "awake") // add class toggle
	.on("enter leave", updateBox)
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//Figure3 is pinned	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	duration:200,
	offset:-50
	})
	.setPin('#figure3')
	.triggerHook(0)
	.on("enter leave", updateBox)
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//First text disappears	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	offset:150
	})
	.setClassToggle("#first", "gone") // add class toggle
	.triggerHook(0)
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//Second text appears	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	offset:150
	})
	.setClassToggle("#second", "here") // add class toggle
	.triggerHook(0)
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);
	
//Sorting	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	duration:200,
	offset:150
	})
	.setPin('#figure3')
	.triggerHook(0)
	.on("enter leave", updateBox)
	.on("enter", function() {
    	triggerOnce("landUseTrigger");
  	})
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);
	
$('#beads-table').on('click', function(){
	$('#beads').toggle('slow');
});

$('#fiberlines-table').on('click', function(){
	$('#fibers').toggle('slow');
});

});