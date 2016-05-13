$(document).ready(function(){
	
	var sections = [
		{file:'js/templates/header.mustache', div: 'header'},
		{file:'js/templates/intro.mustache', div: '#intro'},
		{file:'js/templates/figure1.mustache', div: '#figure1'},
		{file:'js/templates/figure2.mustache', div: '#figure2'},
		{file:'js/templates/footer.mustache', div: 'footer'}
	];
	
	$.each(sections, function(index, value){
			//Gets mustache file
			$.get(value.file, function(data){
				var compiledTemplate = Mustache.render(data);
				//Places mustache file in correct location
				$(value.div).html(compiledTemplate);
			});	
	});
	
	//scrollmagic magic
	// init controller
	var controller = new ScrollMagic.Controller();

	// create a scene
	new ScrollMagic.Scene({
        duration:800,
		offset:50
    })
    .setPin("#pinMe") // pins the element for the the scene's duration
	.addIndicators()
	.triggerHook(0)
    .addTo(controller); // assign the scene to the controller

	
});