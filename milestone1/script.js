function buttonActions(){
  $("div.first").slideUp(300).slideDown(100);
  $(this).oneTime(1000, periodicTask);
}

function periodicTask() {
  console.log("Period task called")
  $("div.second").slideUp(300).slideDown(300);
}

$(document).ready(function() {
    //and then change this code so that your callback gets run
    //when the button gets clicked instead of mine.
    // **by the way, this is jQuery!
  $("button").click(buttonActions);
  $('#prettybutton').click(buttonActions);
});

$(document).ready(function(){

  $("#ball_holder").everyTime(10, function(){
    $("#ball_holder").animate({top:"184px"}, 400).animate({top:"0px"}, 370);
  });

  $("#start").click(function(){
    $("#shoot").everyTime(10, function(){
      $("#shoot").animate({left:"700px"}, 2500).animate({left:"10"}, 2500);
    });
  });
  $("#stop").click(function(){
    $("#shoot").stop(true).stopTime();
  });

});