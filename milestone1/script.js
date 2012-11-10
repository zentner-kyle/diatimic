function buttonActions(){
  $("div.first").slideUp(300).slideDown(100);
  $(this).oneTime(1000, periodicTask);
}

function periodicTask() {
  console.log("Period task called")
  $("div.second").slideUp(300).slideDown(300);
}