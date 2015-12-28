
var x;
$( document ).ready(function() {

  
$('#manuBtn').click(function(){
	x = 1;
        $('#autogenModal').modal('hide').one('hidden.bs.modal',function(){
                $('#summaryBSModal').modal('show');
        });
});

$('#gBack').click(function(x){

	if(window.x == '1'){
	   $('#summaryBSModal').modal('hide').one('hidden.bs.modal',function(){
		$('#autogenModal').modal('show');
		});
	}
	else{
	  $('#summaryBSModal').modal('hide');
	}
});

$('#downloadSet').click(function(){
   $('#ioAlert').children('div').children('p').html("<p><strong>Your dataset  has been downloaded!</strong> Edit your data then save your work and upload the dataset.</p>")

});

$('.nav-tabs > li:nth-child(2)').click(function(event){
	$('.progress-bar').css("width",0);

});
$('.nav-tabs > li:nth-child(1)').click(function(event){
	$('.progress-bar').css("width",0);


});


function handler1() {
    $('#exProfiles').children().css("width",'100%');
    $(this).one("click", handler2);
}

function handler2() {
 
  $('#exProfiles').children().css("width",'');
  $(this).one("click", handler1);
}
$(".sidebar-toggle").one("click", handler1);

}); 



