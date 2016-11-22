
/*LOGIN NAV*/
$(document).ready(function () {
	
	$('.login').click(function () {
		$('#login').toggle();			
	});


/* LOGIN .MOSTRAR */
$(document).ready(function () {
	
	$('.loginmenu').click(function () {
		$('#login_2').toggle();			
	});
	

});




/*RETRAIR MENU	*/
	setInterval(function(){
		var x = window.innerWidth;
		$("#teste").html(x);
		if(x >= 1023)
		{
			$('.nav_responsiva').css("display","none");
		}
	},100);
	


});




$(window).bind('scroll', function () {
      if ($(window).scrollTop() >= 400) {
//		  alert($(window).scrollTop());
        $("#menu").addClass('mostrar');
		$("#menu").css('display','block')
      } else {
//        $("#menu").removeClass('mostrar');
		$("#menu").hide();
      }
});


/*MENU RESPONSIVO*/
$(document).ready(function () {
	
	$('.menu_responsivo').click(function () {
		$('.nav_responsiva').fadeToggle();			
	});


	$('#btnEntrar').click(function() {
    	var dados = $("#login").serialize();
		//alert('senha incorreta'+dados);
	   jQuery.ajax({
				type: "GET", // Método passado via ajax
				url: "logar.php", //Caminho para chamar o ajax
				dataType:"html", // html, text, json.
				data:dados, //variaveis
				success:function(response){
					//alert(response);
					if (response === "0"){
						$("#resposta").html("Senha Incorreta");	//Coloca a mensagem do PHP na DIV resposta					
					}
					else
					{
						window.location="usuario_index.php";
					}
				}
		});     
	});
	



/*FECHAR MENU RESPONSIVO*/

function fecha() {
window.close();

}
});