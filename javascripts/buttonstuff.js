var acc = document.getElementsByClassName("material1");
var i;
for (i = 0; i < acc.length; i++) {
    acc[i].onclick = function(){
           this.classList.toggle("active");
           a=this.parentNode.nextElementSibling.classList;
           a.toggle("show")
      }
}

var acc = document.getElementsByClassName("material2");
var i;
for (i = 0; i < acc.length; i++) {
    acc[i].onclick = function(){
           this.classList.toggle("active");
           this.parentNode.nextElementSibling.nextElementSibling.classList.toggle("show");
      }
}
var acc = document.getElementsByClassName("material3");
var i;
for (i = 0; i < acc.length; i++) {
    acc[i].onclick = function(){
           this.classList.toggle("active");
           this.parentNode.nextElementSibling.nextElementSibling.nextElementSibling.classList.toggle("show");
      }
}
