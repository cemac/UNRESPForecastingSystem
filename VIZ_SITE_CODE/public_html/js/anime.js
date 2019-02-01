var current_image;
var play_mode=0;
var oldIndex=0;
function GoToImage(number){
        play_mode=0;
        viewPic(number);
        document.control_form.SequenceForm.value=nameSequence;
}
function GoToImageG(){
        var iframe = document.createElement("iframe"),
        iframeWindow;
        iframe.src = "google_concrec010001.html";
        document.body.appendChild(iframe);
        iframeWindow = iframe.contentWindow || iframe.contentDocument.parentWindow;
        iframeWindow.onload = function(){
        };
}
function animate_fwd(){
        if(play_mode==0){return;}
        if(current_image>last_pict){
                current_image=0;
                setTimeout("animate_fwd()",1500);
        } else {
                viewPic(current_image);
                current_image++;
                setTimeout("animate_fwd()",300);
        }
}
function viewPic(nr){
        document.animation.src=theImages[nr].src;
        document.anchors[oldIndex].style.backgroundColor='white';
        document.anchors[nr].style.backgroundColor='LightSkyBlue ';
        oldIndex=nr;
        nextslideindex=nr;
}
function startAnimation(){
        if(play_mode==0){
                document.control_form.SequenceForm.value=nameStop;
                play_mode=1;
                current_image=0;
                animate_fwd();
        } else {GoToImage(0)}
}
function rotateimage(e){
    var evt=window.event || e
    var delta=evt.detail? evt.detail*(-120) : evt.wheelDelta
    nextslideindex=(delta<=-120)? nextslideindex+1 : nextslideindex-1
    nextslideindex=(nextslideindex<0)? theImages.length-1 : (nextslideindex>theImages.length-1)? 0 : nextslideindex
    GoToImage(nextslideindex)
    if (evt.preventDefault)
        evt.preventDefault()
    else
        return false

}
var viewportwidth=600;
var viewporthalfwidth=300;
var xposclick=400;
function clickflip(e){
    var evt=window.event || e
    if (evt.pageX)
      xposclick = evt.pageX
    if (typeof window.innerWidth != 'undefined')
      { viewportwidth = window.innerWidth  }
    viewporthalfwidth=viewportwidth/2
    if (xposclick<viewporthalfwidth)
        nextslideindex=nextslideindex-1
    else
        nextslideindex=nextslideindex+1
    nextslideindex=(nextslideindex<0)? theImages.length-1 : (nextslideindex>theImages.length-1)? 0 : nextslideindex
    GoToImage(nextslideindex)
}
function keyflip(e){
    var evt=window.event || e //equalize event object
    if ((evt.keyCode == '37') || (evt.keyCode == '38'))
      nextslideindex=nextslideindex-1
    if ((evt.keyCode == '32') || (evt.keyCode == '39') || (evt.keyCode == '40'))
      nextslideindex=nextslideindex+1
    nextslideindex=(nextslideindex<0)? theImages.length-1 : (nextslideindex>theImages.length-1)? 0 : nextslideindex
    GoToImage(nextslideindex)
}
