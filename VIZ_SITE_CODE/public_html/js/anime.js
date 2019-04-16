var current_image = 0;
var play_mode = 0;
var oldIndex = 0;
var last_pict = 47;
var nameSequence = "RUN (Today's Forecaset Only)";
var nameStop = "Stop";

function GoToImage(number) {
  play_mode = 0;
  viewPic(number);
  if (typeof document.control_form !== 'undefined') {
  document.control_form.SequenceForm.value = nameSequence;
}
}

function GoToImageG(number) {
  play_mode = 0;
  viewPicG(number);
  if (typeof document.control_form !== 'undefined') {
    document.control_form.SequenceForm.value = nameSequence;
  }
}

function animate_fwd() {
  if (play_mode == 0) {
    return;
  }
  if (current_image > last_pict) {
    current_image = 0;
    setTimeout("animate_fwd()", 1500);
  } else {
    viewPic(current_image);
    current_image++;
    setTimeout("animate_fwd()", 300);
  }
}

function viewPic(nr) {
  if (typeof document.animation !== 'undefined') {
  document.animation.src = theImages[nr].src;
}
  oldIndex = nr;
  nextslideindex = nr;
}

function viewPicG(nr) {
  var animationG = document.getElementById("animation")
  if (typeof animationG !== null && typeof animationG !== 'undefined') {
    document.getElementById("animation").src = theImages[nr].src;
  }
  oldIndex = nr;
  nextslideindex = nr;
}

function startAnimation() {
  if (play_mode == 0) {
    document.control_form.SequenceForm.value = nameStop;
    play_mode = 1;
    current_image = 0;
    animate_fwd();
  } else {
    GoToImage(0)
  }
}

function rotateimage(e) {
  var evt = window.event || e
  var delta = evt.detail ? evt.detail * (-120) : evt.wheelDelta
  nextslideindex = (delta <= -120) ? nextslideindex + 1 : nextslideindex - 1
  nextslideindex = (nextslideindex < 0) ? theImages.length - 1 : (nextslideindex > theImages.length - 1) ? 0 : nextslideindex
  GoToImage(nextslideindex)
  if (evt.preventDefault)
    evt.preventDefault()
  else
    return false

}
var viewportwidth = 600;
var viewporthalfwidth = 300;
var xposclick = 400;

function clickflip(e) {
  var evt = window.event || e
  if (evt.pageX)
    xposclick = evt.pageX
  if (typeof window.innerWidth != 'undefined') {
    viewportwidth = window.innerWidth
  }
  viewporthalfwidth = viewportwidth / 2
  if (xposclick < viewporthalfwidth)
    nextslideindex = nextslideindex - 1
  else
    nextslideindex = nextslideindex + 1
  nextslideindex = (nextslideindex < 0) ? theImages.length - 1 : (nextslideindex > theImages.length - 1) ? 0 : nextslideindex
  GoToImage(nextslideindex)
}

function keyflip(e) {
  var evt = window.event || e //equalize event object
  if ((evt.keyCode == '37') || (evt.keyCode == '38'))
    nextslideindex = nextslideindex - 1
  if ((evt.keyCode == '32') || (evt.keyCode == '39') || (evt.keyCode == '40'))
    nextslideindex = nextslideindex + 1
  nextslideindex = (nextslideindex < 0) ? theImages.length - 1 : (nextslideindex > theImages.length - 1) ? 0 : nextslideindex
  GoToImage(nextslideindex)
}
