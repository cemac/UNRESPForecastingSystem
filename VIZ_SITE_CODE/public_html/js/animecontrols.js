var animation = document.getElementById("animation")
var nextslideindex = 0
var mousewheelevt = (/Firefox/i.test(navigator.userAgent)) ? "DOMMouseScroll" : "mousewheel"
if (typeof animation !== 'undefined' && animation !== null) {
  if (animation.attachEvent)
    animation.attachEvent("on" + mousewheelevt, rotateimage)
  else if (animation.addEventListener)
    animation.addEventListener(mousewheelevt, rotateimage, false, passive = true)
  if (animation.attachEvent)
    animation.attachEvent('onclick', clickflip)
  else if (animation.addEventListener)
    animation.addEventListener('click', clickflip, false)
  if (window.attachEvent)
    window.attachEvent('keydown', keyflip)
  else if (window.addEventListener) //WC3 browsers
    window.addEventListener('keydown', keyflip, false)
}
