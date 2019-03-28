window.onload = function() {
  'use strict';

  var Picker = window.Picker;
  var input = document.querySelector('.docs-date');
  var pickerContainer = document.querySelector('.docs-picker-container');
  var options = {
    show: function(e) {
      console.log(e.type);
    },
    shown: function(e) {
      console.log(e.type);
    },
    hide: function(e) {
      console.log(e.type);
    },
    hidden: function(e) {
      console.log(e.type);
    },
    pick: function(e) {
      console.log(e.type);
    }
  };
  var picker = new Picker(input, options);

  console.log(picker);

  input.addEventListener('change', function(e) {
    console.log(e.type);
  }, false);

  input.addEventListener('show', function(e) {
    console.log(e.type);
  }, false);

  input.addEventListener('shown', function(e) {
    console.log(e.type);
  }, false);

  input.addEventListener('hide', function(e) {
    console.log(e.type);
  }, false);

  input.addEventListener('hidden', function(e) {
    console.log(e.type);
  }, false);

  input.addEventListener('pick', function(e) {
    console.log(e.type);
  }, false);

  document.querySelector('.docs-options').addEventListener('change', function(e) {
    if (!picker) {
      return;
    }

    var target = e.target;
    var type = target.type;
    var name = target.name;
    var value = type === 'checkbox' ? target.checked : target.value;
    var relatedElement;
    pickerContainer.style.display = 'block';
    switch (name) {
      case 'container':
        if (value) {
          value = pickerContainer;
          pickerContainer.style.display = 'block';
        } else {
          pickerContainer.style.display = 'none';
        }

        break;

      case 'inline':
        relatedElement = document.querySelector('input[name="container"]');

        if (!relatedElement.checked) {
          relatedElement.click();
        }

        break;

      case 'language':
        relatedElement = document.querySelector('input[name="format"]');

        setTimeout(function() {
          relatedElement.placeholder = relatedElement.value = picker.options.format;
        }, 0);
        break;

        // No default
    }

    options[name] = value;
    picker.destroy();
    picker = new Picker(input, options);
  }, false);

  document.querySelector('.docs-methods').addEventListener('click', function(e) {
    if (!picker) {
      return;
    }

    var target = e.target;
    var method = target.getAttribute('data-method');
    var relatedTarget;
    var result;

    if (method) {
      result = picker[method].apply(picker, JSON.parse(target.getAttribute('data-args')));
      relatedTarget = target.getAttribute('data-related-target');

      if (relatedTarget) {
        document.querySelector(relatedTarget).value = result;
      }

      if (method === 'destroy') {
        picker = null;
      }
    }
  });

 

  if (typeof hljs !== 'undefined') {
    hljs.initHighlightingOnLoad();
  }
  if (result) {
  return result
}
};
