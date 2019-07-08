// Scale image (available on Remarkjs GH page)
remark.macros.scale = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + percentage + '" />';
};

// Image top right
remark.macros.cube = function () {
  var url = "images/rubicub.png";
  return '<img style="float: right; margin: 0px 0px 5px 55px; width:150px" src="' + url + '"/>';
};


// Font-awesome icons (versio 4.7)
remark.macros.faic = function (size = 2) {
  var name = this;
  return '<i class="fa fa-'+ name + ' fa-'+ size + 'x" aria-hidden="true"></i>';
};

// Academicons
remark.macros.acic = function (size = 2) {
  var name = this;
  return '<i class="ai ai-'+ name + ' ai-'+ size + 'x" aria-hidden="true"></i>';
};


// toupper (see https://github.com/gnab/remark/issues/72)
remark.macros.upper = function () {
  // `this` is the value in the parenthesis, or undefined if left out
  return this.toUpperCase();
};
