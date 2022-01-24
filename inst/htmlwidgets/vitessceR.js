HTMLWidgets.widget({
  name: 'vitessceR',
  type: 'output',
  factory: function(el, width, height) {
    // TODO: define shared variables for this instance

    return {
      renderValue: function(x) {
        render_vitessce(el, x.config, width, height, x.theme);
      },
      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});
