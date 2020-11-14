HTMLWidgets.widget({
  name: 'vitessce',
  type: 'output',
  factory: function(el, width, height) {
    // TODO: define shared variables for this instance

    return {
      renderValue: function(x) {
        renderVitessce(el, x.config, width, height, x.theme, x.port);
      },
      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});
