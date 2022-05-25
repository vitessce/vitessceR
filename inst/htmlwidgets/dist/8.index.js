(window["webpackJsonp"] = window["webpackJsonp"] || []).push([[8],{

/***/ "./node_modules/geotiff/dist-module/compression/webimage.js":
/*!******************************************************************!*\
  !*** ./node_modules/geotiff/dist-module/compression/webimage.js ***!
  \******************************************************************/
/*! exports provided: default */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "default", function() { return WebImageDecoder; });
/* harmony import */ var _basedecoder_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./basedecoder.js */ "./node_modules/geotiff/dist-module/compression/basedecoder.js");


/**
 * class WebImageDecoder
 *
 * This decoder uses the browsers image decoding facilities to read image
 * formats like WebP when supported.
 */
class WebImageDecoder extends _basedecoder_js__WEBPACK_IMPORTED_MODULE_0__["default"] {
  constructor() {
    super();
    if (typeof createImageBitmap === 'undefined') {
      throw new Error('Cannot decode WebImage as `createImageBitmap` is not available');
    } else if (typeof document === 'undefined' && typeof OffscreenCanvas === 'undefined') {
      throw new Error('Cannot decode WebImage as neither `document` nor `OffscreenCanvas` is not available');
    }
  }

  async decode(fileDirectory, buffer) {
    const blob = new Blob([buffer]);
    const imageBitmap = await createImageBitmap(blob);

    let canvas;
    if (typeof document !== 'undefined') {
      canvas = document.createElement('canvas');
      canvas.width = imageBitmap.width;
      canvas.height = imageBitmap.height;
    } else {
      canvas = new OffscreenCanvas(imageBitmap.width, imageBitmap.height);
    }

    const ctx = canvas.getContext('2d');
    ctx.drawImage(imageBitmap, 0, 0);

    // TODO: check how many samples per pixel we have, and return RGB/RGBA accordingly
    // it seems like GDAL always encodes via RGBA which does not require a translation

    return ctx.getImageData(0, 0, imageBitmap.width, imageBitmap.height).data.buffer;
  }
}


/***/ })

}]);
//# sourceMappingURL=8.index.js.map