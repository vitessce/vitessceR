(window["webpackJsonp"] = window["webpackJsonp"] || []).push([[6],{

/***/ "./node_modules/geotiff/dist-module/compression/packbits.js":
/*!******************************************************************!*\
  !*** ./node_modules/geotiff/dist-module/compression/packbits.js ***!
  \******************************************************************/
/*! exports provided: default */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "default", function() { return PackbitsDecoder; });
/* harmony import */ var _basedecoder_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./basedecoder.js */ "./node_modules/geotiff/dist-module/compression/basedecoder.js");


class PackbitsDecoder extends _basedecoder_js__WEBPACK_IMPORTED_MODULE_0__["default"] {
  decodeBlock(buffer) {
    const dataView = new DataView(buffer);
    const out = [];

    for (let i = 0; i < buffer.byteLength; ++i) {
      let header = dataView.getInt8(i);
      if (header < 0) {
        const next = dataView.getUint8(i + 1);
        header = -header;
        for (let j = 0; j <= header; ++j) {
          out.push(next);
        }
        i += 1;
      } else {
        for (let j = 0; j <= header; ++j) {
          out.push(dataView.getUint8(i + j + 1));
        }
        i += header + 1;
      }
    }
    return new Uint8Array(out).buffer;
  }
}


/***/ })

}]);
//# sourceMappingURL=6.index.js.map