(window.webpackJsonp=window.webpackJsonp||[]).push([[7],{1284:function(e,t,r){var a,n,s,l,f,B,i,o,x,k,y,u;n=function(e,t,r,i,a){for(var n,s,l,f,o,u=0,c=e.pixels.numBlocksX,d=e.pixels.numBlocksY,h=Math.floor(e.width/c),m=Math.floor(e.height/d),p=2*e.maxZError,g=Number.MAX_VALUE,w=(r=r||(e.mask?e.mask.bitset:null),s=new t(e.width*e.height),a&&r&&(l=new Uint8Array(e.width*e.height)),new Float32Array(h*m)),x=0;x<=d;x++){var k=x!==d?m:e.height%d;if(0!==k)for(var y=0;y<=c;y++){var b=y!==c?h:e.width%c;if(0!==b){var U,I,v,V,D=x*e.width*m+y*h,M=e.width-b,A=e.pixels.blocks[u];if(A.encoding<2?(U=0===A.encoding?A.rawData:(B(A.stuffedData,A.bitsPerPixel,A.numValidPixels,A.offset,p,w,e.pixels.maxValue),w),I=0):v=2===A.encoding?0:A.offset,r)for(o=0;o<k;o++){for(7&D&&(V=r[D>>3],V<<=7&D),f=0;f<b;f++)128&(V=7&D?V:r[D>>3])?(l&&(l[D]=1),g=(n=A.encoding<2?U[I++]:v)<g?n:g,s[D++]=n):(l&&(l[D]=0),s[D++]=i),V<<=1;D+=M}else if(A.encoding<2)for(o=0;o<k;o++){for(f=0;f<b;f++)g=(n=U[I++])<g?n:g,s[D++]=n;D+=M}else for(g=v<g?v:g,o=0;o<k;o++){for(f=0;f<b;f++)s[D++]=v;D+=M}if(1===A.encoding&&I!==A.numValidPixels)throw"Block and Mask do not match";u++}}}return{resultPixels:s,resultMask:l,minValue:g}},s=function(e){return{fileIdentifierString:e.fileIdentifierString,fileVersion:e.fileVersion,imageType:e.imageType,height:e.height,width:e.width,maxZError:e.maxZError,eofOffset:e.eofOffset,mask:e.mask?{numBlocksX:e.mask.numBlocksX,numBlocksY:e.mask.numBlocksY,numBytes:e.mask.numBytes,maxValue:e.mask.maxValue}:null,pixels:{numBlocksX:e.pixels.numBlocksX,numBlocksY:e.pixels.numBlocksY,numBytes:e.pixels.numBytes,maxValue:e.pixels.maxValue,noDataValue:e.noDataValue}}},l=function(e){for(var t=e.pixels.numBlocksX*e.pixels.numBlocksY,r={},i=0;i<t;i++){var a=e.pixels.blocks[i];0===a.encoding?r.float32=!0:1===a.encoding?r[a.bitsPerPixel]=!0:r[0]=!0}return Object.keys(r)},f=function(e,t,r){var i={},a=new Uint8Array(e,t,10);if(i.fileIdentifierString=String.fromCharCode.apply(null,a),"CntZImage"!==i.fileIdentifierString.trim())throw"Unexpected file identifier string: "+i.fileIdentifierString;t+=10;var n=new DataView(e,t,24);if(i.fileVersion=n.getInt32(0,!0),i.imageType=n.getInt32(4,!0),i.height=n.getUint32(8,!0),i.width=n.getUint32(12,!0),i.maxZError=n.getFloat64(16,!0),t+=24,!r)if(n=new DataView(e,t,16),i.mask={},i.mask.numBlocksY=n.getUint32(0,!0),i.mask.numBlocksX=n.getUint32(4,!0),i.mask.numBytes=n.getUint32(8,!0),i.mask.maxValue=n.getFloat32(12,!0),t+=16,0<i.mask.numBytes){var s=new Uint8Array(Math.ceil(i.width*i.height/8)),l=(n=new DataView(e,t,i.mask.numBytes)).getInt16(0,!0),f=2,o=0;do{if(0<l)for(;l--;)s[o++]=n.getUint8(f++);else for(var u=n.getUint8(f++),l=-l;l--;)s[o++]=u}while(l=n.getInt16(f,!0),(f+=2)<i.mask.numBytes);if(-32768!==l||o<s.length)throw"Unexpected end of mask RLE encoding";i.mask.bitset=s,t+=i.mask.numBytes}else 0==(i.mask.numBytes|i.mask.numBlocksY|i.mask.maxValue)&&(i.mask.bitset=new Uint8Array(Math.ceil(i.width*i.height/8)));n=new DataView(e,t,16),i.pixels={},i.pixels.numBlocksY=n.getUint32(0,!0),i.pixels.numBlocksX=n.getUint32(4,!0),i.pixels.numBytes=n.getUint32(8,!0),i.pixels.maxValue=n.getFloat32(12,!0),t+=16;for(var a=i.pixels.numBlocksX,r=i.pixels.numBlocksY,c=a+(0<i.width%a?1:0),d=r+(0<i.height%r?1:0),h=(i.pixels.blocks=new Array(c*d),0),m=0;m<d;m++)for(var p=0;p<c;p++){var g,w=0,x=e.byteLength-t,x=(n=new DataView(e,t,Math.min(10,x)),{}),k=(i.pixels.blocks[h++]=x,n.getUint8(0));if(w++,x.encoding=63&k,3<x.encoding)throw"Invalid block encoding ("+x.encoding+")";if(2===x.encoding)t++;else{if(0!==k&&2!==k){if(k>>=6,2===(x.offsetType=k))x.offset=n.getInt8(1),w++;else if(1===k)x.offset=n.getInt16(1,!0),w+=2;else{if(0!==k)throw"Invalid block offset type";x.offset=n.getFloat32(1,!0),w+=4}if(1===x.encoding)if(k=n.getUint8(w),w++,x.bitsPerPixel=63&k,k>>=6,2===(x.numValidPixelsType=k))x.numValidPixels=n.getUint8(w),w++;else if(1===k)x.numValidPixels=n.getUint16(w,!0),w+=2;else{if(0!==k)throw"Invalid valid pixel count type";x.numValidPixels=n.getUint32(w,!0),w+=4}}if(t+=w,3!==x.encoding)if(0===x.encoding){var k=(i.pixels.numBytes-1)/4;if(k!==Math.floor(k))throw"uncompressed block has invalid length";g=new ArrayBuffer(4*k),new Uint8Array(g).set(new Uint8Array(e,t,4*k));var w=new Float32Array(g);x.rawData=w,t+=4*k}else 1===x.encoding&&(w=Math.ceil(x.numValidPixels*x.bitsPerPixel/8),k=Math.ceil(w/4),g=new ArrayBuffer(4*k),new Uint8Array(g).set(new Uint8Array(e,t,w)),x.stuffedData=new Uint32Array(g),t+=w)}}return i.eofOffset=t,i},B=function(e,t,r,i,a,n,s){var l,f,o,u,c=(1<<t)-1,d=0,h=0,m=Math.ceil((s-i)/a),p=4*e.length-Math.ceil(t*r/8);for(e[e.length-1]<<=8*p,l=0;l<r;l++)0===h&&(u=e[d++],h=32),t<=h?(o=u>>>h-t&c,h-=t):(o=(u&c)<<(f=t-h)&c,o+=(u=e[d++])>>>(h=32-f)),n[l]=o<m?i+o*a:s;return n},
/* Copyright 2015-2021 Esri. Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 @preserve */
x=a={defaultNoDataValue:-34027999387901484e22,decode:function(e,t){var r=(t=t||{}).encodedMaskData||null===t.encodedMaskData,e=f(e,t.inputOffset||0,r),r=null!==t.noDataValue?t.noDataValue:a.defaultNoDataValue,i=n(e,t.pixelType||Float32Array,t.encodedMaskData,r,t.returnMask),r={width:e.width,height:e.height,pixelData:i.resultPixels,minValue:i.minValue,maxValue:e.pixels.maxValue,noDataValue:r};return i.resultMask&&(r.maskData=i.resultMask),t.returnEncodedMask&&e.mask&&(r.encodedMaskData=e.mask.bitset||null),t.returnFileInfo&&(r.fileInfo=s(e),t.computeUsedBitDepths&&(r.fileInfo.bitDepths=l(e))),r}},k=function(){"use strict";var y=function(e,t,r,i,a,n,s,l){var f,o,u,c,d,h=(1<<r)-1,m=0,p=0,g=4*e.length-Math.ceil(r*i/8);if(e[e.length-1]<<=8*g,a)for(f=0;f<i;f++)0===p&&(u=e[m++],p=32),r<=p?(o=u>>>p-r&h,p-=r):(o=(u&h)<<(c=r-p)&h,o+=(u=e[m++])>>>(p=32-c)),t[f]=a[o];else for(d=Math.ceil((l-n)/s),f=0;f<i;f++)0===p&&(u=e[m++],p=32),r<=p?(o=u>>>p-r&h,p-=r):(o=(u&h)<<(c=r-p)&h,o+=(u=e[m++])>>>(p=32-c)),t[f]=o<d?n+o*s:l},b=function(e,t,r,i,a,n){for(var s,l,f=(1<<t)-1,o=0,u=0,c=0,d=0,h=[],m=4*e.length-Math.ceil(t*r/8),p=(e[e.length-1]<<=8*m,Math.ceil((n-i)/a)),u=0;u<r;u++)0===c&&(l=e[o++],c=32),t<=c?(d=l>>>c-t&f,c-=t):(d=(l&f)<<(s=t-c)&f,d+=(l=e[o++])>>>(c=32-s)),h[u]=d<p?i+d*a:n;return h.unshift(i),h},U=function(e,t,r,i,a,n,s,l){var f,o,u,c=(1<<r)-1,d=0,h=0,m=0;if(a)for(g=0;g<i;g++)0===h&&(o=e[d++],h=32,m=0),r<=h?(f=o>>>m&c,h-=r,m+=r):(f=o>>>m&c,h=32-(u=r-h),f|=((o=e[d++])&(1<<u)-1)<<r-u,m=u),t[g]=a[f];else for(var p=Math.ceil((l-n)/s),g=0;g<i;g++)0===h&&(o=e[d++],h=32,m=0),r<=h?(f=o>>>m&c,h-=r,m+=r):(f=o>>>m&c,h=32-(u=r-h),f|=((o=e[d++])&(1<<u)-1)<<r-u,m=u),t[g]=f<p?n+f*s:l;return t},I=function(e,t,r,i,a,n){for(var s,l,f=(1<<t)-1,o=0,u=0,c=0,d=0,h=0,m=[],p=Math.ceil((n-i)/a),u=0;u<r;u++)0===c&&(l=e[o++],c=32,h=0),t<=c?(d=l>>>h&f,c-=t,h+=t):(d=l>>>h&f,c=32-(s=t-c),d|=((l=e[o++])&(1<<s)-1)<<t-s,h=s),m[u]=d<p?i+d*a:n;return m.unshift(i),m},v=function(e,t,r,i){var a,n,s,l,f=(1<<r)-1,o=0,u=0,c=4*e.length-Math.ceil(r*i/8);for(e[e.length-1]<<=8*c,a=0;a<i;a++)0===u&&(s=e[o++],u=32),r<=u?(n=s>>>u-r&f,u-=r):(n=(s&f)<<(l=r-u)&f,n+=(s=e[o++])>>>(u=32-l)),t[a]=n;return t},V=function(e,t,r,i){for(var a,n,s,l=(1<<r)-1,f=0,o=0,u=0,c=0;c<i;c++)0===o&&(n=e[f++],o=32,u=0),r<=o?(a=n>>>u&l,o-=r,u+=r):(a=n>>>u&l,o=32-(s=r-o),a|=((n=e[f++])&(1<<s)-1)<<r-s,u=s),t[c]=a;return t},Z={HUFFMAN_LUT_BITS_MAX:12,computeChecksumFletcher32:function(e){for(var t=65535,r=65535,i=e.length,a=Math.floor(i/2),n=0;a;){var s=359<=a?359:a;for(a-=s;r+=t=(t+=e[n++]<<8)+e[n++],--s;);t=(65535&t)+(t>>>16),r=(65535&r)+(r>>>16)}return 1&i&&(r+=t+=e[n]<<8),((r=(65535&r)+(r>>>16))<<16|(t=(65535&t)+(t>>>16)))>>>0},readHeaderInfo:function(e,t){var r=t.ptr,i=new Uint8Array(e,r,6),a={};if(a.fileIdentifierString=String.fromCharCode.apply(null,i),0!==a.fileIdentifierString.lastIndexOf("Lerc2",0))throw"Unexpected file identifier string (expect Lerc2 ): "+a.fileIdentifierString;r+=6;var i=new DataView(e,r,8),n=i.getInt32(0,!0);if(r+=4,3<=(a.fileVersion=n)&&(a.checksum=i.getUint32(4,!0),r+=4),i=new DataView(e,r,12),a.height=i.getUint32(0,!0),a.width=i.getUint32(4,!0),r+=8,4<=n?(a.numDims=i.getUint32(8,!0),r+=4):a.numDims=1,i=new DataView(e,r,40),a.numValidPixel=i.getUint32(0,!0),a.microBlockSize=i.getInt32(4,!0),a.blobSize=i.getInt32(8,!0),a.imageType=i.getInt32(12,!0),a.maxZError=i.getFloat64(16,!0),a.zMin=i.getFloat64(24,!0),a.zMax=i.getFloat64(32,!0),r+=40,t.headerInfo=a,t.ptr=r,3<=n&&this.computeChecksumFletcher32(new Uint8Array(e,r-(4<=n?52:48),a.blobSize-14))!==a.checksum)throw"Checksum failed.";return!0},checkMinMaxRanges:function(e,t){var r=t.headerInfo,i=this.getDataTypeArray(r.imageType),a=r.numDims*this.getDataTypeSize(r.imageType),n=this.readSubArray(e,t.ptr,i,a),s=this.readSubArray(e,t.ptr+a,i,a);t.ptr+=2*a;for(var l=!0,f=0;f<r.numDims;f++)if(n[f]!==s[f]){l=!1;break}return r.minValues=n,r.maxValues=s,l},readSubArray:function(e,t,r,i){var a,e=r===Uint8Array?new Uint8Array(e,t,i):(a=new ArrayBuffer(i),new Uint8Array(a).set(new Uint8Array(e,t,i)),new r(a));return e},readMask:function(e,t){var r=t.ptr,i=t.headerInfo,a=i.width*i.height,i=i.numValidPixel,n=new DataView(e,r,4),s={};if(s.numBytes=n.getUint32(0,!0),r+=4,(0===i||a===i)&&0!==s.numBytes)throw"invalid mask";if(0===i)f=new Uint8Array(Math.ceil(a/8)),s.bitset=f,d=new Uint8Array(a),t.pixels.resultMask=d,r+=s.numBytes;else if(0<s.numBytes){var l,f=new Uint8Array(Math.ceil(a/8)),o=(n=new DataView(e,r,s.numBytes)).getInt16(0,!0),u=2,c=0;do{if(0<o)for(;o--;)f[c++]=n.getUint8(u++);else for(l=n.getUint8(u++),o=-o;o--;)f[c++]=l}while(o=n.getInt16(u,!0),(u+=2)<s.numBytes);if(-32768!==o||c<f.length)throw"Unexpected end of mask RLE encoding";for(var d=new Uint8Array(a),h=0,m=0,m=0;m<a;m++)7&m?(h=f[m>>3],h<<=7&m):h=f[m>>3],128&h&&(d[m]=1);t.pixels.resultMask=d,s.bitset=f,r+=s.numBytes}return t.ptr=r,t.mask=s,!0},readDataOneSweep:function(e,t,r,i){var a=t.ptr,n=t.headerInfo,s=n.numDims,l=n.width*n.height,f=n.imageType,n=n.numValidPixel*Z.getDataTypeSize(f)*s,o=t.pixels.resultMask,u=r===Uint8Array?new Uint8Array(e,a,n):(f=new ArrayBuffer(n),new Uint8Array(f).set(new Uint8Array(e,a,n)),new r(f));if(u.length===l*s)t.pixels.resultPixels=i?Z.swapDimensionOrder(u,l,s,r,!0):u;else{t.pixels.resultPixels=new r(l*s);var c=0,d=0,h=0,m=0;if(1<s){if(i){for(d=0;d<l;d++)if(o[d])for(m=d,h=0;h<s;h++,m+=l)t.pixels.resultPixels[m]=u[c++]}else for(d=0;d<l;d++)if(o[d])for(m=d*s,h=0;h<s;h++)t.pixels.resultPixels[m+h]=u[c++]}else for(d=0;d<l;d++)o[d]&&(t.pixels.resultPixels[d]=u[c++])}return t.ptr=a+=n,!0},readHuffmanTree:function(e,t){var r=this.HUFFMAN_LUT_BITS_MAX,i=new DataView(e,t.ptr,16);if(t.ptr+=16,i.getInt32(0,!0)<2)throw"unsupported Huffman version";var a=i.getInt32(4,!0),n=i.getInt32(8,!0),s=i.getInt32(12,!0);if(s<=n)return!1;for(var l,f,o,u=new Uint32Array(s-n),c=(Z.decodeBits(e,t,u),[]),d=n;d<s;d++)c[l=d-(d<a?0:a)]={first:u[d-n],second:null};var i=e.byteLength-t.ptr,h=Math.ceil(i/4),h=new ArrayBuffer(4*h),m=(new Uint8Array(h).set(new Uint8Array(e,t.ptr,i)),new Uint32Array(h)),p=0,g=0,w=m[0];for(d=n;d<s;d++)0<(o=c[l=d-(d<a?0:a)].first)&&(c[l].second=w<<p>>>32-o,o<=32-p?32===(p+=o)&&(p=0,w=m[++g]):(w=m[++g],c[l].second|=w>>>32-(p+=o-32)));var x=0,k=0,y=new M;for(d=0;d<c.length;d++)void 0!==c[d]&&(x=Math.max(x,c[d].first));var b,U,I,v,V,k=r<=x?r:x,D=[];for(d=n;d<s;d++)if(0<(o=c[l=d-(d<a?0:a)].first))if(b=[o,l],o<=k)for(U=c[l].second<<k-o,I=1<<k-o,f=0;f<I;f++)D[U|f]=b;else for(U=c[l].second,V=y,v=o-1;0<=v;v--)V=U>>>v&1?(V.right||(V.right=new M),V.right):(V.left||(V.left=new M),V.left),0!==v||V.val||(V.val=b[1]);return{decodeLut:D,numBitsLUTQick:k,numBitsLUT:x,tree:y,stuffedData:m,srcPtr:g,bitPos:p}},readHuffman:function(e,t,r,i){var a,n,s,l,f,o,u,c,d,h,m=t.headerInfo.numDims,p=t.headerInfo.height,g=t.headerInfo.width,w=g*p,e=this.readHuffmanTree(e,t),x=e.decodeLut,k=e.tree,y=e.stuffedData,b=e.srcPtr,U=e.bitPos,I=e.numBitsLUTQick,v=e.numBitsLUT,V=0===t.headerInfo.imageType?128:0,D=t.pixels.resultMask,M=0,A=(0<U&&(b++,U=0),y[b]),B=1===t.encodeMode,P=new r(w*m),T=P;if(m<2||B){for(h=0;h<m;h++)if(1<m&&(T=new r(P.buffer,w*h,w),M=0),t.headerInfo.numValidPixel===g*p)for(o=c=0;o<p;o++)for(u=0;u<g;u++,c++){if(n=0,f=l=A<<U>>>32-I,x[f=32-U<I?l|=y[b+1]>>>64-U-I:f])n=x[f][1],U+=x[f][0];else for(f=l=A<<U>>>32-v,32-U<v&&(f=l|=y[b+1]>>>64-U-v),a=k,d=0;d<v;d++)if(!(a=l>>>v-d-1&1?a.right:a.left).left&&!a.right){n=a.val,U=U+d+1;break}32<=U&&(U-=32,A=y[++b]),s=n-V,B?(s=s+(!(0<u)&&0<o?T[c-g]:M)&255,M=T[c]=s):T[c]=s}else for(o=c=0;o<p;o++)for(u=0;u<g;u++,c++)if(D[c]){if(n=0,f=l=A<<U>>>32-I,x[f=32-U<I?l|=y[b+1]>>>64-U-I:f])n=x[f][1],U+=x[f][0];else for(f=l=A<<U>>>32-v,32-U<v&&(f=l|=y[b+1]>>>64-U-v),a=k,d=0;d<v;d++)if(!(a=l>>>v-d-1&1?a.right:a.left).left&&!a.right){n=a.val,U=U+d+1;break}32<=U&&(U-=32,A=y[++b]),s=n-V,B?(!(0<u&&D[c-1])&&0<o&&D[c-g]?s+=T[c-g]:s+=M,s&=255,M=T[c]=s):T[c]=s}}else for(o=c=0;o<p;o++)for(u=0;u<g;u++)if(c=o*g+u,!D||D[c])for(h=0;h<m;h++,c+=w){if(n=0,f=l=A<<U>>>32-I,x[f=32-U<I?l|=y[b+1]>>>64-U-I:f])n=x[f][1],U+=x[f][0];else for(f=l=A<<U>>>32-v,32-U<v&&(f=l|=y[b+1]>>>64-U-v),a=k,d=0;d<v;d++)if(!(a=l>>>v-d-1&1?a.right:a.left).left&&!a.right){n=a.val,U=U+d+1;break}32<=U&&(U-=32,A=y[++b]),T[c]=s=n-V}t.ptr=t.ptr+4*(b+1)+(0<U?4:0),t.pixels.resultPixels=P,1<m&&!i&&(t.pixels.resultPixels=Z.swapDimensionOrder(P,w,m,r))},decodeBits:function(e,t,r,i,a){var n=t.headerInfo,s=n.fileVersion,l=0,f=5<=e.byteLength-t.ptr?5:e.byteLength-t.ptr,f=new DataView(e,t.ptr,f),o=f.getUint8(0),u=(l++,o>>6),u=0==u?4:3-u,c=0<(32&o),o=31&o,d=0;if(1==u)d=f.getUint8(l),l++;else if(2==u)d=f.getUint16(l,!0),l+=2;else{if(4!=u)throw"Invalid valid pixel count type";d=f.getUint32(l,!0),l+=4}var h,m,p,g,w,x,k,u=2*n.maxZError,a=1<n.numDims?n.maxValues[a]:n.zMax;if(c){for(t.counter.lut++,x=f.getUint8(l),l++,g=Math.ceil((x-1)*o/8),w=Math.ceil(g/4),m=new ArrayBuffer(4*w),p=new Uint8Array(m),t.ptr+=l,p.set(new Uint8Array(e,t.ptr,g)),n=new Uint32Array(m),t.ptr+=g,k=0;x-1>>>k;)k++;g=Math.ceil(d*k/8),w=Math.ceil(g/4),m=new ArrayBuffer(4*w),(p=new Uint8Array(m)).set(new Uint8Array(e,t.ptr,g)),h=new Uint32Array(m),t.ptr+=g,c=(3<=s?I:b)(n,o,x-1,i,u,a),(3<=s?U:y)(h,r,k,d,c)}else t.counter.bitstuffer++,k=o,t.ptr+=l,0<k&&(g=Math.ceil(d*k/8),w=Math.ceil(g/4),m=new ArrayBuffer(4*w),(p=new Uint8Array(m)).set(new Uint8Array(e,t.ptr,g)),h=new Uint32Array(m),t.ptr+=g,3<=s?null==i?V(h,r,k,d):U(h,r,k,d,!1,i,u,a):null==i?v(h,r,k,d):y(h,r,k,d,!1,i,u,a))},readTiles:function(e,t,r,O){for(var i,a,L,n,s,E,l,f,o,u,c,d,h,m=t.headerInfo,p=m.width,g=m.height,w=p*g,x=m.microBlockSize,k=m.imageType,y=Z.getDataTypeSize(k),b=Math.ceil(p/x),U=Math.ceil(g/x),I=(t.pixels.numBlocksY=U,t.pixels.numBlocksX=b,t.pixels.ptr=0),v=0,V=0,D=0,M=0,A=0,B=0,P=0,T=new r(x*x),X=g%x||x,Y=p%x||x,S=m.numDims,z=t.pixels.resultMask,F=t.pixels.resultPixels,H=5<=m.fileVersion?14:15,C=m.zMax,V=0;V<U;V++)for(i=V!==U-1?x:X,D=0;D<b;D++)for(M=V*p*x+D*x,n=p-(a=D!==b-1?x:Y),c=0;c<S;c++){if(1<S?(h=F,M=V*p*x+D*x,F=new r(t.pixels.resultPixels.buffer,w*c*y,w),C=m.maxValues[c]):h=null,s=e.byteLength-t.ptr,E={},f=(s=new DataView(e,t.ptr,Math.min(10,s))).getUint8(P=0),P++,d=5<=m.fileVersion?4&f:0,L=f>>6&255,(f>>2&H)!=(D*x>>3&H))throw"integrity issue";if(d&&0===c)throw"integrity issue";if(3<(f=3&f))throw t.ptr+=P,"Invalid block encoding ("+f+")";if(2==f){if(d)if(z)for(I=0;I<i;I++)for(v=0;v<a;v++)z[M]&&(F[M]=h[M]),M++;else for(I=0;I<i;I++)for(v=0;v<a;v++)F[M]=h[M],M++;t.counter.constant++,t.ptr+=P}else if(0==f){if(d)throw"integrity issue";if(t.counter.uncompressed++,t.ptr+=P,A=(A=i*a*y)<(o=e.byteLength-t.ptr)?A:o,o=new ArrayBuffer(A%y==0?A:A+y-A%y),new Uint8Array(o).set(new Uint8Array(e,t.ptr,A)),l=new r(o),B=0,z)for(I=0;I<i;I++){for(v=0;v<a;v++)z[M]&&(F[M]=l[B++]),M++;M+=n}else for(I=0;I<i;I++){for(v=0;v<a;v++)F[M++]=l[B++];M+=n}t.ptr+=B*y}else if(o=Z.getDataTypeUsed(d&&k<6?4:k,L),u=Z.getOnePixel(E,P,o,s),P+=Z.getDataTypeSize(o),3==f)if(t.ptr+=P,t.counter.constantoffset++,z)for(I=0;I<i;I++){for(v=0;v<a;v++)z[M]&&(F[M]=d?Math.min(C,h[M]+u):u),M++;M+=n}else for(I=0;I<i;I++){for(v=0;v<a;v++)F[M]=d?Math.min(C,h[M]+u):u,M++;M+=n}else if(t.ptr+=P,Z.decodeBits(e,t,T,u,c),P=0,d)if(z)for(I=0;I<i;I++){for(v=0;v<a;v++)z[M]&&(F[M]=T[P++]+h[M]),M++;M+=n}else for(I=0;I<i;I++){for(v=0;v<a;v++)F[M]=T[P++]+h[M],M++;M+=n}else if(z)for(I=0;I<i;I++){for(v=0;v<a;v++)z[M]&&(F[M]=T[P++]),M++;M+=n}else for(I=0;I<i;I++){for(v=0;v<a;v++)F[M++]=T[P++];M+=n}}1<S&&!O&&(t.pixels.resultPixels=Z.swapDimensionOrder(t.pixels.resultPixels,w,S,r))},formatFileInfo:function(e){return{fileIdentifierString:e.headerInfo.fileIdentifierString,fileVersion:e.headerInfo.fileVersion,imageType:e.headerInfo.imageType,height:e.headerInfo.height,width:e.headerInfo.width,numValidPixel:e.headerInfo.numValidPixel,microBlockSize:e.headerInfo.microBlockSize,blobSize:e.headerInfo.blobSize,maxZError:e.headerInfo.maxZError,pixelType:Z.getPixelType(e.headerInfo.imageType),eofOffset:e.eofOffset,mask:e.mask?{numBytes:e.mask.numBytes}:null,pixels:{numBlocksX:e.pixels.numBlocksX,numBlocksY:e.pixels.numBlocksY,maxValue:e.headerInfo.zMax,minValue:e.headerInfo.zMin,noDataValue:e.noDataValue}}},constructConstantSurface:function(e,t){var r=e.headerInfo.zMax,i=e.headerInfo.zMin,a=e.headerInfo.maxValues,n=e.headerInfo.numDims,s=e.headerInfo.height*e.headerInfo.width,l=0,f=0,o=0,u=e.pixels.resultMask,c=e.pixels.resultPixels;if(u)if(1<n){if(t)for(l=0;l<n;l++)for(o=l*s,r=a[l],f=0;f<s;f++)u[f]&&(c[o+f]=r);else for(f=0;f<s;f++)if(u[f])for(o=f*n,l=0;l<n;l++)c[o+n]=a[l]}else for(f=0;f<s;f++)u[f]&&(c[f]=r);else if(1<n&&i!==r)if(t)for(l=0;l<n;l++)for(o=l*s,r=a[l],f=0;f<s;f++)c[o+f]=r;else for(f=0;f<s;f++)for(o=f*n,l=0;l<n;l++)c[o+l]=a[l];else for(f=0;f<s*n;f++)c[f]=r},getDataTypeArray:function(e){var t;switch(e){case 0:t=Int8Array;break;case 1:t=Uint8Array;break;case 2:t=Int16Array;break;case 3:t=Uint16Array;break;case 4:t=Int32Array;break;case 5:t=Uint32Array;break;case 6:t=Float32Array;break;case 7:t=Float64Array;break;default:t=Float32Array}return t},getPixelType:function(e){var t;switch(e){case 0:t="S8";break;case 1:t="U8";break;case 2:t="S16";break;case 3:t="U16";break;case 4:t="S32";break;case 5:t="U32";break;case 6:t="F32";break;case 7:t="F64";break;default:t="F32"}return t},isValidPixelValue:function(e,t){if(null==t)return!1;var r;switch(e){case 0:r=-128<=t&&t<=127;break;case 1:r=0<=t&&t<=255;break;case 2:r=-32768<=t&&t<=32767;break;case 3:r=0<=t&&t<=65536;break;case 4:r=-2147483648<=t&&t<=2147483647;break;case 5:r=0<=t&&t<=4294967296;break;case 6:r=-34027999387901484e22<=t&&t<=34027999387901484e22;break;case 7:r=-17976931348623157e292<=t&&t<=17976931348623157e292;break;default:r=!1}return r},getDataTypeSize:function(e){var t=0;switch(e){case 0:case 1:t=1;break;case 2:case 3:t=2;break;case 4:case 5:case 6:t=4;break;case 7:t=8;break;default:t=e}return t},getDataTypeUsed:function(e,t){var r=e;switch(e){case 2:case 4:r=e-t;break;case 3:case 5:r=e-2*t;break;case 6:r=0===t?e:1===t?2:1;break;case 7:r=0===t?e:e-2*t+1;break;default:r=e}return r},getOnePixel:function(e,t,r,i){var a=0;switch(r){case 0:a=i.getInt8(t);break;case 1:a=i.getUint8(t);break;case 2:a=i.getInt16(t,!0);break;case 3:a=i.getUint16(t,!0);break;case 4:a=i.getInt32(t,!0);break;case 5:a=i.getUInt32(t,!0);break;case 6:a=i.getFloat32(t,!0);break;case 7:a=i.getFloat64(t,!0);break;default:throw"the decoder does not understand this pixel type"}return a},swapDimensionOrder:function(e,t,r,i,a){var n=0,s=0,l=0,f=0,o=e;if(1<r)if(o=new i(t*r),a)for(n=0;n<t;n++)for(f=n,l=0;l<r;l++,f+=t)o[f]=e[s++];else for(n=0;n<t;n++)for(f=n,l=0;l<r;l++,f+=t)o[s++]=e[f];return o}},M=function(e,t,r){this.val=e,this.left=t,this.right=r};return{decode:function(e,t){var r=(t=t||{}).noDataValue,i=0,a={};if(a.ptr=t.inputOffset||0,a.pixels={},Z.readHeaderInfo(e,a)){var n=a.headerInfo,s=n.fileVersion,l=Z.getDataTypeArray(n.imageType);if(5<s)throw"unsupported lerc version 2."+s;Z.readMask(e,a),n.numValidPixel===n.width*n.height||a.pixels.resultMask||(a.pixels.resultMask=t.maskData);var f,o=n.width*n.height,u=(a.pixels.resultPixels=new l(o*n.numDims),a.counter={onesweep:0,uncompressed:0,lut:0,bitstuffer:0,constant:0,constantoffset:0},!t.returnPixelInterleavedDims);if(0!==n.numValidPixel)if(n.zMax===n.zMin)Z.constructConstantSurface(a,u);else if(4<=s&&Z.checkMinMaxRanges(e,a))Z.constructConstantSurface(a,u);else{var c=new DataView(e,a.ptr,2),d=c.getUint8(0);if(a.ptr++,d)Z.readDataOneSweep(e,a,l,u);else if(1<s&&n.imageType<=1&&Math.abs(n.maxZError-.5)<1e-5){d=c.getUint8(1);if(a.ptr++,2<(a.encodeMode=d)||s<4&&1<d)throw"Invalid Huffman flag "+d;d?Z.readHuffman(e,a,l,u):Z.readTiles(e,a,l,u)}else Z.readTiles(e,a,l,u)}a.eofOffset=a.ptr,t.inputOffset?(f=a.headerInfo.blobSize+t.inputOffset-a.ptr,1<=Math.abs(f)&&(a.eofOffset=t.inputOffset+a.headerInfo.blobSize)):(f=a.headerInfo.blobSize-a.ptr,1<=Math.abs(f)&&(a.eofOffset=a.headerInfo.blobSize));var h={width:n.width,height:n.height,pixelData:a.pixels.resultPixels,minValue:n.zMin,maxValue:n.zMax,validPixelCount:n.numValidPixel,dimCount:n.numDims,dimStats:{minValues:n.minValues,maxValues:n.maxValues},maskData:a.pixels.resultMask};if(a.pixels.resultMask&&Z.isValidPixelValue(n.imageType,r)){for(var m=a.pixels.resultMask,i=0;i<o;i++)m[i]||(h.pixelData[i]=r);h.noDataValue=r}return a.noDataValue=r,t.returnFileInfo&&(h.fileInfo=Z.formatFileInfo(a)),h}},getBandCount:function(e){for(var t=0,r=0,i={ptr:0,pixels:{}};r<e.byteLength-58;)Z.readHeaderInfo(e,i),r+=i.headerInfo.blobSize,t++,i.ptr=r;return t}}}(),i=new ArrayBuffer(4),o=new Uint8Array(i),y=(new Uint32Array(i)[0]=1)===o[0],u={decode:function(e,t){if(!y)throw"Big endian system is not supported.";var r,i,a=(t=t||{}).inputOffset||0,n=new Uint8Array(e,a,10),n=String.fromCharCode.apply(null,n);if("CntZImage"===n.trim())r=x,i=1;else{if("Lerc2"!==n.substring(0,5))throw"Unexpected file identifier string: "+n;r=k,i=2}for(var s,l,f,o,u,c=0,d=e.byteLength-10,h=[],m={width:0,height:0,pixels:[],pixelType:t.pixelType,mask:null,statistics:[]},p=0;a<d;){var g=r.decode(e,{inputOffset:a,encodedMaskData:s,maskData:w,returnMask:0===c,returnEncodedMask:0===c,returnFileInfo:!0,returnPixelInterleavedDims:t.returnPixelInterleavedDims,pixelType:t.pixelType||null,noDataValue:t.noDataValue||null}),a=g.fileInfo.eofOffset,w=g.maskData;0===c&&(s=g.encodedMaskData,m.width=g.width,m.height=g.height,m.dimCount=g.dimCount||1,m.pixelType=g.pixelType||g.fileInfo.pixelType,m.mask=w),1<i&&(w&&h.push(w),g.fileInfo.mask&&0<g.fileInfo.mask.numBytes&&p++),c++,m.pixels.push(g.pixelData),m.statistics.push({minValue:g.minValue,maxValue:g.maxValue,noDataValue:g.noDataValue,dimStats:g.dimStats})}if(1<i&&1<p){for(u=m.width*m.height,m.bandMasks=h,(w=new Uint8Array(u)).set(h[0]),f=1;f<h.length;f++)for(l=h[f],o=0;o<u;o++)w[o]=w[o]&l[o];m.maskData=w}return m}},void 0!==(i=function(){return u}.apply(t,[]))&&(e.exports=i)},1290:function(e,t,r){"use strict";r.r(t),r.d(t,"default",function(){return LercDecoder});var i=r(1283),t=r(1284),a=r.n(t),t=r(688),n=r(19);class LercDecoder extends t.a{constructor(e){super(),this.planarConfiguration=void 0!==e.PlanarConfiguration?e.PlanarConfiguration:1,this.samplesPerPixel=void 0!==e.SamplesPerPixel?e.SamplesPerPixel:1,this.addCompression=e.LercParameters[n.c.AddCompression]}decodeBlock(e){switch(this.addCompression){case n.b.None:break;case n.b.Deflate:e=Object(i.a)(new Uint8Array(e)).buffer;break;default:throw new Error("Unsupported LERC additional compression method identifier: "+this.addCompression)}return a.a.decode(e,{returnPixelInterleavedDims:1===this.planarConfiguration}).pixels[0].buffer}}}}]);
//# sourceMappingURL=7.index.js.map