(window.webpackJsonp=window.webpackJsonp||[]).push([[103],{172:function(e,r,n){"use strict";n.r(r),n.d(r,"frontMatter",(function(){return c})),n.d(r,"metadata",(function(){return a})),n.d(r,"toc",(function(){return l})),n.d(r,"default",(function(){return p}));var t=n(3),o=n(7),i=(n(0),n(215)),c={id:"developer-guide-introduction",title:"Introduction"},a={unversionedId:"reference/developer-guide/developer-guide-introduction",id:"reference/developer-guide/developer-guide-introduction",isDocsHomePage:!1,title:"Introduction",description:"You can embed the FEEL engine in your application in different ways. Have a look",source:"@site/docs/reference/developer-guide/developer-guide-introduction.md",slug:"/reference/developer-guide/developer-guide-introduction",permalink:"/feel-scala/docs/reference/developer-guide/developer-guide-introduction",editUrl:"https://github.com/camunda/feel-scala/edit/master/docs/docs/reference/developer-guide/developer-guide-introduction.md",version:"current",sidebar:"Reference",previous:{title:"Range Functions",permalink:"/feel-scala/docs/reference/builtin-functions/feel-built-in-functions-range"},next:{title:"Bootstrapping",permalink:"/feel-scala/docs/reference/developer-guide/bootstrapping"}},l=[],u={toc:l};function p(e){var r=e.components,n=Object(o.a)(e,["components"]);return Object(i.b)("wrapper",Object(t.a)({},u,n,{components:r,mdxType:"MDXLayout"}),Object(i.b)("p",null,"You can embed the FEEL engine in your application in different ways. Have a look\nat ",Object(i.b)("a",Object(t.a)({parentName:"p"},{href:"/feel-scala/docs/reference/developer-guide/bootstrapping"}),"Bootstrapping")," to see how."),Object(i.b)("p",null,"Afterward, you can extend and customize the FEEL engine by implementing one of the following\nSPIs (Service Provider Interface):"),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},Object(i.b)("a",Object(t.a)({parentName:"li"},{href:"/feel-scala/docs/reference/developer-guide/function-provider-spi"}),"Function Provider SPI")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("a",Object(t.a)({parentName:"li"},{href:"/feel-scala/docs/reference/developer-guide/value-mapper-spi"}),"Value Mapper SPI")),Object(i.b)("li",{parentName:"ul"},Object(i.b)("a",Object(t.a)({parentName:"li"},{href:"/feel-scala/docs/reference/developer-guide/clock-spi"}),"Clock SPI"))))}p.isMDXComponent=!0},215:function(e,r,n){"use strict";n.d(r,"a",(function(){return d})),n.d(r,"b",(function(){return b}));var t=n(0),o=n.n(t);function i(e,r,n){return r in e?Object.defineProperty(e,r,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[r]=n,e}function c(e,r){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);r&&(t=t.filter((function(r){return Object.getOwnPropertyDescriptor(e,r).enumerable}))),n.push.apply(n,t)}return n}function a(e){for(var r=1;r<arguments.length;r++){var n=null!=arguments[r]?arguments[r]:{};r%2?c(Object(n),!0).forEach((function(r){i(e,r,n[r])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):c(Object(n)).forEach((function(r){Object.defineProperty(e,r,Object.getOwnPropertyDescriptor(n,r))}))}return e}function l(e,r){if(null==e)return{};var n,t,o=function(e,r){if(null==e)return{};var n,t,o={},i=Object.keys(e);for(t=0;t<i.length;t++)n=i[t],r.indexOf(n)>=0||(o[n]=e[n]);return o}(e,r);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(t=0;t<i.length;t++)n=i[t],r.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var u=o.a.createContext({}),p=function(e){var r=o.a.useContext(u),n=r;return e&&(n="function"==typeof e?e(r):a(a({},r),e)),n},d=function(e){var r=p(e.components);return o.a.createElement(u.Provider,{value:r},e.children)},f={inlineCode:"code",wrapper:function(e){var r=e.children;return o.a.createElement(o.a.Fragment,{},r)}},s=o.a.forwardRef((function(e,r){var n=e.components,t=e.mdxType,i=e.originalType,c=e.parentName,u=l(e,["components","mdxType","originalType","parentName"]),d=p(n),s=t,b=d["".concat(c,".").concat(s)]||d[s]||f[s]||i;return n?o.a.createElement(b,a(a({ref:r},u),{},{components:n})):o.a.createElement(b,a({ref:r},u))}));function b(e,r){var n=arguments,t=r&&r.mdxType;if("string"==typeof e||t){var i=n.length,c=new Array(i);c[0]=s;var a={};for(var l in r)hasOwnProperty.call(r,l)&&(a[l]=r[l]);a.originalType=e,a.mdxType="string"==typeof e?e:t,c[1]=a;for(var u=2;u<i;u++)c[u]=n[u];return o.a.createElement.apply(null,c)}return o.a.createElement.apply(null,n)}s.displayName="MDXCreateElement"}}]);