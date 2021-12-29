(window.webpackJsonp=window.webpackJsonp||[]).push([[102],{171:function(e,n,r){"use strict";r.r(n),r.d(n,"frontMatter",(function(){return c})),r.d(n,"metadata",(function(){return o})),r.d(n,"toc",(function(){return l})),r.d(n,"default",(function(){return u}));var t=r(3),i=r(7),a=(r(0),r(215)),c={id:"feel-numeric-expressions",title:"Numeric Expressions"},o={unversionedId:"reference/language-guide/feel-numeric-expressions",id:"version-1.14/reference/language-guide/feel-numeric-expressions",isDocsHomePage:!1,title:"Numeric Expressions",description:"Literal",source:"@site/versioned_docs/version-1.14/reference/language-guide/feel-numeric-expressions.md",slug:"/reference/language-guide/feel-numeric-expressions",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-numeric-expressions",editUrl:"https://github.com/camunda/feel-scala/edit/master/docs/versioned_docs/version-1.14/reference/language-guide/feel-numeric-expressions.md",version:"1.14",sidebar:"version-1.14/Reference",previous:{title:"String Expressions",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-string-expressions"},next:{title:"List Expressions",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-list-expressions"}},l=[{value:"Literal",id:"literal",children:[]},{value:"Addition",id:"addition",children:[]},{value:"Subtraction",id:"subtraction",children:[]},{value:"Multiplication",id:"multiplication",children:[]},{value:"Division",id:"division",children:[]},{value:"Exponentiation",id:"exponentiation",children:[]}],s={toc:l};function u(e){var n=e.components,r=Object(i.a)(e,["components"]);return Object(a.b)("wrapper",Object(t.a)({},s,r,{components:n,mdxType:"MDXLayout"}),Object(a.b)("h3",{id:"literal"},"Literal"),Object(a.b)("p",null,"Creates a new numeric value. Leading zeros are valid."),Object(a.b)("pre",null,Object(a.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"1\n\n0.5\n.5\n\n-2 \n\n01\n\n-0002\n")),Object(a.b)("h3",{id:"addition"},"Addition"),Object(a.b)("pre",null,Object(a.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"2 + 3\n// 5\n")),Object(a.b)("h3",{id:"subtraction"},"Subtraction"),Object(a.b)("pre",null,Object(a.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"5 - 3\n// 2\n")),Object(a.b)("h3",{id:"multiplication"},"Multiplication"),Object(a.b)("pre",null,Object(a.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"5 * 3        \n// 15\n")),Object(a.b)("h3",{id:"division"},"Division"),Object(a.b)("pre",null,Object(a.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"6 / 2  \n// 3\n")),Object(a.b)("h3",{id:"exponentiation"},"Exponentiation"),Object(a.b)("pre",null,Object(a.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"2 ** 3   \n// 8\n")))}u.isMDXComponent=!0},215:function(e,n,r){"use strict";r.d(n,"a",(function(){return p})),r.d(n,"b",(function(){return f}));var t=r(0),i=r.n(t);function a(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}function c(e,n){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);n&&(t=t.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),r.push.apply(r,t)}return r}function o(e){for(var n=1;n<arguments.length;n++){var r=null!=arguments[n]?arguments[n]:{};n%2?c(Object(r),!0).forEach((function(n){a(e,n,r[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):c(Object(r)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(r,n))}))}return e}function l(e,n){if(null==e)return{};var r,t,i=function(e,n){if(null==e)return{};var r,t,i={},a=Object.keys(e);for(t=0;t<a.length;t++)r=a[t],n.indexOf(r)>=0||(i[r]=e[r]);return i}(e,n);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(t=0;t<a.length;t++)r=a[t],n.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(i[r]=e[r])}return i}var s=i.a.createContext({}),u=function(e){var n=i.a.useContext(s),r=n;return e&&(r="function"==typeof e?e(n):o(o({},n),e)),r},p=function(e){var n=u(e.components);return i.a.createElement(s.Provider,{value:n},e.children)},d={inlineCode:"code",wrapper:function(e){var n=e.children;return i.a.createElement(i.a.Fragment,{},n)}},b=i.a.forwardRef((function(e,n){var r=e.components,t=e.mdxType,a=e.originalType,c=e.parentName,s=l(e,["components","mdxType","originalType","parentName"]),p=u(r),b=t,f=p["".concat(c,".").concat(b)]||p[b]||d[b]||a;return r?i.a.createElement(f,o(o({ref:n},s),{},{components:r})):i.a.createElement(f,o({ref:n},s))}));function f(e,n){var r=arguments,t=n&&n.mdxType;if("string"==typeof e||t){var a=r.length,c=new Array(a);c[0]=b;var o={};for(var l in n)hasOwnProperty.call(n,l)&&(o[l]=n[l]);o.originalType=e,o.mdxType="string"==typeof e?e:t,c[1]=o;for(var s=2;s<a;s++)c[s]=r[s];return i.a.createElement.apply(null,c)}return i.a.createElement.apply(null,r)}b.displayName="MDXCreateElement"}}]);