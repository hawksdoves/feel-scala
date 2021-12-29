(window.webpackJsonp=window.webpackJsonp||[]).push([[131],{203:function(e,n,t){"use strict";t.r(n),t.d(n,"frontMatter",(function(){return o})),t.d(n,"metadata",(function(){return l})),t.d(n,"toc",(function(){return c})),t.d(n,"default",(function(){return s}));var r=t(3),i=t(7),a=(t(0),t(215)),o={id:"feel-built-in-functions-boolean",title:"Boolean Functions"},l={unversionedId:"reference/builtin-functions/feel-built-in-functions-boolean",id:"version-1.12/reference/builtin-functions/feel-built-in-functions-boolean",isDocsHomePage:!1,title:"Boolean Functions",description:"not()",source:"@site/versioned_docs/version-1.12/reference/builtin-functions/feel-built-in-functions-boolean.md",slug:"/reference/builtin-functions/feel-built-in-functions-boolean",permalink:"/feel-scala/docs/1.12/reference/builtin-functions/feel-built-in-functions-boolean",editUrl:"https://github.com/camunda/feel-scala/edit/master/docs/versioned_docs/version-1.12/reference/builtin-functions/feel-built-in-functions-boolean.md",version:"1.12",sidebar:"version-1.12/Reference",previous:{title:"Conversion Functions",permalink:"/feel-scala/docs/1.12/reference/builtin-functions/feel-built-in-functions-conversion"},next:{title:"String Functions",permalink:"/feel-scala/docs/1.12/reference/builtin-functions/feel-built-in-functions-string"}},c=[{value:"not()",id:"not",children:[]},{value:"is defined()",id:"is-defined",children:[]}],u={toc:c};function s(e){var n=e.components,t=Object(i.a)(e,["components"]);return Object(a.b)("wrapper",Object(r.a)({},u,t,{components:n,mdxType:"MDXLayout"}),Object(a.b)("h2",{id:"not"},"not()"),Object(a.b)("ul",null,Object(a.b)("li",{parentName:"ul"},"parameters:",Object(a.b)("ul",{parentName:"li"},Object(a.b)("li",{parentName:"ul"},Object(a.b)("inlineCode",{parentName:"li"},"negand"),": boolean"))),Object(a.b)("li",{parentName:"ul"},"result: boolean")),Object(a.b)("pre",null,Object(a.b)("code",Object(r.a)({parentName:"pre"},{className:"language-js"}),"not(true)\n// false\n")),Object(a.b)("h2",{id:"is-defined"},"is defined()"),Object(a.b)("p",null,"Checks if a given value is defined or not. A value is defined if it exists, and it is an instance of one of the FEEL data types including ",Object(a.b)("inlineCode",{parentName:"p"},"null"),"."),Object(a.b)("p",null,"The function can be used to check if a variable, or a context entry (e.g. a property of a variable) exists. It allows differentiating between a variable that is ",Object(a.b)("inlineCode",{parentName:"p"},"null")," and a value that doesn't exist.   "),Object(a.b)("ul",null,Object(a.b)("li",{parentName:"ul"},"parameters:",Object(a.b)("ul",{parentName:"li"},Object(a.b)("li",{parentName:"ul"},Object(a.b)("inlineCode",{parentName:"li"},"value"),": any"))),Object(a.b)("li",{parentName:"ul"},"result: boolean")),Object(a.b)("pre",null,Object(a.b)("code",Object(r.a)({parentName:"pre"},{className:"language-js"}),'is defined(1)\n// true\n\nis defined(null)\n// true\n\nis defined(x)\n// false - if no variable "x" exists\n\nis defined(x.y)\n// false - if no variable "x" exists or it doesn\'t have a property "y"\n')))}s.isMDXComponent=!0},215:function(e,n,t){"use strict";t.d(n,"a",(function(){return b})),t.d(n,"b",(function(){return d}));var r=t(0),i=t.n(r);function a(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function o(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);n&&(r=r.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,r)}return t}function l(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?o(Object(t),!0).forEach((function(n){a(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):o(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function c(e,n){if(null==e)return{};var t,r,i=function(e,n){if(null==e)return{};var t,r,i={},a=Object.keys(e);for(r=0;r<a.length;r++)t=a[r],n.indexOf(t)>=0||(i[t]=e[t]);return i}(e,n);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(r=0;r<a.length;r++)t=a[r],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(i[t]=e[t])}return i}var u=i.a.createContext({}),s=function(e){var n=i.a.useContext(u),t=n;return e&&(t="function"==typeof e?e(n):l(l({},n),e)),t},b=function(e){var n=s(e.components);return i.a.createElement(u.Provider,{value:n},e.children)},f={inlineCode:"code",wrapper:function(e){var n=e.children;return i.a.createElement(i.a.Fragment,{},n)}},p=i.a.forwardRef((function(e,n){var t=e.components,r=e.mdxType,a=e.originalType,o=e.parentName,u=c(e,["components","mdxType","originalType","parentName"]),b=s(t),p=r,d=b["".concat(o,".").concat(p)]||b[p]||f[p]||a;return t?i.a.createElement(d,l(l({ref:n},u),{},{components:t})):i.a.createElement(d,l({ref:n},u))}));function d(e,n){var t=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var a=t.length,o=new Array(a);o[0]=p;var l={};for(var c in n)hasOwnProperty.call(n,c)&&(l[c]=n[c]);l.originalType=e,l.mdxType="string"==typeof e?e:r,o[1]=l;for(var u=2;u<a;u++)o[u]=t[u];return i.a.createElement.apply(null,o)}return i.a.createElement.apply(null,t)}p.displayName="MDXCreateElement"}}]);