(window.webpackJsonp=window.webpackJsonp||[]).push([[76],{145:function(e,n,t){"use strict";t.r(n),t.d(n,"frontMatter",(function(){return o})),t.d(n,"metadata",(function(){return a})),t.d(n,"toc",(function(){return l})),t.d(n,"default",(function(){return s}));var i=t(3),r=t(7),c=(t(0),t(215)),o={id:"feel-built-in-functions-introduction",title:"Introduction"},a={unversionedId:"reference/builtin-functions/feel-built-in-functions-introduction",id:"version-1.14/reference/builtin-functions/feel-built-in-functions-introduction",isDocsHomePage:!1,title:"Introduction",description:"FEEL includes a lot of built-in functions. These functions can be invoked",source:"@site/versioned_docs/version-1.14/reference/builtin-functions/feel-built-in-functions-introduction.md",slug:"/reference/builtin-functions/feel-built-in-functions-introduction",permalink:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-introduction",editUrl:"https://github.com/camunda/feel-scala/edit/master/docs/versioned_docs/version-1.14/reference/builtin-functions/feel-built-in-functions-introduction.md",version:"1.14",sidebar:"version-1.14/Reference",previous:{title:"Functions",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-functions"},next:{title:"Conversion Functions",permalink:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-conversion"}},l=[],u={toc:l};function s(e){var n=e.components,t=Object(r.a)(e,["components"]);return Object(c.b)("wrapper",Object(i.a)({},u,t,{components:n,mdxType:"MDXLayout"}),Object(c.b)("p",null,"FEEL includes a lot of built-in functions. These functions can be invoked\nin ",Object(c.b)("a",Object(i.a)({parentName:"p"},{href:"/feel-scala/docs/1.14/reference/language-guide/feel-expressions-introduction"}),"expressions"),"\nand ",Object(c.b)("a",Object(i.a)({parentName:"p"},{href:"/feel-scala/docs/1.14/reference/language-guide/feel-unary-tests"}),"unary-tests"),"."),Object(c.b)("pre",null,Object(c.b)("code",Object(i.a)({parentName:"pre"},{className:"language-js"}),'contains("me@camunda.com", ".com")\n// invoke function with positional arguments\n\ncontains(string: "me@camunda.com", match: ".de")\n// invoke function with named arguments\n')),Object(c.b)("p",null,"Read more about functions ",Object(c.b)("a",Object(i.a)({parentName:"p"},{href:"/feel-scala/docs/1.14/reference/language-guide/feel-functions#invocation"}),"here"),"."),Object(c.b)("p",null,"For a better overview, this section is split into functions based on their primary operational data type:"),Object(c.b)("ul",null,Object(c.b)("li",{parentName:"ul"},Object(c.b)("a",Object(i.a)({parentName:"li"},{href:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-boolean"}),"Boolean")),Object(c.b)("li",{parentName:"ul"},Object(c.b)("a",Object(i.a)({parentName:"li"},{href:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-string"}),"String")),Object(c.b)("li",{parentName:"ul"},Object(c.b)("a",Object(i.a)({parentName:"li"},{href:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-numeric"}),"Numeric")),Object(c.b)("li",{parentName:"ul"},Object(c.b)("a",Object(i.a)({parentName:"li"},{href:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-list"}),"List")),Object(c.b)("li",{parentName:"ul"},Object(c.b)("a",Object(i.a)({parentName:"li"},{href:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-context"}),"Context")),Object(c.b)("li",{parentName:"ul"},Object(c.b)("a",Object(i.a)({parentName:"li"},{href:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-temporal"}),"Temporal")),Object(c.b)("li",{parentName:"ul"},Object(c.b)("a",Object(i.a)({parentName:"li"},{href:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-range"}),"Range"))),Object(c.b)("p",null,"Additionally, there are ",Object(c.b)("a",Object(i.a)({parentName:"p"},{href:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-conversion"}),"conversion")," functions that allows\nto construct new values of a data type (aka factory functions)."))}s.isMDXComponent=!0},215:function(e,n,t){"use strict";t.d(n,"a",(function(){return f})),t.d(n,"b",(function(){return d}));var i=t(0),r=t.n(i);function c(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function o(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);n&&(i=i.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,i)}return t}function a(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?o(Object(t),!0).forEach((function(n){c(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):o(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function l(e,n){if(null==e)return{};var t,i,r=function(e,n){if(null==e)return{};var t,i,r={},c=Object.keys(e);for(i=0;i<c.length;i++)t=c[i],n.indexOf(t)>=0||(r[t]=e[t]);return r}(e,n);if(Object.getOwnPropertySymbols){var c=Object.getOwnPropertySymbols(e);for(i=0;i<c.length;i++)t=c[i],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(r[t]=e[t])}return r}var u=r.a.createContext({}),s=function(e){var n=r.a.useContext(u),t=n;return e&&(t="function"==typeof e?e(n):a(a({},n),e)),t},f=function(e){var n=s(e.components);return r.a.createElement(u.Provider,{value:n},e.children)},b={inlineCode:"code",wrapper:function(e){var n=e.children;return r.a.createElement(r.a.Fragment,{},n)}},p=r.a.forwardRef((function(e,n){var t=e.components,i=e.mdxType,c=e.originalType,o=e.parentName,u=l(e,["components","mdxType","originalType","parentName"]),f=s(t),p=i,d=f["".concat(o,".").concat(p)]||f[p]||b[p]||c;return t?r.a.createElement(d,a(a({ref:n},u),{},{components:t})):r.a.createElement(d,a({ref:n},u))}));function d(e,n){var t=arguments,i=n&&n.mdxType;if("string"==typeof e||i){var c=t.length,o=new Array(c);o[0]=p;var a={};for(var l in n)hasOwnProperty.call(n,l)&&(a[l]=n[l]);a.originalType=e,a.mdxType="string"==typeof e?e:i,o[1]=a;for(var u=2;u<c;u++)o[u]=t[u];return r.a.createElement.apply(null,o)}return r.a.createElement.apply(null,t)}p.displayName="MDXCreateElement"}}]);