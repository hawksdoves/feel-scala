(window.webpackJsonp=window.webpackJsonp||[]).push([[27],{215:function(e,a,n){"use strict";n.d(a,"a",(function(){return p})),n.d(a,"b",(function(){return m}));var t=n(0),r=n.n(t);function c(e,a,n){return a in e?Object.defineProperty(e,a,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[a]=n,e}function i(e,a){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);a&&(t=t.filter((function(a){return Object.getOwnPropertyDescriptor(e,a).enumerable}))),n.push.apply(n,t)}return n}function o(e){for(var a=1;a<arguments.length;a++){var n=null!=arguments[a]?arguments[a]:{};a%2?i(Object(n),!0).forEach((function(a){c(e,a,n[a])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(a){Object.defineProperty(e,a,Object.getOwnPropertyDescriptor(n,a))}))}return e}function l(e,a){if(null==e)return{};var n,t,r=function(e,a){if(null==e)return{};var n,t,r={},c=Object.keys(e);for(t=0;t<c.length;t++)n=c[t],a.indexOf(n)>=0||(r[n]=e[n]);return r}(e,a);if(Object.getOwnPropertySymbols){var c=Object.getOwnPropertySymbols(e);for(t=0;t<c.length;t++)n=c[t],a.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=r.a.createContext({}),b=function(e){var a=r.a.useContext(s),n=a;return e&&(n="function"==typeof e?e(a):o(o({},a),e)),n},p=function(e){var a=b(e.components);return r.a.createElement(s.Provider,{value:a},e.children)},u={inlineCode:"code",wrapper:function(e){var a=e.children;return r.a.createElement(r.a.Fragment,{},a)}},d=r.a.forwardRef((function(e,a){var n=e.components,t=e.mdxType,c=e.originalType,i=e.parentName,s=l(e,["components","mdxType","originalType","parentName"]),p=b(n),d=t,m=p["".concat(i,".").concat(d)]||p[d]||u[d]||c;return n?r.a.createElement(m,o(o({ref:a},s),{},{components:n})):r.a.createElement(m,o({ref:a},s))}));function m(e,a){var n=arguments,t=a&&a.mdxType;if("string"==typeof e||t){var c=n.length,i=new Array(c);i[0]=d;var o={};for(var l in a)hasOwnProperty.call(a,l)&&(o[l]=a[l]);o.originalType=e,o.mdxType="string"==typeof e?e:t,i[1]=o;for(var s=2;s<c;s++)i[s]=n[s];return r.a.createElement.apply(null,i)}return r.a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},96:function(e,a,n){"use strict";n.r(a),n.d(a,"frontMatter",(function(){return i})),n.d(a,"metadata",(function(){return o})),n.d(a,"toc",(function(){return l})),n.d(a,"default",(function(){return b}));var t=n(3),r=n(7),c=(n(0),n(215)),i={id:"feel-variables",title:"Variables"},o={unversionedId:"reference/language-guide/feel-variables",id:"version-1.14/reference/language-guide/feel-variables",isDocsHomePage:!1,title:"Variables",description:"Access Variables",source:"@site/versioned_docs/version-1.14/reference/language-guide/feel-variables.md",slug:"/reference/language-guide/feel-variables",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-variables",editUrl:"https://github.com/camunda/feel-scala/edit/master/docs/versioned_docs/version-1.14/reference/language-guide/feel-variables.md",version:"1.14",sidebar:"version-1.14/Reference",previous:{title:"Temporal Expressions",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-temporal-expressions"},next:{title:"Control Flow",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-control-flow"}},l=[{value:"Access Variables",id:"access-variables",children:[]},{value:"Escape Variable Names",id:"escape-variable-names",children:[]}],s={toc:l};function b(e){var a=e.components,n=Object(r.a)(e,["components"]);return Object(c.b)("wrapper",Object(t.a)({},s,n,{components:a,mdxType:"MDXLayout"}),Object(c.b)("h3",{id:"access-variables"},"Access Variables"),Object(c.b)("p",null,"Access the value of a variable by its variable name."),Object(c.b)("pre",null,Object(c.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"a + b\n")),Object(c.b)("p",null,"If the value of the variable is a context then a ",Object(c.b)("a",Object(t.a)({parentName:"p"},{href:"feel-context-expressions#get-entry--path"}),"context entry can be accessed")," by its key. "),Object(c.b)("pre",null,Object(c.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"a.b\n")),Object(c.b)("div",{className:"admonition admonition-tip alert alert--success"},Object(c.b)("div",Object(t.a)({parentName:"div"},{className:"admonition-heading"}),Object(c.b)("h5",{parentName:"div"},Object(c.b)("span",Object(t.a)({parentName:"h5"},{className:"admonition-icon"}),Object(c.b)("svg",Object(t.a)({parentName:"span"},{xmlns:"http://www.w3.org/2000/svg",width:"12",height:"16",viewBox:"0 0 12 16"}),Object(c.b)("path",Object(t.a)({parentName:"svg"},{fillRule:"evenodd",d:"M6.5 0C3.48 0 1 2.19 1 5c0 .92.55 2.25 1 3 1.34 2.25 1.78 2.78 2 4v1h5v-1c.22-1.22.66-1.75 2-4 .45-.75 1-2.08 1-3 0-2.81-2.48-5-5.5-5zm3.64 7.48c-.25.44-.47.8-.67 1.11-.86 1.41-1.25 2.06-1.45 3.23-.02.05-.02.11-.02.17H5c0-.06 0-.13-.02-.17-.2-1.17-.59-1.83-1.45-3.23-.2-.31-.42-.67-.67-1.11C2.44 6.78 2 5.65 2 5c0-2.2 2.02-4 4.5-4 1.22 0 2.36.42 3.22 1.19C10.55 2.94 11 3.94 11 5c0 .66-.44 1.78-.86 2.48zM4 14h5c-.23 1.14-1.3 2-2.5 2s-2.27-.86-2.5-2z"})))),"Tip")),Object(c.b)("div",Object(t.a)({parentName:"div"},{className:"admonition-content"}),Object(c.b)("p",{parentName:"div"},"Use a ",Object(c.b)("a",Object(t.a)({parentName:"p"},{href:"feel-boolean-expressions#null-check"}),"null-check")," if the variable can be ",Object(c.b)("inlineCode",{parentName:"p"},"null")," or is optional.  "),Object(c.b)("pre",{parentName:"div"},Object(c.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"a != null and a.b > 10 \n")))),Object(c.b)("h3",{id:"escape-variable-names"},"Escape Variable Names"),Object(c.b)("p",null,"The name of a variable can be any alphanumeric string including ",Object(c.b)("inlineCode",{parentName:"p"},"_")," (an underscore). For a\ncombination of words, it is recommended to use the ",Object(c.b)("inlineCode",{parentName:"p"},"camelCase")," or the ",Object(c.b)("inlineCode",{parentName:"p"},"snake_case")," format."),Object(c.b)("p",null,"If a variable name or context key contains any special character (e.g. whitespace, dash, etc.) then\nthe name can be wrapped into single backquotes/backticks (e.g. ",Object(c.b)("inlineCode",{parentName:"p"},"`foo bar`"),")."),Object(c.b)("pre",null,Object(c.b)("code",Object(t.a)({parentName:"pre"},{className:"language-js"}),"`first name`\n\n`tracking-id`\n\norder.`total price`\n")))}b.isMDXComponent=!0}}]);