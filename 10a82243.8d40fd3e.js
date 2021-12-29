(window.webpackJsonp=window.webpackJsonp||[]).push([[11],{215:function(e,n,t){"use strict";t.d(n,"a",(function(){return s})),t.d(n,"b",(function(){return j}));var a=t(0),l=t.n(a);function r(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function b(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?i(Object(t),!0).forEach((function(n){r(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):i(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function c(e,n){if(null==e)return{};var t,a,l=function(e,n){if(null==e)return{};var t,a,l={},r=Object.keys(e);for(a=0;a<r.length;a++)t=r[a],n.indexOf(t)>=0||(l[t]=e[t]);return l}(e,n);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)t=r[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(l[t]=e[t])}return l}var o=l.a.createContext({}),u=function(e){var n=l.a.useContext(o),t=n;return e&&(t="function"==typeof e?e(n):b(b({},n),e)),t},s=function(e){var n=u(e.components);return l.a.createElement(o.Provider,{value:n},e.children)},d={inlineCode:"code",wrapper:function(e){var n=e.children;return l.a.createElement(l.a.Fragment,{},n)}},p=l.a.forwardRef((function(e,n){var t=e.components,a=e.mdxType,r=e.originalType,i=e.parentName,o=c(e,["components","mdxType","originalType","parentName"]),s=u(t),p=a,j=s["".concat(i,".").concat(p)]||s[p]||d[p]||r;return t?l.a.createElement(j,b(b({ref:n},o),{},{components:t})):l.a.createElement(j,b({ref:n},o))}));function j(e,n){var t=arguments,a=n&&n.mdxType;if("string"==typeof e||a){var r=t.length,i=new Array(r);i[0]=p;var b={};for(var c in n)hasOwnProperty.call(n,c)&&(b[c]=n[c]);b.originalType=e,b.mdxType="string"==typeof e?e:a,i[1]=b;for(var o=2;o<r;o++)i[o]=t[o];return l.a.createElement.apply(null,i)}return l.a.createElement.apply(null,t)}p.displayName="MDXCreateElement"},72:function(e,n,t){"use strict";t.r(n),t.d(n,"frontMatter",(function(){return i})),t.d(n,"metadata",(function(){return b})),t.d(n,"toc",(function(){return c})),t.d(n,"default",(function(){return u}));var a=t(3),l=t(7),r=(t(0),t(215)),i={id:"feel-boolean-expressions",title:"Boolean Expressions"},b={unversionedId:"reference/language-guide/feel-boolean-expressions",id:"version-1.14/reference/language-guide/feel-boolean-expressions",isDocsHomePage:!1,title:"Boolean Expressions",description:"Literal",source:"@site/versioned_docs/version-1.14/reference/language-guide/feel-boolean-expressions.md",slug:"/reference/language-guide/feel-boolean-expressions",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-boolean-expressions",editUrl:"https://github.com/camunda/feel-scala/edit/master/docs/versioned_docs/version-1.14/reference/language-guide/feel-boolean-expressions.md",version:"1.14",sidebar:"version-1.14/Reference",previous:{title:"Introduction",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-expressions-introduction"},next:{title:"String Expressions",permalink:"/feel-scala/docs/1.14/reference/language-guide/feel-string-expressions"}},c=[{value:"Literal",id:"literal",children:[]},{value:"Comparison",id:"comparison",children:[]},{value:"Null Check",id:"null-check",children:[]},{value:"Conjunction / And",id:"conjunction--and",children:[]},{value:"Disjunction / Or",id:"disjunction--or",children:[]},{value:"Instance of",id:"instance-of",children:[]},{value:"Unary-Tests / In",id:"unary-tests--in",children:[]}],o={toc:c};function u(e){var n=e.components,t=Object(l.a)(e,["components"]);return Object(r.b)("wrapper",Object(a.a)({},o,t,{components:n,mdxType:"MDXLayout"}),Object(r.b)("h3",{id:"literal"},"Literal"),Object(r.b)("p",null,"Creates a new boolean value."),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),"true\n\nfalse\n")),Object(r.b)("h3",{id:"comparison"},"Comparison"),Object(r.b)("p",null,"Two values of the same type can be compared using the following operators."),Object(r.b)("table",null,Object(r.b)("tr",null,Object(r.b)("th",null,"Operator"),Object(r.b)("th",null,"Description"),Object(r.b)("th",null,"Supported types")),Object(r.b)("tr",null,Object(r.b)("td",null,"="),Object(r.b)("td",null,"equal to"),Object(r.b)("td",null,"any")),Object(r.b)("tr",null,Object(r.b)("td",null,"!="),Object(r.b)("td",null,"not equal to"),Object(r.b)("td",null,"any")),Object(r.b)("tr",null,Object(r.b)("td",null,"<"),Object(r.b)("td",null,"less than"),Object(r.b)("td",null,"number, date, time, date-time, duration")),Object(r.b)("tr",null,Object(r.b)("td",null,"<="),Object(r.b)("td",null,"less than or equal to"),Object(r.b)("td",null,"number, date, time, date-time, duration")),Object(r.b)("tr",null,Object(r.b)("td",null,">"),Object(r.b)("td",null,"greater than"),Object(r.b)("td",null,"number, date, time, date-time, duration")),Object(r.b)("tr",null,Object(r.b)("td",null,">="),Object(r.b)("td",null,"greater than or equal"),Object(r.b)("td",null,"number, date, time, date-time, duration")),Object(r.b)("tr",null,Object(r.b)("td",null,"between [x] and [y]"),Object(r.b)("td",null,"same as (_ >= x and _ <= y)"),Object(r.b)("td",null,"number, date, time, date-time, duration"))),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),'5 = 5\n// true\n\n5 != 5\n// false\n\ndate("2020-04-05") < date("2020-04-06")\n// true\n\ntime("08:00:00") <= time("08:00:00")\n// true\n\nduration("P1D") > duration("P5D")\n// false\n\nduration("P1Y") >= duration("P6M")\n// true\n\n5 between 3 and 7\n// true\n\ndate("2020-04-06") between date("2020-04-05") and date("2020-04-09")\n// true\n')),Object(r.b)("div",{className:"admonition admonition-caution alert alert--warning"},Object(r.b)("div",Object(a.a)({parentName:"div"},{className:"admonition-heading"}),Object(r.b)("h5",{parentName:"div"},Object(r.b)("span",Object(a.a)({parentName:"h5"},{className:"admonition-icon"}),Object(r.b)("svg",Object(a.a)({parentName:"span"},{xmlns:"http://www.w3.org/2000/svg",width:"16",height:"16",viewBox:"0 0 16 16"}),Object(r.b)("path",Object(a.a)({parentName:"svg"},{fillRule:"evenodd",d:"M8.893 1.5c-.183-.31-.52-.5-.887-.5s-.703.19-.886.5L.138 13.499a.98.98 0 0 0 0 1.001c.193.31.53.501.886.501h13.964c.367 0 .704-.19.877-.5a1.03 1.03 0 0 0 .01-1.002L8.893 1.5zm.133 11.497H6.987v-2.003h2.039v2.003zm0-3.004H6.987V5.987h2.039v4.006z"})))),"be careful!")),Object(r.b)("div",Object(a.a)({parentName:"div"},{className:"admonition-content"}),Object(r.b)("p",{parentName:"div"},"The equal operator has only ",Object(r.b)("strong",{parentName:"p"},"one")," equal sign (e.g. ",Object(r.b)("inlineCode",{parentName:"p"},"x = y"),"). In other languages, the operator has two equal signs (e.g. ",Object(r.b)("inlineCode",{parentName:"p"},"x == y"),")."))),Object(r.b)("h3",{id:"null-check"},"Null Check"),Object(r.b)("p",null,"Any value or variable can be compared with ",Object(r.b)("inlineCode",{parentName:"p"},"null")," to check if it is equal to ",Object(r.b)("inlineCode",{parentName:"p"},"null"),", or if it exists."),Object(r.b)("p",null,"Comparing ",Object(r.b)("inlineCode",{parentName:"p"},"null")," to a value different from ",Object(r.b)("inlineCode",{parentName:"p"},"null")," results in ",Object(r.b)("inlineCode",{parentName:"p"},"false"),". It returns ",Object(r.b)("inlineCode",{parentName:"p"},"true")," if the\nvalue is ",Object(r.b)("inlineCode",{parentName:"p"},"null")," or the variable doesn't exist."),Object(r.b)("p",null,"Comparing a context entry with ",Object(r.b)("inlineCode",{parentName:"p"},"null")," results in ",Object(r.b)("inlineCode",{parentName:"p"},"true")," if the value of the entry is ",Object(r.b)("inlineCode",{parentName:"p"},"null")," or if\nthe context doesn't contain an entry with this key. "),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),'null = null\n// true\n\n"foo" = null\n// false\n\n{x: null}.x = null\n// true\n\n{}.y = null\n// true\n')),Object(r.b)("div",{className:"admonition admonition-tip alert alert--success"},Object(r.b)("div",Object(a.a)({parentName:"div"},{className:"admonition-heading"}),Object(r.b)("h5",{parentName:"div"},Object(r.b)("span",Object(a.a)({parentName:"h5"},{className:"admonition-icon"}),Object(r.b)("svg",Object(a.a)({parentName:"span"},{xmlns:"http://www.w3.org/2000/svg",width:"12",height:"16",viewBox:"0 0 12 16"}),Object(r.b)("path",Object(a.a)({parentName:"svg"},{fillRule:"evenodd",d:"M6.5 0C3.48 0 1 2.19 1 5c0 .92.55 2.25 1 3 1.34 2.25 1.78 2.78 2 4v1h5v-1c.22-1.22.66-1.75 2-4 .45-.75 1-2.08 1-3 0-2.81-2.48-5-5.5-5zm3.64 7.48c-.25.44-.47.8-.67 1.11-.86 1.41-1.25 2.06-1.45 3.23-.02.05-.02.11-.02.17H5c0-.06 0-.13-.02-.17-.2-1.17-.59-1.83-1.45-3.23-.2-.31-.42-.67-.67-1.11C2.44 6.78 2 5.65 2 5c0-2.2 2.02-4 4.5-4 1.22 0 2.36.42 3.22 1.19C10.55 2.94 11 3.94 11 5c0 .66-.44 1.78-.86 2.48zM4 14h5c-.23 1.14-1.3 2-2.5 2s-2.27-.86-2.5-2z"})))),"Tip")),Object(r.b)("div",Object(a.a)({parentName:"div"},{className:"admonition-content"}),Object(r.b)("p",{parentName:"div"},"The built-in\nfunction ",Object(r.b)("a",Object(a.a)({parentName:"p"},{href:"/feel-scala/docs/1.14/reference/builtin-functions/feel-built-in-functions-boolean#is-defined"}),"is defined()")," can be\nused to differentiate between a value that is ",Object(r.b)("inlineCode",{parentName:"p"},"null"),", and a variable or context entry that doesn't\nexist."),Object(r.b)("pre",{parentName:"div"},Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),"is defined(null)\n// true\n\nis defined({x: null}.x)\n// true\n\nis defined({}.y)\n// false\n")))),Object(r.b)("h3",{id:"conjunction--and"},"Conjunction / And"),Object(r.b)("p",null,"Combines multiple boolean values following the ternary logic."),Object(r.b)("ul",null,Object(r.b)("li",{parentName:"ul"},"the result is ",Object(r.b)("inlineCode",{parentName:"li"},"true")," if all values are ",Object(r.b)("inlineCode",{parentName:"li"},"true")),Object(r.b)("li",{parentName:"ul"},"the result is ",Object(r.b)("inlineCode",{parentName:"li"},"false")," if one value is ",Object(r.b)("inlineCode",{parentName:"li"},"false")),Object(r.b)("li",{parentName:"ul"},"otherwise, the result is ",Object(r.b)("inlineCode",{parentName:"li"},"null")," (i.e. if a value is not a boolean)  ")),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),'true and true\n// true\n\ntrue and false\n// false\n\ntrue and null\n// null\n\ntrue and "otherwise"\n// null\n\nfalse and null\n// false\n\nfalse and "otherwise"\n// false\n')),Object(r.b)("h3",{id:"disjunction--or"},"Disjunction / Or"),Object(r.b)("p",null,"Combines multiple boolean values following the ternary logic."),Object(r.b)("ul",null,Object(r.b)("li",{parentName:"ul"},"the result is ",Object(r.b)("inlineCode",{parentName:"li"},"true")," if at least one value is ",Object(r.b)("inlineCode",{parentName:"li"},"true")),Object(r.b)("li",{parentName:"ul"},"the result is ",Object(r.b)("inlineCode",{parentName:"li"},"false")," if all values are ",Object(r.b)("inlineCode",{parentName:"li"},"false")),Object(r.b)("li",{parentName:"ul"},"otherwise, the result is ",Object(r.b)("inlineCode",{parentName:"li"},"null")," (i.e. if a value is not a boolean)")),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),'true or false   \n// true\n\nfalse or false  \n// false\n\ntrue or null   \n// true\n\ntrue or "otherwise"\n// true\n\nfalse or null  \n// null\n\nfalse or "otherwise"\n// null\n')),Object(r.b)("h3",{id:"instance-of"},"Instance of"),Object(r.b)("p",null,"Checks if the value is of the given type. Available type names:"),Object(r.b)("ul",null,Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"boolean")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"number")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"string")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"date")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"time")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"date time")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"day-time-duration")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"year-month-duration")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"list")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"context")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"function")),Object(r.b)("li",{parentName:"ul"},Object(r.b)("inlineCode",{parentName:"li"},"Any"))),Object(r.b)("p",null,"Use the type ",Object(r.b)("inlineCode",{parentName:"p"},"Any")," to check if the value is not ",Object(r.b)("inlineCode",{parentName:"p"},"null"),"."),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),"1 instance of number\n// true\n\n1 instance of string\n// false\n\n1 instance of Any\n// true\n\nnull instance of Any\n// false\n")),Object(r.b)("h3",{id:"unary-tests--in"},"Unary-Tests / In"),Object(r.b)("p",null,"Evaluates a ",Object(r.b)("a",Object(a.a)({parentName:"p"},{href:"feel-unary-tests"}),"unary-tests")," with the given value. The keyword ",Object(r.b)("inlineCode",{parentName:"p"},"in")," separates the value from the unary-tests."),Object(r.b)("pre",null,Object(r.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),'5 in (3..7)\n// true\n\ndate("2021-06-04") in [date("2021-05-01")..date("2021-05-31")]\n// false\n\n5 in (3,5,7)\n// true\n\n5 in [2,4,6,8]\n// false\n')))}u.isMDXComponent=!0}}]);