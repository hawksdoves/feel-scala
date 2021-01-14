(window.webpackJsonp=window.webpackJsonp||[]).push([[7],{142:function(e,t,n){"use strict";n.d(t,"a",(function(){return p})),n.d(t,"b",(function(){return b}));var a=n(0),r=n.n(a);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function c(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=r.a.createContext({}),u=function(e){var t=r.a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):c(c({},t),e)),n},p=function(e){var t=u(e.components);return r.a.createElement(s.Provider,{value:t},e.children)},d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},m=r.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,l=e.parentName,s=o(e,["components","mdxType","originalType","parentName"]),p=u(n),m=a,b=p["".concat(l,".").concat(m)]||p[m]||d[m]||i;return n?r.a.createElement(b,c(c({ref:t},s),{},{components:n})):r.a.createElement(b,c({ref:t},s))}));function b(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,l=new Array(i);l[0]=m;var c={};for(var o in t)hasOwnProperty.call(t,o)&&(c[o]=t[o]);c.originalType=e,c.mdxType="string"==typeof e?e:a,l[1]=c;for(var s=2;s<i;s++)l[s]=n[s];return r.a.createElement.apply(null,l)}return r.a.createElement.apply(null,n)}m.displayName="MDXCreateElement"},74:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return l})),n.d(t,"metadata",(function(){return c})),n.d(t,"rightToc",(function(){return o})),n.d(t,"default",(function(){return u}));var a=n(3),r=n(7),i=(n(0),n(142)),l={id:"samples",title:"Samples",slug:"/samples/"},c={unversionedId:"samples/samples",id:"version-1.11/samples/samples",isDocsHomePage:!1,title:"Samples",description:"Some example FEEL expressions which are used with the engine. Feel free to add your examples ;)",source:"@site/versioned_docs/version-1.11/samples/samples.md",slug:"/samples/",permalink:"/feel-scala/docs/1.11/samples/",editUrl:"https://github.com/camunda/feel-scala/edit/master/docs/versioned_docs/version-1.11/samples/samples.md",version:"1.11",sidebar:"version-1.11/Samples"},o=[{value:"Date/Time Calculation",id:"datetime-calculation",children:[]},{value:"Filter a List and Return the first Element",id:"filter-a-list-and-return-the-first-element",children:[]},{value:"Validate Data using a Context",id:"validate-data-using-a-context",children:[]},{value:"Structure Calculation using a Context",id:"structure-calculation-using-a-context",children:[]}],s={rightToc:o};function u(e){var t=e.components,n=Object(r.a)(e,["components"]);return Object(i.b)("wrapper",Object(a.a)({},s,n,{components:t,mdxType:"MDXLayout"}),Object(i.b)("p",null,"Some example FEEL expressions which are used with the engine. Feel free to add your examples ;)"),Object(i.b)("h3",{id:"datetime-calculation"},"Date/Time Calculation"),Object(i.b)("p",null,"Example: check if a date is at least 6 months before another."),Object(i.b)("pre",null,Object(i.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),'date1 < date2 + duration("P6M")\n')),Object(i.b)("h3",{id:"filter-a-list-and-return-the-first-element"},"Filter a List and Return the first Element"),Object(i.b)("p",null,'Example: return the first packaging element which unit is "Palette".'),Object(i.b)("pre",null,Object(i.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),'(data.attribute.packaging[unit = "Palette"])[1]\n')),Object(i.b)("h3",{id:"validate-data-using-a-context"},"Validate Data using a Context"),Object(i.b)("p",null,"Example: validate journal entries and return all violations."),Object(i.b)("pre",null,Object(i.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),'{\n  check1: {\n    error: "Document Type invalid for current year posting",\n    violations: collection[documentType = "S2" and glDate > startFiscalYear] \n  },\n  check2: {\n    error: "Document Type invalid for current year posting",\n    violations: collection[ledgerType = "GP" and foreignAmount != null] \n  },\n  result: ([check1, check2])[count(violations) > 0] \n}\n')),Object(i.b)("h3",{id:"structure-calculation-using-a-context"},"Structure Calculation using a Context"),Object(i.b)("p",null,"Example: calculate the minimum age of a given list of birthdays."),Object(i.b)("pre",null,Object(i.b)("code",Object(a.a)({parentName:"pre"},{className:"language-js"}),"{\n  age: function(birthday) (today - birthday).years,\n  ages: for birthday in birthdays return age(birthday),\n  minAge: min(ages)\n}.minAge\n")))}u.isMDXComponent=!0}}]);