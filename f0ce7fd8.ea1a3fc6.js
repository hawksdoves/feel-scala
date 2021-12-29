(window.webpackJsonp=window.webpackJsonp||[]).push([[128],{200:function(e,n,t){"use strict";t.r(n),t.d(n,"frontMatter",(function(){return u})),t.d(n,"metadata",(function(){return l})),t.d(n,"toc",(function(){return s})),t.d(n,"default",(function(){return f}));var r=t(3),a=t(7),i=(t(0),t(215)),o=t(219),c=t(220),u={id:"function-provider-spi",title:"Function Provider SPI"},l={unversionedId:"reference/developer-guide/function-provider-spi",id:"version-1.12/reference/developer-guide/function-provider-spi",isDocsHomePage:!1,title:"Function Provider SPI",description:"Functions can be invoked in expressions and unary tests. The engine includes some predefined built-in functions.",source:"@site/versioned_docs/version-1.12/reference/developer-guide/function-provider-spi.md",slug:"/reference/developer-guide/function-provider-spi",permalink:"/feel-scala/docs/1.12/reference/developer-guide/function-provider-spi",editUrl:"https://github.com/camunda/feel-scala/edit/master/docs/versioned_docs/version-1.12/reference/developer-guide/function-provider-spi.md",version:"1.12",sidebar:"version-1.12/Reference",previous:{title:"Temporal Functions",permalink:"/feel-scala/docs/1.12/reference/builtin-functions/feel-built-in-functions-temporal"},next:{title:"Value Mapper SPI",permalink:"/feel-scala/docs/1.12/reference/developer-guide/value-mapper-spi"}},s=[{value:"Register the Function",id:"register-the-function",children:[]}],p={toc:s};function f(e){var n=e.components,t=Object(a.a)(e,["components"]);return Object(i.b)("wrapper",Object(r.a)({},p,t,{components:n,mdxType:"MDXLayout"}),Object(i.b)("p",null,"Functions can be invoked in expressions and unary tests. The engine includes some predefined built-in functions."),Object(i.b)("p",null,"Own functions can be defined in two ways:"),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"declaring them in an expression (e.g. a context)"),Object(i.b)("li",{parentName:"ul"},"via the function provider SPI")),Object(i.b)("p",null,"Using the SPI, the function can be implemented in Scala/Java and is not limited by FEEL. So, it's possible to use language features or libraries.  "),Object(i.b)(o.a,{defaultValue:"scala",values:[{label:"Scala",value:"scala"},{label:"Java",value:"java"}],mdxType:"Tabs"},Object(i.b)(c.a,{value:"scala",mdxType:"TabItem"},Object(i.b)("p",null,"Create a sub-class of ",Object(i.b)("inlineCode",{parentName:"p"},"org.camunda.feel.context.CustomFunctionProvider")," and implement the method ",Object(i.b)("inlineCode",{parentName:"p"},"getFunction()")," which returns the function for the given name. If a function can have different parameters (i.e. different parameter count) then override ",Object(i.b)("inlineCode",{parentName:"p"},"getFunctions()")," instead."),Object(i.b)("pre",null,Object(i.b)("code",Object(r.a)({parentName:"pre"},{className:"language-scala"}),'class CustomScalaFunctionProvider extends CustomFunctionProvider {\n\n  def getFunction(name: String): Option[ValFunction] = functions.get(name)\n\n  def functionNames: Iterable[String] = functions.keys\n\n  val functions: Map[String, ValFunction] = Map(\n    "incr" -> ValFunction(\n      params = List("x"),\n      invoke = { case List(ValNumber(x)) => ValNumber(x + 1) }\n    )\n  )\n\n}\n')),Object(i.b)("p",null,"The function must be of type ",Object(i.b)("inlineCode",{parentName:"p"},"ValFunction"),". It contains"),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},Object(i.b)("inlineCode",{parentName:"li"},"params")," - list of the named parameters of the function"),Object(i.b)("li",{parentName:"ul"},Object(i.b)("inlineCode",{parentName:"li"},"invoke")," - business logic as function which takes the arguments and returns the result. The order of the arguments is defined by the parameter list.  "),Object(i.b)("li",{parentName:"ul"},Object(i.b)("inlineCode",{parentName:"li"},"hasVarArgs")," - if ",Object(i.b)("inlineCode",{parentName:"li"},"true")," the function can have variable arguments for the last parameter. The last argument is of type list. "))),Object(i.b)(c.a,{value:"java",mdxType:"TabItem"},"Using Java, create a sub-class of `org.camunda.feel.context.JavaFunctionProvider` instead. It is equal to the Scala one but uses more Java-like classes.",Object(i.b)("pre",null,Object(i.b)("code",Object(r.a)({parentName:"pre"},{className:"language-java"}),'public class CustomJavaFunctionProvider extends JavaFunctionProvider\n{\n    private static final Map<String, JavaFunction> functions = new HashMap<>();\n\n    static {\n    \n        final JavaFunction function = new JavaFunction(Arrays.asList("x"), args -> {\n            final ValNumber arg = (ValNumber) args.get(0);\n\n            int x = arg.value().intValue();\n\n            return new ValNumber(BigDecimal.valueOf(x - 1));\n        });\n\n        functions.put("decr", function);\n    }\n\n    @Override\n    public Optional<JavaFunction> resolveFunction(String functionName)\n    {\n      return Optional.ofNullable(functions.get(functionName));\n    }\n \n    @Override\n    public Collection<String> getFunctionNames() {\n      return functions.keySet();\n    }\n\n}\n')))),Object(i.b)("h2",{id:"register-the-function"},"Register the Function"),Object(i.b)("p",null,"Depending how the FEEL engine is used, the function provider can be passed directly on creation, or is loaded via Java ServiceLoader mechanism. "),Object(i.b)("p",null,"In the second case, create a new file ",Object(i.b)("inlineCode",{parentName:"p"},"org.camunda.feel.context.CustomFunctionProvider")," in the folder ",Object(i.b)("inlineCode",{parentName:"p"},"META-INF/services/"),". It must contain all function providers by their full qualified name."),Object(i.b)("pre",null,Object(i.b)("code",Object(r.a)({parentName:"pre"},{}),"org.camunda.feel.example.context.CustomScalaFunctionProvider\norg.camunda.feel.example.context.CustomJavaFunctionProvider\n")))}f.isMDXComponent=!0},215:function(e,n,t){"use strict";t.d(n,"a",(function(){return p})),t.d(n,"b",(function(){return b}));var r=t(0),a=t.n(r);function i(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function o(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);n&&(r=r.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,r)}return t}function c(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?o(Object(t),!0).forEach((function(n){i(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):o(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function u(e,n){if(null==e)return{};var t,r,a=function(e,n){if(null==e)return{};var t,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||(a[t]=e[t]);return a}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(a[t]=e[t])}return a}var l=a.a.createContext({}),s=function(e){var n=a.a.useContext(l),t=n;return e&&(t="function"==typeof e?e(n):c(c({},n),e)),t},p=function(e){var n=s(e.components);return a.a.createElement(l.Provider,{value:n},e.children)},f={inlineCode:"code",wrapper:function(e){var n=e.children;return a.a.createElement(a.a.Fragment,{},n)}},d=a.a.forwardRef((function(e,n){var t=e.components,r=e.mdxType,i=e.originalType,o=e.parentName,l=u(e,["components","mdxType","originalType","parentName"]),p=s(t),d=r,b=p["".concat(o,".").concat(d)]||p[d]||f[d]||i;return t?a.a.createElement(b,c(c({ref:n},l),{},{components:t})):a.a.createElement(b,c({ref:n},l))}));function b(e,n){var t=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var i=t.length,o=new Array(i);o[0]=d;var c={};for(var u in n)hasOwnProperty.call(n,u)&&(c[u]=n[u]);c.originalType=e,c.mdxType="string"==typeof e?e:r,o[1]=c;for(var l=2;l<i;l++)o[l]=t[l];return a.a.createElement.apply(null,o)}return a.a.createElement.apply(null,t)}d.displayName="MDXCreateElement"},216:function(e,n,t){"use strict";function r(e){var n,t,a="";if("string"==typeof e||"number"==typeof e)a+=e;else if("object"==typeof e)if(Array.isArray(e))for(n=0;n<e.length;n++)e[n]&&(t=r(e[n]))&&(a&&(a+=" "),a+=t);else for(n in e)e[n]&&(a&&(a+=" "),a+=n);return a}n.a=function(){for(var e,n,t=0,a="";t<arguments.length;)(e=arguments[t++])&&(n=r(e))&&(a&&(a+=" "),a+=n);return a}},217:function(e,n,t){"use strict";var r=t(0),a=t(218);n.a=function(){var e=Object(r.useContext)(a.a);if(null==e)throw new Error("`useUserPreferencesContext` is used outside of `Layout` Component.");return e}},218:function(e,n,t){"use strict";var r=t(0),a=Object(r.createContext)(void 0);n.a=a},219:function(e,n,t){"use strict";var r=t(0),a=t.n(r),i=t(217),o=t(216),c=t(54),u=t.n(c),l=37,s=39;n.a=function(e){var n=e.lazy,t=e.block,c=e.defaultValue,p=e.values,f=e.groupId,d=e.className,b=Object(i.a)(),m=b.tabGroupChoices,v=b.setTabGroupChoices,g=Object(r.useState)(c),O=g[0],h=g[1],j=r.Children.toArray(e.children);if(null!=f){var y=m[f];null!=y&&y!==O&&p.some((function(e){return e.value===y}))&&h(y)}var N=function(e){h(e),null!=f&&v(f,e)},x=[];return a.a.createElement("div",null,a.a.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:Object(o.a)("tabs",{"tabs--block":t},d)},p.map((function(e){var n=e.value,t=e.label;return a.a.createElement("li",{role:"tab",tabIndex:0,"aria-selected":O===n,className:Object(o.a)("tabs__item",u.a.tabItem,{"tabs__item--active":O===n}),key:n,ref:function(e){return x.push(e)},onKeyDown:function(e){!function(e,n,t){switch(t.keyCode){case s:!function(e,n){var t=e.indexOf(n)+1;e[t]?e[t].focus():e[0].focus()}(e,n);break;case l:!function(e,n){var t=e.indexOf(n)-1;e[t]?e[t].focus():e[e.length-1].focus()}(e,n)}}(x,e.target,e)},onFocus:function(){return N(n)},onClick:function(){N(n)}},t)}))),n?Object(r.cloneElement)(j.filter((function(e){return e.props.value===O}))[0],{className:"margin-vert--md"}):a.a.createElement("div",{className:"margin-vert--md"},j.map((function(e,n){return Object(r.cloneElement)(e,{key:n,hidden:e.props.value!==O})}))))}},220:function(e,n,t){"use strict";var r=t(3),a=t(0),i=t.n(a);n.a=function(e){var n=e.children,t=e.hidden,a=e.className;return i.a.createElement("div",Object(r.a)({role:"tabpanel"},{hidden:t,className:a}),n)}}}]);