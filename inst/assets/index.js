(()=>{var e={390:e=>{function n(e){return e instanceof Map?e.clear=e.delete=e.set=function(){throw new Error("map is read-only")}:e instanceof Set&&(e.add=e.clear=e.delete=function(){throw new Error("set is read-only")}),Object.freeze(e),Object.getOwnPropertyNames(e).forEach((t=>{const i=e[t],s=typeof i;"object"!==s&&"function"!==s||Object.isFrozen(i)||n(i)})),e}class t{constructor(e){void 0===e.data&&(e.data={}),this.data=e.data,this.isMatchIgnored=!1}ignoreMatch(){this.isMatchIgnored=!0}}function i(e){return e.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#x27;")}function s(e,...n){const t=Object.create(null);for(const n in e)t[n]=e[n];return n.forEach((function(e){for(const n in e)t[n]=e[n]})),t}const o=e=>!!e.scope;class a{constructor(e,n){this.buffer="",this.classPrefix=n.classPrefix,e.walk(this)}addText(e){this.buffer+=i(e)}openNode(e){if(!o(e))return;const n=((e,{prefix:n})=>{if(e.startsWith("language:"))return e.replace("language:","language-");if(e.includes(".")){const t=e.split(".");return[`${n}${t.shift()}`,...t.map(((e,n)=>`${e}${"_".repeat(n+1)}`))].join(" ")}return`${n}${e}`})(e.scope,{prefix:this.classPrefix});this.span(n)}closeNode(e){o(e)&&(this.buffer+="</span>")}value(){return this.buffer}span(e){this.buffer+=`<span class="${e}">`}}const r=(e={})=>{const n={children:[]};return Object.assign(n,e),n};class c{constructor(){this.rootNode=r(),this.stack=[this.rootNode]}get top(){return this.stack[this.stack.length-1]}get root(){return this.rootNode}add(e){this.top.children.push(e)}openNode(e){const n=r({scope:e});this.add(n),this.stack.push(n)}closeNode(){if(this.stack.length>1)return this.stack.pop()}closeAllNodes(){for(;this.closeNode(););}toJSON(){return JSON.stringify(this.rootNode,null,4)}walk(e){return this.constructor._walk(e,this.rootNode)}static _walk(e,n){return"string"==typeof n?e.addText(n):n.children&&(e.openNode(n),n.children.forEach((n=>this._walk(e,n))),e.closeNode(n)),e}static _collapse(e){"string"!=typeof e&&e.children&&(e.children.every((e=>"string"==typeof e))?e.children=[e.children.join("")]:e.children.forEach((e=>{c._collapse(e)})))}}class l extends c{constructor(e){super(),this.options=e}addText(e){""!==e&&this.add(e)}startScope(e){this.openNode(e)}endScope(){this.closeNode()}__addSublanguage(e,n){const t=e.root;n&&(t.scope=`language:${n}`),this.add(t)}toHTML(){return new a(this,this.options).value()}finalize(){return this.closeAllNodes(),!0}}function d(e){return e?"string"==typeof e?e:e.source:null}function u(e){return f("(?=",e,")")}function g(e){return f("(?:",e,")*")}function h(e){return f("(?:",e,")?")}function f(...e){return e.map((e=>d(e))).join("")}function p(...e){const n=function(e){const n=e[e.length-1];return"object"==typeof n&&n.constructor===Object?(e.splice(e.length-1,1),n):{}}(e);return"("+(n.capture?"":"?:")+e.map((e=>d(e))).join("|")+")"}function b(e){return new RegExp(e.toString()+"|").exec("").length-1}const m=/\[(?:[^\\\]]|\\.)*\]|\(\??|\\([1-9][0-9]*)|\\./;function E(e,{joinWith:n}){let t=0;return e.map((e=>{t+=1;const n=t;let i=d(e),s="";for(;i.length>0;){const e=m.exec(i);if(!e){s+=i;break}s+=i.substring(0,e.index),i=i.substring(e.index+e[0].length),"\\"===e[0][0]&&e[1]?s+="\\"+String(Number(e[1])+n):(s+=e[0],"("===e[0]&&t++)}return s})).map((e=>`(${e})`)).join(n)}const x="[a-zA-Z]\\w*",_="[a-zA-Z_]\\w*",w="\\b\\d+(\\.\\d+)?",k="(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)",y="\\b(0b[01]+)",v={begin:"\\\\[\\s\\S]",relevance:0},S={scope:"string",begin:"'",end:"'",illegal:"\\n",contains:[v]},A={scope:"string",begin:'"',end:'"',illegal:"\\n",contains:[v]},N=function(e,n,t={}){const i=s({scope:"comment",begin:e,end:n,contains:[]},t);i.contains.push({scope:"doctag",begin:"[ ]*(?=(TODO|FIXME|NOTE|BUG|OPTIMIZE|HACK|XXX):)",end:/(TODO|FIXME|NOTE|BUG|OPTIMIZE|HACK|XXX):/,excludeBegin:!0,relevance:0});const o=p("I","a","is","so","us","to","at","if","in","it","on",/[A-Za-z]+['](d|ve|re|ll|t|s|n)/,/[A-Za-z]+[-][a-z]+/,/[A-Za-z][a-z]{2,}/);return i.contains.push({begin:f(/[ ]+/,"(",o,/[.]?[:]?([.][ ]|[ ])/,"){3}")}),i},M=N("//","$"),$=N("/\\*","\\*/"),O=N("#","$"),C={scope:"number",begin:w,relevance:0},R={scope:"number",begin:k,relevance:0},T={scope:"number",begin:y,relevance:0},I={scope:"regexp",begin:/\/(?=[^/\n]*\/)/,end:/\/[gimuy]*/,contains:[v,{begin:/\[/,end:/\]/,relevance:0,contains:[v]}]},j={scope:"title",begin:x,relevance:0},L={scope:"title",begin:_,relevance:0},B={begin:"\\.\\s*"+_,relevance:0};var H=Object.freeze({__proto__:null,APOS_STRING_MODE:S,BACKSLASH_ESCAPE:v,BINARY_NUMBER_MODE:T,BINARY_NUMBER_RE:y,COMMENT:N,C_BLOCK_COMMENT_MODE:$,C_LINE_COMMENT_MODE:M,C_NUMBER_MODE:R,C_NUMBER_RE:k,END_SAME_AS_BEGIN:function(e){return Object.assign(e,{"on:begin":(e,n)=>{n.data._beginMatch=e[1]},"on:end":(e,n)=>{n.data._beginMatch!==e[1]&&n.ignoreMatch()}})},HASH_COMMENT_MODE:O,IDENT_RE:x,MATCH_NOTHING_RE:/\b\B/,METHOD_GUARD:B,NUMBER_MODE:C,NUMBER_RE:w,PHRASAL_WORDS_MODE:{begin:/\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/},QUOTE_STRING_MODE:A,REGEXP_MODE:I,RE_STARTERS_RE:"!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~",SHEBANG:(e={})=>{const n=/^#![ ]*\//;return e.binary&&(e.begin=f(n,/.*\b/,e.binary,/\b.*/)),s({scope:"meta",begin:n,end:/$/,relevance:0,"on:begin":(e,n)=>{0!==e.index&&n.ignoreMatch()}},e)},TITLE_MODE:j,UNDERSCORE_IDENT_RE:_,UNDERSCORE_TITLE_MODE:L});function D(e,n){"."===e.input[e.index-1]&&n.ignoreMatch()}function P(e,n){void 0!==e.className&&(e.scope=e.className,delete e.className)}function z(e,n){n&&e.beginKeywords&&(e.begin="\\b("+e.beginKeywords.split(" ").join("|")+")(?!\\.)(?=\\b|\\s)",e.__beforeBegin=D,e.keywords=e.keywords||e.beginKeywords,delete e.beginKeywords,void 0===e.relevance&&(e.relevance=0))}function U(e,n){Array.isArray(e.illegal)&&(e.illegal=p(...e.illegal))}function G(e,n){if(e.match){if(e.begin||e.end)throw new Error("begin & end are not supported with match");e.begin=e.match,delete e.match}}function Z(e,n){void 0===e.relevance&&(e.relevance=1)}const F=(e,n)=>{if(!e.beforeMatch)return;if(e.starts)throw new Error("beforeMatch cannot be used with starts");const t=Object.assign({},e);Object.keys(e).forEach((n=>{delete e[n]})),e.keywords=t.keywords,e.begin=f(t.beforeMatch,u(t.begin)),e.starts={relevance:0,contains:[Object.assign(t,{endsParent:!0})]},e.relevance=0,delete t.beforeMatch},W=["of","and","for","in","not","or","if","then","parent","list","value"],X="keyword";function K(e,n,t=X){const i=Object.create(null);return"string"==typeof e?s(t,e.split(" ")):Array.isArray(e)?s(t,e):Object.keys(e).forEach((function(t){Object.assign(i,K(e[t],n,t))})),i;function s(e,t){n&&(t=t.map((e=>e.toLowerCase()))),t.forEach((function(n){const t=n.split("|");i[t[0]]=[e,q(t[0],t[1])]}))}}function q(e,n){return n?Number(n):function(e){return W.includes(e.toLowerCase())}(e)?0:1}const V={},J=e=>{console.error(e)},Y=(e,...n)=>{console.log(`WARN: ${e}`,...n)},Q=(e,n)=>{V[`${e}/${n}`]||(console.log(`Deprecated as of ${e}. ${n}`),V[`${e}/${n}`]=!0)},ee=new Error;function ne(e,n,{key:t}){let i=0;const s=e[t],o={},a={};for(let e=1;e<=n.length;e++)a[e+i]=s[e],o[e+i]=!0,i+=b(n[e-1]);e[t]=a,e[t]._emit=o,e[t]._multi=!0}function te(e){!function(e){e.scope&&"object"==typeof e.scope&&null!==e.scope&&(e.beginScope=e.scope,delete e.scope)}(e),"string"==typeof e.beginScope&&(e.beginScope={_wrap:e.beginScope}),"string"==typeof e.endScope&&(e.endScope={_wrap:e.endScope}),function(e){if(Array.isArray(e.begin)){if(e.skip||e.excludeBegin||e.returnBegin)throw J("skip, excludeBegin, returnBegin not compatible with beginScope: {}"),ee;if("object"!=typeof e.beginScope||null===e.beginScope)throw J("beginScope must be object"),ee;ne(e,e.begin,{key:"beginScope"}),e.begin=E(e.begin,{joinWith:""})}}(e),function(e){if(Array.isArray(e.end)){if(e.skip||e.excludeEnd||e.returnEnd)throw J("skip, excludeEnd, returnEnd not compatible with endScope: {}"),ee;if("object"!=typeof e.endScope||null===e.endScope)throw J("endScope must be object"),ee;ne(e,e.end,{key:"endScope"}),e.end=E(e.end,{joinWith:""})}}(e)}function ie(e){function n(n,t){return new RegExp(d(n),"m"+(e.case_insensitive?"i":"")+(e.unicodeRegex?"u":"")+(t?"g":""))}class t{constructor(){this.matchIndexes={},this.regexes=[],this.matchAt=1,this.position=0}addRule(e,n){n.position=this.position++,this.matchIndexes[this.matchAt]=n,this.regexes.push([n,e]),this.matchAt+=b(e)+1}compile(){0===this.regexes.length&&(this.exec=()=>null);const e=this.regexes.map((e=>e[1]));this.matcherRe=n(E(e,{joinWith:"|"}),!0),this.lastIndex=0}exec(e){this.matcherRe.lastIndex=this.lastIndex;const n=this.matcherRe.exec(e);if(!n)return null;const t=n.findIndex(((e,n)=>n>0&&void 0!==e)),i=this.matchIndexes[t];return n.splice(0,t),Object.assign(n,i)}}class i{constructor(){this.rules=[],this.multiRegexes=[],this.count=0,this.lastIndex=0,this.regexIndex=0}getMatcher(e){if(this.multiRegexes[e])return this.multiRegexes[e];const n=new t;return this.rules.slice(e).forEach((([e,t])=>n.addRule(e,t))),n.compile(),this.multiRegexes[e]=n,n}resumingScanAtSamePosition(){return 0!==this.regexIndex}considerAll(){this.regexIndex=0}addRule(e,n){this.rules.push([e,n]),"begin"===n.type&&this.count++}exec(e){const n=this.getMatcher(this.regexIndex);n.lastIndex=this.lastIndex;let t=n.exec(e);if(this.resumingScanAtSamePosition())if(t&&t.index===this.lastIndex);else{const n=this.getMatcher(0);n.lastIndex=this.lastIndex+1,t=n.exec(e)}return t&&(this.regexIndex+=t.position+1,this.regexIndex===this.count&&this.considerAll()),t}}if(e.compilerExtensions||(e.compilerExtensions=[]),e.contains&&e.contains.includes("self"))throw new Error("ERR: contains `self` is not supported at the top-level of a language.  See documentation.");return e.classNameAliases=s(e.classNameAliases||{}),function t(o,a){const r=o;if(o.isCompiled)return r;[P,G,te,F].forEach((e=>e(o,a))),e.compilerExtensions.forEach((e=>e(o,a))),o.__beforeBegin=null,[z,U,Z].forEach((e=>e(o,a))),o.isCompiled=!0;let c=null;return"object"==typeof o.keywords&&o.keywords.$pattern&&(o.keywords=Object.assign({},o.keywords),c=o.keywords.$pattern,delete o.keywords.$pattern),c=c||/\w+/,o.keywords&&(o.keywords=K(o.keywords,e.case_insensitive)),r.keywordPatternRe=n(c,!0),a&&(o.begin||(o.begin=/\B|\b/),r.beginRe=n(r.begin),o.end||o.endsWithParent||(o.end=/\B|\b/),o.end&&(r.endRe=n(r.end)),r.terminatorEnd=d(r.end)||"",o.endsWithParent&&a.terminatorEnd&&(r.terminatorEnd+=(o.end?"|":"")+a.terminatorEnd)),o.illegal&&(r.illegalRe=n(o.illegal)),o.contains||(o.contains=[]),o.contains=[].concat(...o.contains.map((function(e){return function(e){return e.variants&&!e.cachedVariants&&(e.cachedVariants=e.variants.map((function(n){return s(e,{variants:null},n)}))),e.cachedVariants?e.cachedVariants:se(e)?s(e,{starts:e.starts?s(e.starts):null}):Object.isFrozen(e)?s(e):e}("self"===e?o:e)}))),o.contains.forEach((function(e){t(e,r)})),o.starts&&t(o.starts,a),r.matcher=function(e){const n=new i;return e.contains.forEach((e=>n.addRule(e.begin,{rule:e,type:"begin"}))),e.terminatorEnd&&n.addRule(e.terminatorEnd,{type:"end"}),e.illegal&&n.addRule(e.illegal,{type:"illegal"}),n}(r),r}(e)}function se(e){return!!e&&(e.endsWithParent||se(e.starts))}class oe extends Error{constructor(e,n){super(e),this.name="HTMLInjectionError",this.html=n}}const ae=i,re=s,ce=Symbol("nomatch"),le=function(e){const i=Object.create(null),s=Object.create(null),o=[];let a=!0;const r="Could not find the language '{}', did you forget to load/include a language module?",c={disableAutodetect:!0,name:"Plain text",contains:[]};let d={ignoreUnescapedHTML:!1,throwUnescapedHTML:!1,noHighlightRe:/^(no-?highlight)$/i,languageDetectRe:/\blang(?:uage)?-([\w-]+)\b/i,classPrefix:"hljs-",cssSelector:"pre code",languages:null,__emitter:l};function b(e){return d.noHighlightRe.test(e)}function m(e,n,t){let i="",s="";"object"==typeof n?(i=e,t=n.ignoreIllegals,s=n.language):(Q("10.7.0","highlight(lang, code, ...args) has been deprecated."),Q("10.7.0","Please use highlight(code, options) instead.\nhttps://github.com/highlightjs/highlight.js/issues/2277"),s=e,i=n),void 0===t&&(t=!0);const o={code:i,language:s};A("before:highlight",o);const a=o.result?o.result:E(o.language,o.code,t);return a.code=o.code,A("after:highlight",a),a}function E(e,n,s,o){const c=Object.create(null);function l(){if(!A.keywords)return void M.addText($);let e=0;A.keywordPatternRe.lastIndex=0;let n=A.keywordPatternRe.exec($),t="";for(;n;){t+=$.substring(e,n.index);const s=k.case_insensitive?n[0].toLowerCase():n[0],o=(i=s,A.keywords[i]);if(o){const[e,i]=o;if(M.addText(t),t="",c[s]=(c[s]||0)+1,c[s]<=7&&(O+=i),e.startsWith("_"))t+=n[0];else{const t=k.classNameAliases[e]||e;g(n[0],t)}}else t+=n[0];e=A.keywordPatternRe.lastIndex,n=A.keywordPatternRe.exec($)}var i;t+=$.substring(e),M.addText(t)}function u(){null!=A.subLanguage?function(){if(""===$)return;let e=null;if("string"==typeof A.subLanguage){if(!i[A.subLanguage])return void M.addText($);e=E(A.subLanguage,$,!0,N[A.subLanguage]),N[A.subLanguage]=e._top}else e=x($,A.subLanguage.length?A.subLanguage:null);A.relevance>0&&(O+=e.relevance),M.__addSublanguage(e._emitter,e.language)}():l(),$=""}function g(e,n){""!==e&&(M.startScope(n),M.addText(e),M.endScope())}function h(e,n){let t=1;const i=n.length-1;for(;t<=i;){if(!e._emit[t]){t++;continue}const i=k.classNameAliases[e[t]]||e[t],s=n[t];i?g(s,i):($=s,l(),$=""),t++}}function f(e,n){return e.scope&&"string"==typeof e.scope&&M.openNode(k.classNameAliases[e.scope]||e.scope),e.beginScope&&(e.beginScope._wrap?(g($,k.classNameAliases[e.beginScope._wrap]||e.beginScope._wrap),$=""):e.beginScope._multi&&(h(e.beginScope,n),$="")),A=Object.create(e,{parent:{value:A}}),A}function p(e,n,i){let s=function(e,n){const t=e&&e.exec(n);return t&&0===t.index}(e.endRe,i);if(s){if(e["on:end"]){const i=new t(e);e["on:end"](n,i),i.isMatchIgnored&&(s=!1)}if(s){for(;e.endsParent&&e.parent;)e=e.parent;return e}}if(e.endsWithParent)return p(e.parent,n,i)}function b(e){return 0===A.matcher.regexIndex?($+=e[0],1):(T=!0,0)}function m(e){const t=e[0],i=n.substring(e.index),s=p(A,e,i);if(!s)return ce;const o=A;A.endScope&&A.endScope._wrap?(u(),g(t,A.endScope._wrap)):A.endScope&&A.endScope._multi?(u(),h(A.endScope,e)):o.skip?$+=t:(o.returnEnd||o.excludeEnd||($+=t),u(),o.excludeEnd&&($=t));do{A.scope&&M.closeNode(),A.skip||A.subLanguage||(O+=A.relevance),A=A.parent}while(A!==s.parent);return s.starts&&f(s.starts,e),o.returnEnd?0:t.length}let _={};function w(i,o){const r=o&&o[0];if($+=i,null==r)return u(),0;if("begin"===_.type&&"end"===o.type&&_.index===o.index&&""===r){if($+=n.slice(o.index,o.index+1),!a){const n=new Error(`0 width match regex (${e})`);throw n.languageName=e,n.badRule=_.rule,n}return 1}if(_=o,"begin"===o.type)return function(e){const n=e[0],i=e.rule,s=new t(i),o=[i.__beforeBegin,i["on:begin"]];for(const t of o)if(t&&(t(e,s),s.isMatchIgnored))return b(n);return i.skip?$+=n:(i.excludeBegin&&($+=n),u(),i.returnBegin||i.excludeBegin||($=n)),f(i,e),i.returnBegin?0:n.length}(o);if("illegal"===o.type&&!s){const e=new Error('Illegal lexeme "'+r+'" for mode "'+(A.scope||"<unnamed>")+'"');throw e.mode=A,e}if("end"===o.type){const e=m(o);if(e!==ce)return e}if("illegal"===o.type&&""===r)return 1;if(R>1e5&&R>3*o.index)throw new Error("potential infinite loop, way more iterations than matches");return $+=r,r.length}const k=y(e);if(!k)throw J(r.replace("{}",e)),new Error('Unknown language: "'+e+'"');const v=ie(k);let S="",A=o||v;const N={},M=new d.__emitter(d);!function(){const e=[];for(let n=A;n!==k;n=n.parent)n.scope&&e.unshift(n.scope);e.forEach((e=>M.openNode(e)))}();let $="",O=0,C=0,R=0,T=!1;try{if(k.__emitTokens)k.__emitTokens(n,M);else{for(A.matcher.considerAll();;){R++,T?T=!1:A.matcher.considerAll(),A.matcher.lastIndex=C;const e=A.matcher.exec(n);if(!e)break;const t=w(n.substring(C,e.index),e);C=e.index+t}w(n.substring(C))}return M.finalize(),S=M.toHTML(),{language:e,value:S,relevance:O,illegal:!1,_emitter:M,_top:A}}catch(t){if(t.message&&t.message.includes("Illegal"))return{language:e,value:ae(n),illegal:!0,relevance:0,_illegalBy:{message:t.message,index:C,context:n.slice(C-100,C+100),mode:t.mode,resultSoFar:S},_emitter:M};if(a)return{language:e,value:ae(n),illegal:!1,relevance:0,errorRaised:t,_emitter:M,_top:A};throw t}}function x(e,n){n=n||d.languages||Object.keys(i);const t=function(e){const n={value:ae(e),illegal:!1,relevance:0,_top:c,_emitter:new d.__emitter(d)};return n._emitter.addText(e),n}(e),s=n.filter(y).filter(S).map((n=>E(n,e,!1)));s.unshift(t);const o=s.sort(((e,n)=>{if(e.relevance!==n.relevance)return n.relevance-e.relevance;if(e.language&&n.language){if(y(e.language).supersetOf===n.language)return 1;if(y(n.language).supersetOf===e.language)return-1}return 0})),[a,r]=o,l=a;return l.secondBest=r,l}function _(e){let n=null;const t=function(e){let n=e.className+" ";n+=e.parentNode?e.parentNode.className:"";const t=d.languageDetectRe.exec(n);if(t){const n=y(t[1]);return n||(Y(r.replace("{}",t[1])),Y("Falling back to no-highlight mode for this block.",e)),n?t[1]:"no-highlight"}return n.split(/\s+/).find((e=>b(e)||y(e)))}(e);if(b(t))return;if(A("before:highlightElement",{el:e,language:t}),e.dataset.highlighted)return void console.log("Element previously highlighted. To highlight again, first unset `dataset.highlighted`.",e);if(e.children.length>0&&(d.ignoreUnescapedHTML||(console.warn("One of your code blocks includes unescaped HTML. This is a potentially serious security risk."),console.warn("https://github.com/highlightjs/highlight.js/wiki/security"),console.warn("The element with unescaped HTML:"),console.warn(e)),d.throwUnescapedHTML))throw new oe("One of your code blocks includes unescaped HTML.",e.innerHTML);n=e;const i=n.textContent,o=t?m(i,{language:t,ignoreIllegals:!0}):x(i);e.innerHTML=o.value,e.dataset.highlighted="yes",function(e,n,t){const i=n&&s[n]||t;e.classList.add("hljs"),e.classList.add(`language-${i}`)}(e,t,o.language),e.result={language:o.language,re:o.relevance,relevance:o.relevance},o.secondBest&&(e.secondBest={language:o.secondBest.language,relevance:o.secondBest.relevance}),A("after:highlightElement",{el:e,result:o,text:i})}let w=!1;function k(){"loading"!==document.readyState?document.querySelectorAll(d.cssSelector).forEach(_):w=!0}function y(e){return e=(e||"").toLowerCase(),i[e]||i[s[e]]}function v(e,{languageName:n}){"string"==typeof e&&(e=[e]),e.forEach((e=>{s[e.toLowerCase()]=n}))}function S(e){const n=y(e);return n&&!n.disableAutodetect}function A(e,n){const t=e;o.forEach((function(e){e[t]&&e[t](n)}))}"undefined"!=typeof window&&window.addEventListener&&window.addEventListener("DOMContentLoaded",(function(){w&&k()}),!1),Object.assign(e,{highlight:m,highlightAuto:x,highlightAll:k,highlightElement:_,highlightBlock:function(e){return Q("10.7.0","highlightBlock will be removed entirely in v12.0"),Q("10.7.0","Please use highlightElement now."),_(e)},configure:function(e){d=re(d,e)},initHighlighting:()=>{k(),Q("10.6.0","initHighlighting() deprecated.  Use highlightAll() now.")},initHighlightingOnLoad:function(){k(),Q("10.6.0","initHighlightingOnLoad() deprecated.  Use highlightAll() now.")},registerLanguage:function(n,t){let s=null;try{s=t(e)}catch(e){if(J("Language definition for '{}' could not be registered.".replace("{}",n)),!a)throw e;J(e),s=c}s.name||(s.name=n),i[n]=s,s.rawDefinition=t.bind(null,e),s.aliases&&v(s.aliases,{languageName:n})},unregisterLanguage:function(e){delete i[e];for(const n of Object.keys(s))s[n]===e&&delete s[n]},listLanguages:function(){return Object.keys(i)},getLanguage:y,registerAliases:v,autoDetection:S,inherit:re,addPlugin:function(e){!function(e){e["before:highlightBlock"]&&!e["before:highlightElement"]&&(e["before:highlightElement"]=n=>{e["before:highlightBlock"](Object.assign({block:n.el},n))}),e["after:highlightBlock"]&&!e["after:highlightElement"]&&(e["after:highlightElement"]=n=>{e["after:highlightBlock"](Object.assign({block:n.el},n))})}(e),o.push(e)},removePlugin:function(e){const n=o.indexOf(e);-1!==n&&o.splice(n,1)}}),e.debugMode=function(){a=!1},e.safeMode=function(){a=!0},e.versionString="11.9.0",e.regex={concat:f,lookahead:u,either:p,optional:h,anyNumberOfTimes:g};for(const e in H)"object"==typeof H[e]&&n(H[e]);return Object.assign(e,H),e},de=le({});de.newInstance=()=>le({}),e.exports=de,de.HighlightJS=de,de.default=de}},n={};function t(i){var s=n[i];if(void 0!==s)return s.exports;var o=n[i]={exports:{}};return e[i](o,o.exports,t),o.exports}(()=>{"use strict";Shiny;const e=t(390);e.registerLanguage("r",(function(e){const n=e.regex,t=/(?:(?:[a-zA-Z]|\.[._a-zA-Z])[._a-zA-Z0-9]*)|\.(?!\d)/,i=n.either(/0[xX][0-9a-fA-F]+\.[0-9a-fA-F]*[pP][+-]?\d+i?/,/0[xX][0-9a-fA-F]+(?:[pP][+-]?\d+)?[Li]?/,/(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?[Li]?/),s=/[=!<>:]=|\|\||&&|:::?|<-|<<-|->>|->|\|>|[-+*\/?!$&|:<=>@^~]|\*\*/,o=n.either(/[()]/,/[{}]/,/\[\[/,/[[\]]/,/\\/,/,/);return{name:"R",keywords:{$pattern:t,keyword:"function if in break next repeat else for while",literal:"NULL NA TRUE FALSE Inf NaN NA_integer_|10 NA_real_|10 NA_character_|10 NA_complex_|10",built_in:"LETTERS letters month.abb month.name pi T F abs acos acosh all any anyNA Arg as.call as.character as.complex as.double as.environment as.integer as.logical as.null.default as.numeric as.raw asin asinh atan atanh attr attributes baseenv browser c call ceiling class Conj cos cosh cospi cummax cummin cumprod cumsum digamma dim dimnames emptyenv exp expression floor forceAndCall gamma gc.time globalenv Im interactive invisible is.array is.atomic is.call is.character is.complex is.double is.environment is.expression is.finite is.function is.infinite is.integer is.language is.list is.logical is.matrix is.na is.name is.nan is.null is.numeric is.object is.pairlist is.raw is.recursive is.single is.symbol lazyLoadDBfetch length lgamma list log max min missing Mod names nargs nzchar oldClass on.exit pos.to.env proc.time prod quote range Re rep retracemem return round seq_along seq_len seq.int sign signif sin sinh sinpi sqrt standardGeneric substitute sum switch tan tanh tanpi tracemem trigamma trunc unclass untracemem UseMethod xtfrm"},contains:[e.COMMENT(/#'/,/$/,{contains:[{scope:"doctag",match:/@examples/,starts:{end:n.lookahead(n.either(/\n^#'\s*(?=@[a-zA-Z]+)/,/\n^(?!#')/)),endsParent:!0}},{scope:"doctag",begin:"@param",end:/$/,contains:[{scope:"variable",variants:[{match:t},{match:/`(?:\\.|[^`\\])+`/}],endsParent:!0}]},{scope:"doctag",match:/@[a-zA-Z]+/},{scope:"keyword",match:/\\[a-zA-Z]+/}]}),e.HASH_COMMENT_MODE,{scope:"string",contains:[e.BACKSLASH_ESCAPE],variants:[e.END_SAME_AS_BEGIN({begin:/[rR]"(-*)\(/,end:/\)(-*)"/}),e.END_SAME_AS_BEGIN({begin:/[rR]"(-*)\{/,end:/\}(-*)"/}),e.END_SAME_AS_BEGIN({begin:/[rR]"(-*)\[/,end:/\](-*)"/}),e.END_SAME_AS_BEGIN({begin:/[rR]'(-*)\(/,end:/\)(-*)'/}),e.END_SAME_AS_BEGIN({begin:/[rR]'(-*)\{/,end:/\}(-*)'/}),e.END_SAME_AS_BEGIN({begin:/[rR]'(-*)\[/,end:/\](-*)'/}),{begin:'"',end:'"',relevance:0},{begin:"'",end:"'",relevance:0}]},{relevance:0,variants:[{scope:{1:"operator",2:"number"},match:[s,i]},{scope:{1:"operator",2:"number"},match:[/%[^%]*%/,i]},{scope:{1:"punctuation",2:"number"},match:[o,i]},{scope:{2:"number"},match:[/[^a-zA-Z0-9._]|^/,i]}]},{scope:{3:"operator"},match:[t,/\s+/,/<-/,/\s+/]},{scope:"operator",relevance:0,variants:[{match:s},{match:/%[^%]*%/}]},{scope:"punctuation",relevance:0,match:o},{begin:"`",end:"`",contains:[{begin:/\\./}]}]}})),$((()=>{$(document).on("shiny:value",(n=>{n.name.match(/-code$/)&&($(`#${n.name}`).addClass("language-r"),setTimeout((()=>{delete document.getElementById(n.name).dataset.highlighted,e.highlightElement(document.getElementById(n.name))}),250))}))}));const n=e=>{$(e).find(".block-output-toggle").each(((e,n)=>{$(n).hasClass("block-bound")||($(n).addClass("block-bound"),$(n).on("click",(e=>{const n=$(e.target).closest(".block"),t=n.find(".block-output").is(":visible"),i=n.find(".block-input").is(":visible");t||i?(n.find(".block-inputs").addClass("d-none"),n.find(".block-output").addClass("d-none")):(n.find(".block-inputs").removeClass("d-none"),n.find(".block-output").removeClass("d-none"));let s="shown";n.find(".block-output").hasClass("d-none")&&(s="hidden"),n.find(".block-inputs").trigger(s),n.find(".block-output").trigger(s)})))}))},i=()=>{$(".block-copy-code").each(((e,n)=>{$(n).off("click"),$(n).on("click",(e=>{var n;n=$(e.target).closest("div").find("pre"),navigator.clipboard.writeText($(n).text())}))})),$(".stack-copy-code").each(((e,n)=>{$(n).off("click"),$(n).on("click",(e=>{const n=$(e.target).closest(".stack").find("pre");let t=[];n.each(((e,n)=>{t.push($(n).text())})),t=t.join(" |>\n"),navigator.clipboard.writeText(t)}))}))};Shiny.addCustomMessageHandler("blockr-bind-stack",(e=>{const t=`#${e.stack}`;setTimeout((()=>{(e=>{$(e).find(".stack-remove").each(((e,n)=>{"true"!=n.getAttribute("listener")&&$(n).on("click",(e=>{const n=$(e.target).closest(".stack"),t=n.closest(".masonry-item");n.remove(),0!==t.length&&t.remove()}))}))})(t),(e=>{(e=>{const n=$(e).find(".stack-edit-toggle");$(n).hasClass("block-bound")||($(n).addClass("block-bound"),$(n).on("click",(e=>{const n=$(e.target).closest(".stack").find(".block");$(e.currentTarget).toggleClass("etidable");const t=$(e.currentTarget).hasClass("etidable");n.each(((e,i)=>{const s=$(i);if(t){if(s.removeClass("d-none"),s.find(".block-title").removeClass("d-none"),e==n.length-1)return s.find(".block-output").addClass("show"),s.find(".block-output").removeClass("d-none"),s.find(".block-output").trigger("shown"),window.bootstrap.Collapse.getOrCreateInstance(s.find(".block-code")[0],{toggle:!1}).hide(),s.find(".block-inputs").addClass("d-none"),void s.find(".block-inputs").trigger("hidden");s.find(".block-loading").addClass("d-none")}else{if(s.find(".block-title").addClass("d-none"),e==n.length-1)return s.removeClass("d-none"),s.find(".block-output").addClass("show"),s.find(".block-output").removeClass("d-none"),s.find(".block-output").trigger("shown"),window.bootstrap.Collapse.getOrCreateInstance(s.find(".block-code")[0],{toggle:!1}).hide(),s.find(".block-inputs").addClass("d-none"),void s.find(".block-inputs").trigger("hidden");s.addClass("d-none")}}))})))})(e),(e=>{$(e).each(((e,n)=>{(e=>{const n=$(e).find(".block").last();n.removeClass("d-none");const t=n.find(".block-output");n.find(".block-title").addClass("d-none"),t.removeClass("d-none"),t.trigger("shown");const i=t.find(".datatables").first().attr("id");$(document).on("shiny:value",(e=>{e.name===i&&t.find(".block-loading").addClass("d-none")}))})(n)}))})(e),n(e)})(t),i()}),750)})),Shiny.addCustomMessageHandler("blockr-add-block",(e=>{const t=`#${e.stack}`;setTimeout((()=>{i(),n(t)}),500)})),Shiny.addCustomMessageHandler("validate-block",(e=>{e.state?$(`[data-value="${e.id}"] .card`).removeClass("border-danger"):$(`[data-value="${e.id}"] .card`).addClass("border-danger")})),Shiny.addCustomMessageHandler("validate-input",(e=>{(e=>{let n;n=$(`#${e.id}`).hasClass("shiny-input-select")?$(`#${e.id}-selectized`).parent(".selectize-input").closest("div"):`#${e.id}`,setTimeout((()=>{e.state?$(n).addClass("is-invalid"):$(n).addClass("is-valid")}),500)})(e)})),Shiny.addCustomMessageHandler("toggle-submit",(e=>{$(`#${e.id}`).prop("disabled",!e.state)}))})()})();