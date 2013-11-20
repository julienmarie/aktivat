(function(){var a,b=function(a,b){return function(){return a.apply(b,arguments)}};a=function(){function a(a,c){this.url=a,this.options=c,this.stream=b(this.stream,this),this.xhrSend=b(this.xhrSend,this),this.transports=b(this.transports,this),this.next=b(this.next,this),this.CONNECTING=0,this.OPEN=1,this.CLOSING=2,this.CLOSED=3,"undefined"==typeof window.XMLHttpRequest&&(console.log("XMLHttpRequest is undefined"),window.XMLHttpRequest=function(){var a;try{return new ActiveXObject("Msxml2.XMLHTTP.6.0")}catch(b){a=b}try{return new ActiveXObject("Msxml2.XMLHTTP.3.0")}catch(b){a=b}try{return new ActiveXObject("Microsoft.XMLHTTP")}catch(b){a=b}throw new Error("This browser does not support XMLHttpRequest.")}),this.isClosed=!0,this.readyState=this.CLOSED,this.heartbeat=void 0,this.delay=80,this.delayDefault=80,this.delayMax=1e4,this.transport=void 0,this.stream().init()}var c;return a.prototype.next=function(){var a,b,d,e;a=0;for(b in this.transports){if(c===a){if(e=this.transports[b]())return d=new e.transport(url),d.heart=e.heart,d;c++}a++}return!1},a.prototype.transports=function(){var a=this;return{websocket:function(){var a;return a=null,"undefined"!==options&&options.disableWebSocket?!1:(window.WebSocket&&(a=window.WebSocket),window.MozWebSocket&&-1===navigator.userAgent.indexOf("Firefox/6.0")&&(a=window.MozWebSocket),a?{heart:!0,transport:a}:null)},eventsource:function(){var b,c;return"undefined"!==options&&options.disableEventSource?!1:window.EventSource?(b=url.replace("ws:","http:").replace("wss:","https:"),c=new window.EventSource(b),c.onopen(function(){return fake.readyState=this.OPEN,fake.onopen()}),c.onmessage(function(a){return fake.onmessage(a)}),c.onerror()(function(){return c.close(),c=void 0,fake.onerror()}),{fake:function(){return{readyState:a.CONNECTING,send:xhrSend,close:function(){return fake.readyState=a.CLOSED,c.close(),c=void 0,fake.onclose()}}},heart:!1,transport:function(){return fake}}):!1},xhrPolling:function(){var b,c,d,e;return c=function(){var a;return a=setTimeout(function(){return poll()},100)},"undefined"!==options&&options.disableXHRPolling?!1:(d=void 0,e=null,b={readyState:a.CONNECTING,send:xhrSend,close:function(){return this.readyState=this.CLOSED,e&&(e.abort(),e=null),clearTimeout(d),b.onclose()},onopen:function(){},onmessage:function(){},onerror:function(){},onclose:function(){}},c(),{heart:!1,transport:function(){return b}})}}},a.prototype.xhrSend=function(a){var b,c;return this.readyState!==this.CONNECTING&&this.readyState!==this.OPEN?!1:(c=url.replace("ws:","http:").replace("wss:","https:"),b=this,$.ajax({async:!1,cache:!1,type:"POST",url:c,data:a,dataType:"text",contentType:"application/x-www-form-urlencoded; charset=utf-8",headers:{"X-Socket-Transport":"xhrPolling"},success:function(a){return a&&0!==a.length?b.onmessage({data:a}):void 0}}),!0)},c=0,a.prototype.stream=function(){var a=this;return{init:function(){return a.isClosed=!1,a.readyState=a.CONNECTING,a.transport=a.next(),a.transport?(a.transport.onopen()(function(){var b;return b=delayDefault,a.transport.heart&&(a.heartbeat=setInterval(function(){return a.stream().onheartbeat()},2e4)),a.readyState!==a.OPEN?(a.readyState=a.OPEN,a.stream().onopen()):void 0}),a.transport.onclose()(function(){return a.isClosed||a.readyState===a.CLOSED?void 0:(a.transport=null,clearInterval(a.heartbeat),a.readyState===a.CLOSING?(a.readyState=a.CLOSED,a.transport=!1,a.stream.onclose()):(a.readyState===a.CONNECTING&&c++,a.delay*=2,a.delay>a.delayMax&&(a.delay=a.delayMax),a.isClosed=!0,setTimeout(function(){return a.stream().init()},a.delay)))}),a.transport.onerror()(function(){return a.transport.onclose}),a.transport.onmessage(function(b){return a.stream.onmessage(b)})):(a.delay=a.delayDefault,c=0,a.stream().ondisconnect(),setTimeout(function(){return a.stream().init()},a.delayMax),!1)},onopen:function(){},onmessage:function(){},ondisconnect:function(){},onclose:function(){},onheartbeat:function(){},setURL:function(b){return a.url=b},send:function(b){return a.transport?a.transport.send(b):!1},close:function(){return a.readyState=a.CLOSING,a.transport?a.transport.close():void 0}}},a}(),window.Bullet=a}).call(this);