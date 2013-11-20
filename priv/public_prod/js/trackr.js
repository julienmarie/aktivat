(function() {
  var Trackr, addEvent,
    __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };

  Trackr = (function() {
    function Trackr(options) {
      this.options = options;
      this.onUnload = __bind(this.onUnload, this);
      this.ajaxpost = __bind(this.ajaxpost, this);
      this.post = __bind(this.post, this);
      this.trackClicks = __bind(this.trackClicks, this);
      this.trackPageEnd = __bind(this.trackPageEnd, this);
      this.trackPageStart = __bind(this.trackPageStart, this);
      if (!this.options.trackrClass) {
        this.options.trackrClass = ".trackit";
      }
      if (!this.options.serverUrl) {
        this.options.serverUrl = "http://www.trackit.io";
      }
      if (!this.options.sessionCookieName) {
        this.options.sessionCookieName = "trackit_session";
      }
      if (!this.options.v_visitorCookieName) {
        this.options.v_visitorCookieName = "trackit_v_visitor";
      }
      if (!this.options.visitorCookieName) {
        this.options.visitorCookieName = "trackit_visitor";
      }
      if (!this.options.loadstartvariable) {
        this.options.loadstartvariable = "trackrloadstart";
      }
      if (!this.options.tabIdPrefix) {
        this.options.tabIdPrefix = "trackit_tab_";
      }
      if (!this.options.trackTabs) {
        this.options.trackTabs = true;
      }
      this.setConfig();
      this.createClassSelector();
      this.main();
      this.trackPageDuration();
    }

    Trackr.prototype.trackPageStart = function() {
      this.privTrackPageDuration();
      return window[this.options.loadstartvariable] = new Date().getTime();
    };

    Trackr.prototype.trackPageEnd = function() {
      var data;
      data = {
        trackr_event_type: "trackr_pageloaded_event",
        trackr_page_loadtime: page_loadtime
      };
      return this.post(data);
    };

    Trackr.prototype.signupUser = function(id, data, signupDate) {
      if (typeof signupDate === void 0) {
        signupDate = new Date().getTime();
      }
      this.createCookie(this.options.visitorCookieName, id, 60000 * 60 * 24 * 365);
      data.trackr_event_type = "trackr_signupUser_event";
      data.signupDate = signupDate;
      this.post(data);
      return void 0;
    };

    Trackr.prototype.loginUser = function(id) {
      this.createCookie(this.options.visitorCookieName, id, 60000 * 60 * 24 * 365);
      data.trackr_event_type = "trackr_loginUser_event";
      this.post(data);
      return void 0;
    };

    Trackr.prototype.loginAndImportUser = function(id, data, signupDate) {
      this.createCookie(this.options.visitorCookieName, id, 60000 * 60 * 24 * 365);
      data.trackr_event_type = "trackr_loginUser_event";
      data.signupDate = signupDate;
      this.post(data);
      return void 0;
    };

    Trackr.prototype.updateUser = function(data) {
      data.trackr_event_type = "trackr_updateUser_event";
      data.signupDate = signupDate;
      this.post(data);
      return void 0;
    };

    Trackr.prototype.logoutUser = function() {
      this.eraseCookie(this.options.visitorCookieName);
      data.trackr_event_type = "trackr_logoutUser_event";
      this.post(data);
      return void 0;
    };

    Trackr.prototype.main = function() {
      var _this = this;
      if (document.readyState === "interactive") {
        return this.startupItems();
      } else {
        return document.onreadystatechange = function() {
          if (document.readyState === "interactive") {
            return _this.startupItems();
          }
        };
      }
    };

    Trackr.prototype.startupItems = function() {
      this.trackPageLoadComplete();
      return this.trackClicks();
    };

    Trackr.prototype.trackPageLoadComplete = function() {
      var data, page_loadtime;
      if (window.performance) {
        page_loadtime = window.performance.timing.domInteractive - window.performance.timing.responseEnd;
      } else {
        page_loadtime = new Date().getTime() - window[this.options.loadstartvariable];
      }
      data = {
        trackr_event_type: "trackr_pageloaded_event",
        trackr_page_loadtime: page_loadtime
      };
      return this.post(data);
    };

    Trackr.prototype.trackClicks = function() {
      var eventsTargets,
        _this = this;
      eventsTargets = this.select(this.options.trackrClass);
      addEvent(eventsTargets, 'click', function(e) {
        var data, target;
        target = e.target || e.srcElement;
        if (target && target.getAttribute("data-trackr-label") && target.getAttribute("data-trackr-position") && target.getAttribute("data-trackr-type")) {
          data = {
            trackr_event_type: "trackr_element_event",
            trackr_element_event: "click",
            trackr_element_label: target.getAttribute("data-trackr-label"),
            trackr_element_position: target.getAttribute("data-trackr-position"),
            trackr_element_type: target.getAttribute("data-trackr-type")
          };
          return _this.post(data);
        }
      });
      addEvent(eventsTargets, 'mouseover', function(e) {
        var data, target;
        target = e.target || e.srcElement;
        if (target && target.getAttribute("data-trackr-label") && target.getAttribute("data-trackr-position") && target.getAttribute("data-trackr-type")) {
          data = {
            trackr_event_type: "trackr_element_event",
            trackr_element_event: "mouseover",
            trackr_element_label: target.getAttribute("data-trackr-label"),
            trackr_element_position: target.getAttribute("data-trackr-position"),
            trackr_element_type: target.getAttribute("data-trackr-type")
          };
          return _this.post(data);
        }
      });
      addEvent(eventsTargets, 'focus', function(e) {
        var data, target;
        target = e.target || e.srcElement;
        if (target && target.getAttribute("data-trackr-label") && target.getAttribute("data-trackr-position") && target.getAttribute("data-trackr-type")) {
          data = {
            trackr_event_type: "trackr_element_event",
            trackr_element_event: "focus",
            trackr_element_label: target.getAttribute("data-trackr-label"),
            trackr_element_position: target.getAttribute("data-trackr-position"),
            trackr_element_type: target.getAttribute("data-trackr-type")
          };
          return _this.post(data);
        }
      });
      addEvent(eventsTargets, 'blur', function(e) {
        var data, target;
        target = e.target || e.srcElement;
        if (target && target.getAttribute("data-trackr-label") && target.getAttribute("data-trackr-position") && target.getAttribute("data-trackr-type")) {
          data = {
            trackr_event_type: "trackr_element_event",
            trackr_element_event: "blur",
            trackr_element_label: target.getAttribute("data-trackr-label"),
            trackr_element_position: target.getAttribute("data-trackr-position"),
            trackr_element_type: target.getAttribute("data-trackr-type"),
            trackr_element_value: target.value
          };
          return _this.post(data);
        }
      });
      return addEvent(eventsTargets, 'keyup', function(e) {
        var data, target;
        target = e.target || e.srcElement;
        if (target && target.getAttribute("data-trackr-label") && target.getAttribute("data-trackr-position") && target.getAttribute("data-trackr-type")) {
          data = {
            trackr_event_type: "trackr_element_event",
            trackr_element_event: "keyup",
            trackr_element_label: target.getAttribute("data-trackr-label"),
            trackr_element_position: target.getAttribute("data-trackr-position"),
            trackr_element_type: target.getAttribute("data-trackr-type"),
            trackr_element_char: e.keyCode
          };
          return _this.post(data);
        }
      });
    };

    Trackr.prototype.trackPageDuration = function() {
      var _this = this;
      return this.onUnload(function() {
        return _this.privTrackPageDuration();
      });
    };

    Trackr.prototype.privTrackPageDuration = function() {
      var data;
      data = {
        trackr_event_type: "trackr_visitDuration_event",
        trackr_visit_time: new Date().getTime() - window[this.options.loadstartvariable]
      };
      this.ajaxpost(data);
      return void 0;
    };

    Trackr.prototype.setConfig = function() {
      return this.config = {
        config: {
          account: this.options.account,
          screen: {
            width: screen.width,
            height: screen.height
          },
          cookies: {
            session: this.getCookie(this.options.sessionCookieName, 1200000),
            v_visitor: this.getCookie(this.options.v_visitorCookieName, 31536000000),
            visitor: this.readCookie(this.options.visitorCookieName) ? this.readCookie(this.options.visitorCookieName) : void 0
          },
          tab: this.options.trackTabs === true ? this.getTab() : void 0
        }
      };
    };

    Trackr.prototype.generateUuuid = function() {
      return "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g, function(c) {
        var r, v;
        r = Math.random() * 16 | 0;
        v = (c === "x" ? r : r & 0x3 | 0x8);
        return v.toString(16);
      });
    };

    Trackr.prototype.post = function(data) {
      var eventid, img;
      img = new Image();
      eventid = this.generateUuuid();
      return img.src = "" + this.options.serverUrl + "/track/it?" + (this.serialize(data)) + "&" + (this.serialize(this.config)) + "&" + (this.serialize(this.getUrl())) + "&event_id=" + eventid;
    };

    Trackr.prototype.ajaxpost = function(data) {
      var async, eventid, finalUrl, req;
      eventid = this.generateUuuid();
      async = false;
      finalUrl = "" + this.options.serverUrl + "/track/it?" + (this.serialize(data)) + "&" + (this.serialize(this.config)) + "&" + (this.serialize(this.getUrl())) + "&event_id=" + eventid;
      req = new XMLHttpRequest();
      req.open("GET", finalUrl, async);
      req.send();
      return req;
    };

    Trackr.prototype.getUrl = function() {
      return {
        hostname: window.location.hostname,
        path: window.location.pathname,
        hash: window.location.hash,
        args: this.getArgs()
      };
    };

    Trackr.prototype.getArgs = function() {
      var arr, i, pair, query, query_string, vars;
      query_string = {};
      query = window.location.search.substring(1);
      vars = query.split("&");
      i = 0;
      while (i < vars.length) {
        pair = vars[i].split("=");
        if (typeof query_string[pair[0]] === "undefined") {
          query_string[pair[0]] = pair[1];
        } else if (typeof query_string[pair[0]] === "string") {
          arr = [query_string[pair[0]], pair[1]];
          query_string[pair[0]] = arr;
        } else {
          query_string[pair[0]].push(pair[1]);
        }
        i++;
      }
      return query_string;
    };

    Trackr.prototype.serialize = function(obj, prefix) {
      var k, p, str, v;
      str = [];
      for (p in obj) {
        k = (prefix ? prefix + "_" + p + "" : p);
        v = obj[p];
        str.push((typeof v === "object" ? this.serialize(v, k) : encodeURIComponent(k) + "=" + encodeURIComponent(v)));
      }
      return str.join("&");
    };

    Trackr.prototype.getCookie = function(cookieName, time) {
      var session;
      if (this.readCookie(cookieName) !== null) {
        session = this.readCookie(cookieName);
        this.createCookie(cookieName, session, time);
        return session;
      } else {
        session = this.generateUuuid();
        this.createCookie(cookieName, session, time);
        return session;
      }
    };

    Trackr.prototype.createCookie = function(name, value, time) {
      var date, expires;
      date = new Date();
      date.setTime(date.getTime() + time);
      expires = "; expires=" + date.toGMTString();
      return document.cookie = name + "=" + value + expires + "; path=/";
    };

    Trackr.prototype.readCookie = function(name) {
      var c, ca, i, nameEQ;
      nameEQ = name + "=";
      ca = document.cookie.split(";");
      i = 0;
      while (i < ca.length) {
        c = ca[i];
        while (c.charAt(0) === " ") {
          c = c.substring(1, c.length);
        }
        if (c.indexOf(nameEQ) === 0) {
          return c.substring(nameEQ.length, c.length);
        }
        i++;
      }
      return null;
    };

    Trackr.prototype.eraseCookie = function(name) {
      return createCookie(name, "", -1);
    };

    Trackr.prototype.onUnload = function(func) {
      return window.onunload = func;
    };

    Trackr.prototype.createClassSelector = function() {
      if (!document.getElementsByClassName) {
        return document.getElementsByClassName = function(cl, tag) {
          var els, i, len, matches, regex;
          els = void 0;
          matches = [];
          i = 0;
          len = void 0;
          regex = new RegExp("(?:\\s|^)" + cl + "(?:\\s|$)");
          els = document.getElementsByTagName(tag || "*");
          if (!els[0]) {
            return false;
          }
          len = els.length;
          while (i < len) {
            if (els[i].className.match(regex)) {
              matches.push(els[i]);
            }
            i++;
          }
          return matches;
        };
      }
    };

    Trackr.prototype.select = function(el, tag) {
      var firstChar;
      firstChar = el.charAt(0);
      if (document.querySelectorAll) {
        return document.querySelectorAll(el);
      }
      switch (firstChar) {
        case "#":
          return document.getElementById(el.slice(1));
        case ".":
          return document.getElementsByClassName(el.slice(1), tag);
        default:
          return document.getElementsByTagName(el);
      }
    };

    Trackr.prototype.cancelEvent = function(event) {
      if (event.preventDefault) {
        return event.preventDefault();
      } else {
        return event.returnValue = false;
      }
    };

    Trackr.prototype.getTab = function() {
      if (this.options.trackTabs === true) {
        if (window.name.substring(0, this.options.tabIdPrefix.length) === this.options.tabIdPrefix) {
          return window.name.replace(this.options.tabIdPrefix, "");
        } else {
          window.name = "" + this.options.tabIdPrefix + (this.generateUuuid());
          return window.name.replace(this.options.tabIdPrefix, "");
        }
      }
    };

    return Trackr;

  })();

  addEvent = (function() {
    var filter;
    filter = function(el, type, fn) {
      var i, len, _results;
      i = 0;
      len = el.length;
      _results = [];
      while (i < len) {
        addEvent(el[i], type, fn);
        _results.push(i++);
      }
      return _results;
    };
    if (document.addEventListener) {
      return function(el, type, fn) {
        if (el && el.nodeName || el === window) {
          return el.addEventListener(type, fn, false);
        } else {
          if (el && el.length) {
            return filter(el, type, fn);
          }
        }
      };
    }
    return function(el, type, fn) {
      if (el && el.nodeName || el === window) {
        return el.attachEvent("on" + type, function() {
          return fn.call(el, window.event);
        });
      } else {
        if (el && el.length) {
          return filter(el, type, fn);
        }
      }
    };
  })();

  window.Trackr = Trackr;

}).call(this);
