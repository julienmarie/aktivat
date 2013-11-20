class Trackr
  constructor: (@options) ->
    @options.trackrClass = ".trackit" unless @options.trackrClass
    @options.serverUrl = "http://www.trackit.io" unless @options.serverUrl
    @options.sessionCookieName = "trackit_session" unless @options.sessionCookieName
    @options.v_visitorCookieName = "trackit_v_visitor" unless @options.v_visitorCookieName
    @options.visitorCookieName = "trackit_visitor" unless @options.visitorCookieName
    @options.loadstartvariable = "trackrloadstart" unless @options.loadstartvariable
    @options.tabIdPrefix = "trackit_tab_" unless @options.tabIdPrefix
    @options.trackTabs = true unless @options.trackTabs
    @setConfig()
    @createClassSelector()
    @main()
    @trackPageDuration()


  # ----------------------------------------------------------------------------------------------------
  # API FUNCTIONS
  # ----------------------------------------------------------------------------------------------------

  trackPageStart: =>
    @privTrackPageDuration()
    window[@options.loadstartvariable] = new Date().getTime()

  trackPageEnd: =>  
    data =
      trackr_event_type : "trackr_pageloaded_event"
      trackr_page_loadtime : page_loadtime
    @post data

  signupUser: (id, data, signupDate) ->
    if typeof(signupDate) == undefined
      signupDate = new Date().getTime()
    @createCookie(@options.visitorCookieName, id, 60000*60*24*365)
    data.trackr_event_type = "trackr_signupUser_event"
    data.signupDate = signupDate
    @post data
    undefined

  loginUser: (id) ->
    @createCookie(@options.visitorCookieName, id, 60000*60*24*365)
    data.trackr_event_type = "trackr_loginUser_event"
    @post data
    undefined

  loginAndImportUser: (id, data, signupDate) ->
    @createCookie(@options.visitorCookieName, id, 60000*60*24*365)
    data.trackr_event_type = "trackr_loginUser_event"
    data.signupDate = signupDate
    @post data
    undefined

  updateUser: (data) ->
    data.trackr_event_type = "trackr_updateUser_event"
    data.signupDate = signupDate
    @post data
    undefined

  logoutUser: ->
    @eraseCookie @options.visitorCookieName
    data.trackr_event_type = "trackr_logoutUser_event"
    @post data
    undefined


  # ----------------------------------------------------------------------------------------------------
  # STARTUP FUNCTIONS
  # ----------------------------------------------------------------------------------------------------


  # BOOT

  main: ->
    if document.readyState is "interactive"
      @startupItems()
    else
      document.onreadystatechange = () =>
        if document.readyState is "interactive"
          @startupItems()


  # BOOT SEQUENCE
    
  startupItems: ->
    @trackPageLoadComplete()
    @trackClicks()


  # PAGE LOAD

  trackPageLoadComplete: ->
    if window.performance
      page_loadtime = window.performance.timing.domInteractive - window.performance.timing.responseEnd
    else
      page_loadtime = new Date().getTime() - window[@options.loadstartvariable]
    data =
      trackr_event_type : "trackr_pageloaded_event"
      trackr_page_loadtime : page_loadtime
    @post data


  # TRACK DOM EVENTS

  trackClicks:() =>    
    eventsTargets = @select(@options.trackrClass)
    addEvent eventsTargets, 'click', (e)=>
      target = e.target or e.srcElement
      if target and target.getAttribute("data-trackr-label") and target.getAttribute("data-trackr-position") and target.getAttribute("data-trackr-type")
        data =
          trackr_event_type : "trackr_element_event"
          trackr_element_event : "click"
          trackr_element_label : target.getAttribute("data-trackr-label")
          trackr_element_position : target.getAttribute("data-trackr-position")
          trackr_element_type : target.getAttribute("data-trackr-type")
        @post data
    addEvent eventsTargets, 'mouseover', (e)=>
      target = e.target or e.srcElement
      if target and target.getAttribute("data-trackr-label") and target.getAttribute("data-trackr-position") and target.getAttribute("data-trackr-type")
        data =
          trackr_event_type : "trackr_element_event"
          trackr_element_event : "mouseover"
          trackr_element_label : target.getAttribute("data-trackr-label")
          trackr_element_position : target.getAttribute("data-trackr-position")
          trackr_element_type : target.getAttribute("data-trackr-type")
        @post data
    addEvent eventsTargets, 'focus', (e)=>
      target = e.target or e.srcElement
      if target and target.getAttribute("data-trackr-label") and target.getAttribute("data-trackr-position") and target.getAttribute("data-trackr-type")
        data =
          trackr_event_type : "trackr_element_event"
          trackr_element_event : "focus"
          trackr_element_label : target.getAttribute("data-trackr-label")
          trackr_element_position : target.getAttribute("data-trackr-position")
          trackr_element_type : target.getAttribute("data-trackr-type")
        @post data
    addEvent eventsTargets, 'blur', (e)=>
      target = e.target or e.srcElement
      if target and target.getAttribute("data-trackr-label") and target.getAttribute("data-trackr-position") and target.getAttribute("data-trackr-type")
        data =
          trackr_event_type : "trackr_element_event"
          trackr_element_event : "blur"
          trackr_element_label : target.getAttribute("data-trackr-label")
          trackr_element_position : target.getAttribute("data-trackr-position")
          trackr_element_type : target.getAttribute("data-trackr-type")
          trackr_element_value : target.value
        @post data
    addEvent eventsTargets, 'keyup', (e)=>
      target = e.target or e.srcElement
      if target and target.getAttribute("data-trackr-label") and target.getAttribute("data-trackr-position") and target.getAttribute("data-trackr-type")
        data =
          trackr_event_type : "trackr_element_event"
          trackr_element_event : "keyup"
          trackr_element_label : target.getAttribute("data-trackr-label")
          trackr_element_position : target.getAttribute("data-trackr-position")
          trackr_element_type : target.getAttribute("data-trackr-type")
          trackr_element_char : e.keyCode
        @post data

  # TRACK TIME ON PAGE AND PAGE STATISTICS

  trackPageDuration: ->
    @onUnload => 
      @privTrackPageDuration()

  privTrackPageDuration: ->
    data =
      trackr_event_type : "trackr_visitDuration_event"
      trackr_visit_time : new Date().getTime() - window[@options.loadstartvariable]
    @ajaxpost data
    undefined

  # ----------------------------------------------------------------------------------------------------
  # DATA FUNCTIONS
  # ----------------------------------------------------------------------------------------------------

  setConfig: ->
    @config = 
      config :
        account : @options.account
        screen :
          width : screen.width
          height : screen.height
        cookies : 
          session : @getCookie(@options.sessionCookieName, 1200000)
          v_visitor : @getCookie(@options.v_visitorCookieName, 31536000000)
          visitor : @readCookie(@options.visitorCookieName) if @readCookie(@options.visitorCookieName)
        tab : @getTab() if @options.trackTabs is true

  generateUuuid: ->
    "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace /[xy]/g, (c) ->
      r = Math.random() * 16 | 0
      v = (if c is "x" then r else (r & 0x3 | 0x8))
      v.toString 16

  # ----------------------------------------------------------------------------------------------------
  # POST FUNCTIONS
  # ----------------------------------------------------------------------------------------------------

  post: (data) =>
    img = new Image()
    eventid = @generateUuuid()
    img.src = "#{@options.serverUrl}/track/it?#{@serialize(data)}&#{@serialize(@config)}&#{@serialize @getUrl()}&event_id=#{eventid}"
    #t = document.getElementsByTagName('head')[0]
    #t.appendChild(img)

  ajaxpost: (data) =>
    eventid = @generateUuuid()
    async = false
    finalUrl = "#{@options.serverUrl}/track/it?#{@serialize(data)}&#{@serialize(@config)}&#{@serialize @getUrl()}&event_id=#{eventid}"
    req = new XMLHttpRequest()
    req.open "GET", finalUrl, async
    req.send()
    req

  getUrl: ->
    {
      hostname : window.location.hostname
      path : window.location.pathname
      hash : window.location.hash
      args : @getArgs()
    }

  getArgs: ->
    query_string = {}
    query = window.location.search.substring(1)
    vars = query.split("&")
    i = 0
  
    while i < vars.length
      pair = vars[i].split("=")
      
      # If first entry with this name
      if typeof query_string[pair[0]] is "undefined"
        query_string[pair[0]] = pair[1]
      
      # If second entry with this name
      else if typeof query_string[pair[0]] is "string"
        arr = [query_string[pair[0]], pair[1]]
        query_string[pair[0]] = arr
      
      # If third or later entry with this name
      else
        query_string[pair[0]].push pair[1]
      i++
    query_string

  serialize : (obj, prefix) ->
    str = []
    for p of obj
      k = (if prefix then prefix + "_" + p + "" else p)
      v = obj[p]
      str.push (if typeof v is "object" then @serialize(v, k) else encodeURIComponent(k) + "=" + encodeURIComponent(v))
    str.join "&"


  # ----------------------------------------------------------------------------------------------------
  # COOKIES FUNCTIONS
  # ----------------------------------------------------------------------------------------------------

  getCookie: (cookieName, time) ->
    if @readCookie(cookieName) != null
      session = @readCookie(cookieName)
      @createCookie cookieName, session, time
      session
    else
      session = @generateUuuid()
      @createCookie cookieName, session, time
      session

  createCookie : (name, value, time) ->
    date = new Date()
    date.setTime date.getTime() + (time)
    expires = "; expires=" + date.toGMTString()
    document.cookie = name + "=" + value + expires + "; path=/"

  readCookie : (name) ->
    nameEQ = name + "="
    ca = document.cookie.split(";")
    i = 0
  
    while i < ca.length
      c = ca[i]
      c = c.substring(1, c.length)  while c.charAt(0) is " "
      return c.substring(nameEQ.length, c.length)  if c.indexOf(nameEQ) is 0
      i++
    null

  eraseCookie: (name) ->
    createCookie name, "", -1


  # ----------------------------------------------------------------------------------------------------
  # DOM FUNCTIONS
  # ----------------------------------------------------------------------------------------------------

  onUnload: (func) =>
    window.onunload = func

  createClassSelector: ->
    unless document.getElementsByClassName
      document.getElementsByClassName = (cl, tag) ->
        els = undefined
        matches = []
        i = 0
        len = undefined
        regex = new RegExp("(?:\\s|^)" + cl + "(?:\\s|$)")
        
        # If no tag name is specified,
        # we have to grab EVERY element from the DOM    
        els = document.getElementsByTagName(tag or "*")
        return false  unless els[0]
        len = els.length
        while i < len
          matches.push els[i]  if els[i].className.match(regex)
          i++
        matches

  select: (el, tag) ->
    firstChar = el.charAt(0)
    return document.querySelectorAll(el)  if document.querySelectorAll
    switch firstChar
      when "#"
        document.getElementById el.slice(1)
      when "."
        document.getElementsByClassName el.slice(1), tag
      else
        document.getElementsByTagName el

  cancelEvent: (event) ->
    if event.preventDefault
      event.preventDefault()
    else
      event.returnValue = false

  getTab: () ->
    if @options.trackTabs is true
      if window.name.substring(0, @options.tabIdPrefix.length) == @options.tabIdPrefix
        return window.name.replace(@options.tabIdPrefix, "")
      else
        window.name = "#{@options.tabIdPrefix}#{@generateUuuid()}"
        return window.name.replace(@options.tabIdPrefix, "")




addEvent = (->
  filter = (el, type, fn) ->
    i = 0
    len = el.length

    while i < len
      addEvent el[i], type, fn
      i++

  if document.addEventListener
    return (el, type, fn) ->
      if el and el.nodeName or el is window
        el.addEventListener type, fn, false
      else filter el, type, fn  if el and el.length
  (el, type, fn) ->
    if el and el.nodeName or el is window
      el.attachEvent "on" + type, ->
        fn.call el, window.event

    else filter el, type, fn  if el and el.length
)()

window.Trackr = Trackr
