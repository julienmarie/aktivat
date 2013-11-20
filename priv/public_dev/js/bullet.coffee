class Bullet

  constructor: (@url, @options) ->
    @CONNECTING = 0
    @OPEN = 1
    @CLOSING = 2
    @CLOSED = 3
    if (typeof window.XMLHttpRequest == "undefined")
      console.log 'XMLHttpRequest is undefined'
      window.XMLHttpRequest = ->
        try
          return new ActiveXObject("Msxml2.XMLHTTP.6.0")
        catch error
        try
          return new ActiveXObject("Msxml2.XMLHTTP.3.0")
        catch error
        try
          return new ActiveXObject("Microsoft.XMLHTTP")
        catch error
        throw new Error("This browser does not support XMLHttpRequest.")
    @isClosed = true
    @readyState = @CLOSED
    @heartbeat = undefined
    @delay = 80
    @delayDefault = 80
    @delayMax = 10000
    @transport = undefined
    @stream().init()

  next: =>
    c = 0
    for f of @transports
      if tn is c
        t = @transports[f]()
        if t
          ret = new t.transport(url)
          ret.heart = t.heart
          return ret
        tn++
      c++
    false

  transports: =>
    websocket: =>
      transport = null
      return false  if options isnt 'undefined' and options.disableWebSocket
      transport = window.WebSocket  if window.WebSocket
      transport = window.MozWebSocket  if window.MozWebSocket and navigator.userAgent.indexOf("Firefox/6.0") is -1
      if transport
        return (
          heart: true
          transport: transport
        )
      null

    eventsource: =>
      return false  if options isnt 'undefined' and options.disableEventSource
      return false  unless window.EventSource
      eventsourceURL = url.replace("ws:", "http:").replace("wss:", "https:")
      source = new window.EventSource(eventsourceURL)
      source.onopen ()->
        fake.readyState = @OPEN
        fake.onopen()

      source.onmessage (event) =>
        fake.onmessage event

      source.onerror() =>
        source.close()
        source = `undefined`
        fake.onerror()

      fake: =>
        readyState: @CONNECTING
        send: xhrSend
        close: =>
          fake.readyState = @CLOSED
          source.close()
          source = `undefined`
          fake.onclose()

      heart: false
      transport: =>
        fake

    xhrPolling: =>
      poll: =>
        fakeurl = url.replace("ws:", "http:").replace("wss:", "https:")
        xhr = $.ajax(
          type: "GET"
          cache: false
          url: fakeurl
          dataType: "text"
          data: {}
          headers:
            "X-Socket-Transport": "xhrPolling"

          success: (data) =>
            xhr = null
            if fake.readyState is @CONNECTING
              fake.readyState = @OPEN
              fake.onopen fake
            fake.onmessage data: data  if data and data.length isnt 0
            nextPoll()  if fake.readyState is @OPEN

          error: (xhr) =>
            xhr = null
            fake.onerror()
        )
      nextPoll = ->
        timeout = setTimeout(->
          poll()
        , 100)
      return false  if options isnt 'undefined' and options.disableXHRPolling
      timeout = undefined
      xhr = null
      fake =
        readyState: @CONNECTING
        send: xhrSend
        close: ->
          @readyState = @CLOSED
          if xhr
            xhr.abort()
            xhr = null
          clearTimeout timeout
          fake.onclose()

        onopen: ->

        onmessage: ->

        onerror: ->

        onclose: ->

      nextPoll()
      heart: false
      transport: ->
        fake

  xhrSend: (data) =>
    return false  if @readyState isnt @CONNECTING and @readyState isnt @OPEN
    sendUrl = url.replace("ws:", "http:").replace("wss:", "https:")
    self = this
    $.ajax
      async: false
      cache: false
      type: "POST"
      url: sendUrl
      data: data
      dataType: "text"
      contentType: "application/x-www-form-urlencoded; charset=utf-8"
      headers:
        "X-Socket-Transport": "xhrPolling"

      success: (data) ->
        self.onmessage data: data  if data and data.length isnt 0

    true

  tn = 0

  stream: () => { 
    init: =>
      @isClosed = false
      @readyState = @CONNECTING
      @transport = @next()
      unless @transport
        
        # Hard disconnect, inform the user and retry later
        @delay = @delayDefault
        tn = 0
        @stream().ondisconnect()
        setTimeout (=>
          @stream().init()
        ), @delayMax
        return false
      @transport.onopen() =>
        
        # We got a connection, reset the poll delay
        delay = delayDefault
        if @transport.heart
          @heartbeat = setInterval(=>
            @stream().onheartbeat()
          , 20000)
        unless @readyState is @OPEN
          @readyState = @OPEN
          @stream().onopen()

      @transport.onclose() =>
        
        # Firefox 13.0.1 sends 2 close events.
        # Return directly if we already handled it
        # or we are closed
        return  if @isClosed or @readyState is @CLOSED
        @transport = null
        clearInterval @heartbeat
        if @readyState is @CLOSING
          @readyState = @CLOSED
          @transport = false
          @stream.onclose()
        else
          
          # Close happened on connect, select next transport
          tn++  if @readyState is @CONNECTING
          @delay *= 2
          @delay = @delayMax  if @delay > @delayMax
          @isClosed = true
          setTimeout (=>
            @stream().init()
          ), @delay

      @transport.onerror() =>
        @transport.onclose
      @transport.onmessage (e) =>
        @stream.onmessage e

    

    onopen:() =>

    onmessage:() =>

    ondisconnect:() =>

    onclose: () =>

    onheartbeat: () =>

    setURL: (newURL) =>
      @url = newURL

    send: (data) =>
      if @transport
        @transport.send data
      else
        false

    close: =>
      @readyState = @CLOSING
      @transport.close()  if @transport

  }

  
window.Bullet = Bullet