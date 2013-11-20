Aktivat.io

The MIT License (MIT)

Copyright (c) 2013 Julien Marie / Producture

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


# Aktivat


## Features

##Concepts and vocabulary

### Accounts
Accounts are customer accounts. Accounts can have multiple users with complete ACL per module.

### Property
A property is an area of analysis. It can be a website or a mobile app or anything else.


## JS Tracker Setup

The core of aktivat is tracking activity from multiple sources. The most common source is websites, so the first and standard tracking plugin is a browser JS tracker.

`TODO` This tracker also provides advanced tools like websocket and longpolling based customer service chat. This will be detailed and specified later.

### Basic Setup

This straightforward setup allows you to track navigation mouse and keyboard events, page loading time.

At the top of the head section of your pages


	<script>
		window.trackrloadstart = new Date().getTime();
	</script>

At the bottom of the body of your pages

	<script>
		var o = {
  			account:YOUR_ACCOUNT_ID,
  			trackrClass:DOM_CLASSES_YOU_WANT_TO_TRACK_MOUSE_AND_KEYBOARD_EVENTS_FOR,
  			serverUrl:"http://trackit.io"
		};
		var r,s,t;r=false;s=document.createElement("script");s.type="text/javascript";s.src=o.serverUrl+"/js/trackr.js";s.onload=s.onreadystatechange=function(){if(!r&&(!this.readyState||this.readyState==="complete")){r=true;return window.trackr=new window.Trackr(o)}};t=document.getElementsByTagName("head")[0];t.appendChild(s)
	</script>


### Initialization Options

Option and default value | Type | Definition
--- | ---
`trackrClass = "track-it` | String | Classes of elements you want to track mouse and keyboard events for
`serverUrl = "http://www.trackit.io"` | URL | The url of the server
`sessionCookieName = "trackit_session"` | String | The name of the session cookie
`v_visitorCookieName = "trackit_v_visitor"` | String | The name of the virtual visitor cookie
`visitorCookieName = "trackit_visitor"` | String | The name of the visitor cookie
`loadstartvariable = "trackrloadstart"`  | String | The name of the timing variable ( the one created in the head of the page )
`tabIdPrefix = "trackit_tab_"` | String | The prefix to track the tabs ( used in window.name )
`trackTabs = true` | Boolean | Tell to track the tabs ( put false if you use window.name in some part of your code )

### Dom events tracking

Trackit tracks Dom events for all elements from the class declared in the `trackrClass` option. 

The events tracked are mouseover, click, focus and blur.

To activate it, you must use these attributes on the tracked elements :

Attribute | Description
---|---
data-trackr-type | The category of the element
data-trackr-label | The content of the element
data-trackr-position | The position / order of the element



## JS Tracker API

### Single Page Applications Page Tracking

For js based applications, call

	trackr.trackPageStart()
	
when the user queries for the new page and

	trackr.trackPageEnd()
	
when the new page is rendered.

---

### User management

#### Signup

When a user signs up.

	trackr.signupUser(id, data, signupDate)
	
Variable|Format
---|---
id|The User ID
data|Any user data in JSON object
signupDate | UTC Unix Timestamp


#### Login

When a user logins.

	trackr.loginUser(id)

Variable|Format
---|---
id|The User ID

#### Login and Import (recommanded)

Allows to import users when userbase is already existing.

	trackr.loginAndImportUser(id, data, signupDate)
	
Variable|Format
---|---
id|The User ID
data|Any user data in JSON object
signupDate | UTC Unix Timestamp

#### Update

Update data about the user

	trackr.updateUser(data)
Variable|Format
---|---
data|Any user data in JSON object

#### Logout

Logs out the user

	trackr.logoutUser()
	
---

### Interests


	
---

### Custom Events

---

### AB Testing and Conversion

---

### E-Commerce


---

## Architecture

### Scalability

The system features full autoscability for the database and the application on AWS and Eucalyptus out of the box.

If the system is deployed in another environment, deploys must be handled by the integrated system deployment manager.

### Database

Aktivat runs on a clusterized and sharded Redis. Each shard is a master with optional slaves replicas.
The sharding strategy is based on account/property. One master node will handle all the data for one property.
Redis is also used for nodes autodiscovery.

The only other database used is Mnesia to handle distributed configuration.

#### Datamodel

##### Prefix

`#{account_id}::#{property_id}::`

##### Session Index

`LPUSH #{account_id}::#{property_id}::session_index::#{YYYY-MM-DD} #{session_uuid}`

###### Session Infos

Key : `#{account_id}::#{property_id}::#{YYYY-MM-DD}::session_#{session_uuid}`

###### Event

Bitmap

`BITSET #{account_id}::#{property_id}::evt:#{eventname}::#{YYYY-MM-DD-HH-MM} #{virtual_visitor_id} 1`
`BITSET #{account_id}::#{property_id}::evt:#{eventname}::#{YYYY-MM-DD-HH} #{virtual_visitor_id} 1`
`BITSET #{account_id}::#{property_id}::evt:#{eventname}::#{YYYY-MM-DD} #{virtual_visitor_id} 1`
`BITSET #{account_id}::#{property_id}::evt:#{eventname}::#{YYYY-MM} #{virtual_visitor_id} 1`
`BITSET #{account_id}::#{property_id}::evt:#{eventname}::#{YYYY} #{virtual_visitor_id} 1`

###### Sources

`BITSET #{account_id}::#{property_id}::src:#{srcname}::cpg:#{cpgname}::#{YYYY-MM-DD-HH-MM} #{virtual_visitor_id} 1`
`BITSET #{account_id}::#{property_id}::src:#{srcname}::cpg:#{cpgname}::#{YYYY-MM-DD-HH} #{virtual_visitor_id} 1`
`BITSET #{account_id}::#{property_id}::src:#{srcname}::cpg:#{cpgname}::#{YYYY-MM-DD} #{virtual_visitor_id} 1`
`BITSET #{account_id}::#{property_id}::src:#{srcname}::cpg:#{cpgname}::#{YYYY-MM} #{virtual_visitor_id} 1`
`BITSET #{account_id}::#{property_id}::src:#{srcname}::cpg:#{cpgname}::#{YYYY} #{virtual_visitor_id} 1`



### Con

	

