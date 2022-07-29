// abcweb-1.js file to include in html pages
//
// Copyright (C) 2014-2022 Jean-Francois Moine
//
// This file is part of abc2svg.
//
// abc2svg is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// abc2svg is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with abc2svg.  If not, see <http://www.gnu.org/licenses/>.

// This script is used in HTML or XHTML files.
// It replaces the ABC sequences
// - contained in <script> elements with the type "text/vnd.abc", or
// - defined in HTML elements with the class "abc", or
// - starting with "%abc-n" or "X:n" at start of line up to a XML tag
// by music as SVG images.
// The other elements stay in place.
// The script abc2svg-1.js may be loaded before this script.
// It is automatically loaded when not present.
//
// When the file is .html, if the ABC sequence is contained inside
// elements <script type="text/vnd.abc">, there is no constraint
// about the ABC characters. Note that the <script> tag is removed.
// With a container of class "abc", the characters '<', '>' and '&' may be
// replaced by their XML counterparts ('&lt;', '&gt;' and '&amp;').
// When the file is .xhtml, if the ABC sequence contains the characters
// '<', '>' or '&', this sequence must be enclosed in a XML comment
// (%<!-- .. %-->) or in a CDATA (%<![CDATA[ .. %]]>).
//
// ABC parameters may be defined in the query string of the URL.

window.onerror = function(msg, url, line) {
	if (typeof msg == 'string')
		alert("window error: " + msg +
			"\nURL: " + url +
			"\nLine: " + line)
	else if (typeof msg == 'object')
		alert("window error: " + msg.type + ' ' + msg.target.src)
	else
		alert("window error: " + msg)
	return false
}

    var	user,
	abcplay

if (typeof abc2svg == "undefined")
    var abc2svg = {}

// function called when abc2svg is fully loaded
function dom_loaded() {
var	abc, src,
	a_inc = {},
	new_page = '',
	errtxt = '',
	html,			// set if some pure HTML in the file
	app = "abcweb",
	glopar = '',		// global parameters from URL
	playing,
	playconf = {
		onend: function() {
			playing = false
		}
	},
	page,				// document source
	tune_lst = [],	// array of [tsfirst, voice_tb, info, cfmt] per tune
	jsdir = document.currentScript ?
		    document.currentScript.src.match(/.*\//) :
		    (function() {
		     var s_a = document.getElementsByTagName('script')
			for (var k = 0; k < s_a.length; k++) {
				if (s_a[k].src.indexOf(app) >= 0)
					return s_a[k].src.match(/.*\//) || ''
			}
			return ""	// ??
	})()

// -- abc2svg init argument
    user = {
	read_file: function(fn) {
		return a_inc[fn]
	}, // read_file()
	errmsg: function(msg, l, c) {	// get the errors
		errtxt += clean_txt(msg) + '\n'
	},
	img_out: function(str) {	// image output
		new_page += str
	}
    }

// replace <>& by XML character references
function clean_txt(txt) {
	return txt.replace(/<|>|&.*?;|&/g, function(c) {
		switch (c) {
		case '<': return "&lt;"
		case '>': return "&gt;"
		case '&': return "&amp;"
		}
		return c
	})
}

	// function called on click on the music
	abc2svg.playseq = function(evt) {
		if (playing) {		// stop playing
			abcplay.stop()
			return
		}
	    var	i, j,
		svg = evt.target,
		e = svg			// keep the clicked element

		// search if click in a SVG image
		while (svg.tagName != 'svg') {
			svg = svg.parentNode
			if (!svg)
				return
		}
		i = svg.getAttribute('class')
		if (!i)
			return
		i = i.match(/tune(\d+)/)
		if (!i)
			return
		i = i[1]		// tune number

		// initialize the play object
		if (!abcplay) {
			if (typeof AbcPlay == "undefined") { // as snd-1.js not loaded,
				abc2svg.playseq = function(){}	// don't come here anymore
				return
			}
			if (abc.cfmt().soundfont)
				playconf.sfu = abc.cfmt().soundfont
			abcplay = AbcPlay(playconf);
		}

		// if not done yet,
		// generate the play data of the tune
		if (!tune_lst[i]) {
			tune_lst[i] = abc.tunes[i]
			abcplay.add(tune_lst[i][0],
					tune_lst[i][1],
					tune_lst[i][3])
		}

		// check if click on a music symbol
		// (this works when 'follow' is active)
		s = tune_lst[i][0]		// first symbol of the tune
		i = e.getAttribute('class')
		if (i)
			i = i.match(/abcr _(\d+)_/)
		if (i) {
			i = i[1]		// symbol offset in the source
			while (s && s.istart != i)
				s = s.ts_next
			if (!s) {		// fixme: error ?!
				alert("play bug: no such symbol in the tune")
				return
			}
		}

		playing = true
		abcplay.play(s, null)
	} // playseq()

	// convert HTML to ABC
	function toabc(s) {
		return s.replace(/&gt;/g, '>')
			.replace(/&lt;/g, '<')
			.replace(/&amp;/g, '&')
			.replace(/[ \t]+(%%|.:)/g, '$1')
	} // toabc()

// function to load javascript files
	abc2svg.loadjs = function(fn, relay, onerror) {
		var s = document.createElement('script')
		if (/:\/\//.test(fn))
			s.src = fn		// absolute URL
		else
			s.src = jsdir + fn
		s.onload = relay
		s.onerror = function() {
			if (onerror)
				onerror(fn)
			else
				alert('error loading ' + fn)
		}
		document.head.appendChild(s)
	}

function render() {

	// search the ABC tunes,
	// replace them by SVG images with play on click

//search in page
// 1- <script type="text/vnd.abc"> ..ABC.. <script>
// 2- <anytag .. class="abc" .. > ..ABC.. </anytag>
// 3- %abc-n ..ABC.. '<' with skip %%beginxxx .. %%endxxx
// 4- X:n ..ABC.. '<' with skip %%beginxxx .. %%endxxx

    var	re, src, res,
	ss = 0,
	re_stop = /\n<|\n%.begin[^\s]+/g

	// get the parameters from the script class
	// format '--<key>:<value>'
	function get_p(p) {
	    var	i, j, k, r,
		o = '',
		c = p.match(/class="[^"]+"/),	// "
		sh = document.styleSheets

		if (!c)
			return
		c = '.' + c[0].slice(7, -1)		// selector
		for (i = 0; i < sh.length; i++) {
			r = sh[i].cssRules
			for (j = 0; j < r.length; j++) {
				if (r[j].selectorText == c)
					break
			}
			if (j < r.length)
				break
		}
		if (i == sh.length)
			return

		r = r[j]				// cssRule
		for (i = 0; i < r.styleMap.size; i++) {
			k = r.style[i]
			o += '%%' + k.slice(2)
				+ ' ' + r.style.getPropertyValue(k)
				+ '\n'
		}
		if (o)
			abc.tosvg(app, o)
		return o
	} // get_p()

	// extract a music sequence
	// @re is the regular expression starting a music sequence
	function extract() {
	    var	i, j, k, t

		res = re.exec(page)
		if (!res) {

			// alert about bad print
			if (abc2svg.page && html) {
				new_page += '\
<pre class="nop" style="background:#ff8080">\
Printing may be bad because the file contains pure HTML and %%pageheight\
</pre>\n'
				errtxt = ""
			}

			document.body.innerHTML = new_page // browser page update
				+ page.slice(ss)	// HTML end

			// add the event handlers for playback
			t = document.getElementsByTagName('svg')
			for (i = 0; i < t.length; i++) {
			    if (t[i].getAttribute('class').indexOf('tune') >= 0)
				t[i].addEventListener('click', abc2svg.playseq)
			}
			return				// done
		}
		i = re.lastIndex - res[0].length	// start of music sequence
		if (i > ss) {
			new_page += page.slice(ss, i)		// HTML copy
			html = 1
		}
		t = res[2]
		if (t[0] == '<') {
			t = t.slice(1, 4)
			switch (t) {
			case "scr":			// <script
				k = page.indexOf('>', i)
				get_p(page.slice(i, k))	// get the parameters
				i = page.indexOf('\n', k) + 1
				j = page.indexOf('</script>', i)
				ss = j + 9
				break
			default:			// < .. class="abc"
				i = page.indexOf('>', i) + 1	// start of ABC
				new_page += page.slice(j, i)	// keep the element

				j = res[2].indexOf(' ')		// get the tag
				t = res[2].slice(1, j)

				j = page.indexOf('</' + t + '>', i)
				ss = j + t.length + 4
				break
			}
		} else {		// %abc or X:

			// get the end of the ABC sequence
			// including the %%beginxxx/%%endxxx sequences
			re_stop.lastIndex = i
			while (1) {
				res = re_stop.exec(page)
				if (!res || res[0] == "\n<")
					break
				k = page.indexOf(res[0].replace("begin", "end"),
						re_stop.lastIndex)
				if (k < 0)
					break
				re_stop.lastIndex = k
			}
			if (!res || k < 0)
				j = page.length
			else
				j = re_stop.lastIndex - 1
			ss = j
		}
		if (j < 0) {
//fixme			error
alert("Error: no end of ABC sequence")
console.log('error i/j '+i+' '+j)
			return
		}

		re.lastIndex = ss		// new HTML start
//		src = page.slice(i, j)		// music sequence
//		if (abc2svg.modules.load(src, render1))
//			render1()
//	} // extract()

	// render one text music sequence
	// @page is the content of the page
	// @ss and @se are the start and end indexes of the music sequence
	// i,j are the start and stop of the music inside the page source
//	function render1() {
		abc.tosvg(app, page, i, j)
		abc2svg.abc_end()	// close the page if %%pageheight

		if (errtxt) {
			new_page += '<pre class="nop" style="background:#ff8080">'
					+ errtxt + "</pre>\n"
			errtxt = ""
		}

		// try the next sequence
		extract()
	} // render1()

	// get the ABC insertion mode
	if (/<script[^t>]*type="text\/vnd.abc"/.test(page))
		re = '<script[^t>]*type="text\/vnd.abc"'
	else if (/<[^>]* class="[^"]*abc[^"]*/.test(page))	// "
		re = '<[^>]* class="[^"]*abc[^"]*'
	else
		re = '%abc-\\d|X:\\s*\\d'

	re = new RegExp('(^|\n)(' + re + ')', 'g')

	// prepare the generation of the music sequences
	abc = new abc2svg.Abc(user)
	if (typeof follow == "function") // initialize the play follow function
		follow(abc, user, playconf)
	abc.tosvg(app, glopar)

	extract()			// start
} // render()

	// load the %%abc-include files
	function include() {
	    var	i, j, fn, r,
		k = 0

		while (1) {
			i = page.indexOf('%%abc-include ', k)
			if (i < 0) {
				render()
				return
			}
			i += 14
			j = page.indexOf('\n', i)
			fn = page.slice(i, j).trim()
			if (!a_inc[fn])
				break
			k = j
		}

		// %%abc-include found: load the file
		r = new XMLHttpRequest()
		r.open('GET', fn, true)		// (async)
		r.onload = function() {
			if (r.status === 200) {
				a_inc[fn] = r.responseText
				if (abc2svg.modules.load(a_inc[fn], include))
					include()
			} else {
				a_inc[fn] = '%\n'
				alert('Error getting ' + fn + '\n' + r.statusText)
				include()
			}
		}
		r.onerror = function () {
			a_inc[fn] = '%\n'
			alert('Error getting ' + fn + '\n' + r.statusText)
			include()
		}
		r.send()
	} // include()

	// --- dom_loaded() main code ---

	// load the abc2svg core if not done by <script>
	if (!abc2svg.Abc) {
		abc2svg.loadjs(document.body.innerHTML.indexOf("<mei ") >= 0 ?
					"mei2svg-1.js" :
					"abc2svg-1.js",
						dom_loaded)
		return
	}

	// use the query string of the URL for global parameters
	{
	    var	i, j,
		k = location.search.substr(1).split("&")

	    for (i = 0; i < k.length; i++)
		glopar += "%%" + decodeURIComponent(k[i].replace('=', ' ')) + '\n'
	}

	// accept page formatting
	abc2svg.abc_end = function() {}

	// load the included files if any
	page = document.body.innerHTML		// initial page

	// load the required modules and the include files before rendering
	if (abc2svg.modules.load(page, include))
		include()
} // dom_loaded()

// wait for the scripts to be loaded
window.addEventListener("load", dom_loaded, {once:true})
