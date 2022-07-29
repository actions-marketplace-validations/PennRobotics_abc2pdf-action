// follow-1.js - file included in snd-1.js
//
// This script permits to follow the notes while playing.
// Scrolling the music may be disabled setting 'no_scroll' in the window object.
//
// Copyright (C) 2015-2022 Jean-Francois Moine
//
// This file is part of abc2svg.
//
// abc2svg is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// abc2svg is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with abc2svg.  If not, see <http://www.gnu.org/licenses/>.

// init
function follow(abc, user, playconf) {
    var	keep_types = {
		note: true,
		rest: true
	}

user.anno_stop = function(type, start, stop, x, y, w, h) {
	if (!keep_types[type])
		return

	// create a rectangle
	abc.out_svg('<rect class="abcr _' + start + '_" x="');
	abc.out_sxsy(x, '" y="', y);
	abc.out_svg('" width="' + w.toFixed(2) +
		'" height="' + abc.sh(h).toFixed(2) + '"/>\n')
}

	playconf.onnote = function(i, on) {
	    var	b, x, y,
		elts = document.getElementsByClassName('_' + i + '_')
		if (elts && elts[0]) {
			elts[0].style.fillOpacity = on ? 0.4 : 0

			// scroll for the element to be in the screen
			if (on && !window.no_scroll) {	
				b = elts[0].getBoundingClientRect()

				// normal
				if (b.top < 0)
					y = window.scrollY + b.top -
							window.innerHeight / 2
				else if (b.bottom > window.innerHeight)
					y = window.scrollY + b.bottom +
							window.innerHeight / 2

				// single line
				if (b.left < 0)
					x = window.scrollX + b.left -
							window.innerWidth / 2
				else if (b.right > window.innerWidth)
					x = window.scrollX + b.right +
							window.innerWidth / 2
				if (x != undefined || y != undefined)
					window.scrollTo(x || 0, y || 0)
			}
		}
	}
} // follow()

// create the style of the rectangles
(function () {
    var	sty = document.createElement("style")
	sty.innerHTML = ".abcr {fill: #d00000; fill-opacity: 0; z-index: 15}"
	document.head.appendChild(sty)
})()
