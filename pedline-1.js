// abc2svg - ABC to SVG translator
// @source: https://chiselapp.com/user/moinejf/repository/abc2svg
// Copyright (C) 2014-2022 Jean-Francois Moine - LGPL3+
// pedline.js - module to draw pedal lines instead of 'Ped .. *'
//
// Copyright (C) 2020-2022 Jean-Francois Moine
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
//
// This module is loaded when "%%pedline" appears in a ABC source.
//
// Parameters
//	%%pedline 1

abc2svg.pedline = {
    draw_all_deco: function(of) {
    var	de, i,
	a_de = this.a_de()

	if (!a_de.length)
		return			// no decoration in this music line
	if (this.cfmt().pedline) {
		for (i = 0; i < a_de.length; i++) {
			de = a_de[i]
			if (de.dd.name != "ped)")
				continue
			if (de.prev
			 && de.prev.dd.name == "ped)") {
// ( .. ) ( .. )
//		\ de
//	\ de.prev
// \ de.prev.start
// |_____/\____|
				de.defl.nost = true
				de.prev.defl.noen = true
				de.x = de.prev.s.x - 5
				de.val = de.s.x - de.x - 5
				de.prev.val = de.x - de.prev.x
			} else {
				de.x -= 3
				de.val += 10
			}
		}
	}
	of()
    }, // draw_all_deco()

    out_lped: function(of, x, y, val, defl) {
	if (!this.cfmt().pedline) {
		of(x, y, val, defl)
		return
	}
	this.xypath(x, y + 16)
	if (defl.nost) {
		this.out_svg("l2.5 6")
		val -= 2.5
	} else {
		this.out_svg("v6")
	}
	if (defl.noen) {
		val -= 2.5
		this.out_svg("h" + val.toFixed(1) + 'l2.5 -6"/>\n')
	} else {
		this.out_svg("h" + val.toFixed(1) + 'v-6"/>\n')
	}
    }, // out_lped()

    set_fmt: function(of, cmd, param) {
	if (cmd == "pedline")
		this.cfmt().pedline = this.get_bool(param)
	else
		of(cmd, param)
    }, // set_fmt()

    set_hooks: function(abc) {
	abc.draw_all_deco = abc2svg.pedline.draw_all_deco.bind(abc, abc.draw_all_deco)
	abc.out_lped = abc2svg.pedline.out_lped.bind(abc, abc.out_lped)
	abc.set_format = abc2svg.pedline.set_fmt.bind(abc, abc.set_format)
    } // set_hooks()
} // pedline

abc2svg.modules.hooks.push(abc2svg.pedline.set_hooks)

// the module is loaded
abc2svg.modules.pedline.loaded = true
