// abc2svg - tune.js - tune generation
//
// Copyright (C) 2014-2022 Jean-Francois Moine
//
// This file is part of abc2svg-core.
//
// abc2svg-core is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// abc2svg-core is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with abc2svg-core.  If not, see <http://www.gnu.org/licenses/>.

var	par_sy,		// current staff system for parse
	cur_sy,		// current staff system for generation
	voice_tb,
	curvoice,
	staves_found,
	vover,		// voice overlay
	tsfirst

/* apply the %%voice options of the current voice */
function voice_filter() {
    var	opt

	function vfilt(opts, opt) {
	    var	i,
		sel = new RegExp(opt)

		if (sel.test(curvoice.id)
		 || sel.test(curvoice.nm)) {
			for (i = 0; i < opts.length; i++)
				self.do_pscom(opts[i])
		}
	}

	// global
	if (parse.voice_opts)
	    for (opt in parse.voice_opts) {
		if (parse.voice_opts.hasOwnProperty(opt))
			vfilt(parse.voice_opts[opt], opt)
	}

	// tune
	if (parse.tune_v_opts)
	    for (opt in parse.tune_v_opts) {
		if (parse.tune_v_opts.hasOwnProperty(opt))
			vfilt(parse.tune_v_opts[opt], opt)
	}
}

/* -- link a ABC symbol into the current voice -- */
// if a voice is ignored (not in %%staves) don't link the symbol
//	but update the time for P: and Q:
function sym_link(s) {
    var	tim = curvoice.time

	if (!s.fname)
		set_ref(s)
    if (!curvoice.ignore) {
	s.prev = curvoice.last_sym
	if (curvoice.last_sym)
		curvoice.last_sym.next = s
	else
		curvoice.sym = s
    }
	curvoice.last_sym = s
	s.v = curvoice.v;
	s.p_v = curvoice;
	s.st = curvoice.cst;
	s.time = tim
	if (s.dur && !s.grace)
		curvoice.time += s.dur;
	parse.ufmt = true
	s.fmt = cfmt				// global parameters
	s.pos = curvoice.pos
	if (curvoice.second)
		s.second = true
	if (curvoice.floating)
		s.floating = true
	if (curvoice.eoln) {
		s.soln = true
		curvoice.eoln = false
	}
}

/* -- add a new symbol in a voice -- */
function sym_add(p_voice, type) {
	var	s = {
			type:type,
			dur:0
		},
		s2,
		p_voice2 = curvoice;

	curvoice = p_voice;
	sym_link(s);
	curvoice = p_voice2;
	s2 = s.prev
	if (!s2)
		s2 = s.next
	if (s2) {
		s.fname = s2.fname;
		s.istart = s2.istart;
		s.iend = s2.iend
	}
	return s
}

/* -- sort all symbols by time and vertical sequence -- */
// weight of the symbols !! depends on the symbol type !!
var w_tb = new Uint8Array([
	4,	// bar
	1,	// clef
	8,	// custos
	5,	// sm (sequence marker)
	0,	// grace
	2,	// key
	3,	// meter
	9,	// mrest
	9,	// note
	0,	// part
	9,	// rest
	6,	// space (after bar)
	0,	// staves
	7,	// stbrk
	0,	// tempo
	0,	// (free)
	0,	// block
	0	// remark
])

function sort_all() {
    var	s, s2, time, w, wmin, ir, fmt,
	fl, new_sy,
	nv = voice_tb.length,
	vtb = [],
	vn = [],			// voice indexed by range
	sy = cur_sy,			// first staff system
	v = sy.top_voice,
	p_voice = voice_tb[v],		// top voice
	prev = {			// symbol defining the first staff system
		type: C.STAVES,
		dur: 0,
		v: v,
		p_v: p_voice,
		time: 0,
		st: 0,
		sy: sy,
		next: p_voice.sym,
		seqst: true
	}

	if (!p_voice.sym)
		return

	prev.fmt = fmt = p_voice.sym.fmt	// starting format

	// set the first symbol of each voice
	for (v = 0; v < nv; v++) {
		s = voice_tb[v].sym
		vtb[v] = s
		if (sy.voices[v])
			vn[sy.voices[v].range] = v
	}

	// insert the first staff system in the top voice
	p_voice.sym = tsfirst = s = prev
	if (s.next)
		s.next.prev = s
	else
		p_voice.last_sym = s

	// if Q: from tune header, put it at start of the music
	// (after the staff system)
	s = glovar.tempo
	if (s) {
		s.v = v = p_voice.v
		s.p_v = p_voice
		s.st = 0
		s.time = 0
		s.prev = prev
		s.next = prev.next
		if (s.next)
			s.next.prev = s
		else
			p_voice.last_sym = s
		s.prev.next = s
		s.fmt = fmt
		glovar.tempo = null
		vtb[v] = s
	}

	// loop on the symbols of all voices
	while (1) {
		if (new_sy) {
			sy = new_sy;
			new_sy = null;
			vn = []
			for (v = 0; v < nv; v++) {
				if (!sy.voices[v])
					continue
				vn[sy.voices[v].range] = v
			}
		}

		/* search the min time and symbol weight */
		wmin = time = 10000000		// big int
		ir = 0
		while (1) {
			v = vn[ir++]
			if (v == undefined)
				break
			s = vtb[v]
			if (!s || s.time > time)
				continue
			w = w_tb[s.type]
			if (s.time < time) {
				time = s.time;
				wmin = w
			} else if (w < wmin) {
				wmin = w
			}
		}

		if (wmin > 127)
			break			// done

		/* link the vertical sequence */
		ir = 0
		while (1) {
			v = vn[ir++]
			if (v == undefined)
				break
			s = vtb[v]
			if (!s || s.time != time
			 || w_tb[s.type] != wmin)
				continue
			if (s.type == C.STAVES) {
				new_sy = s.sy
			} else if (s.type == C.SM) {	// if a sequence marker
				s.next.prev = s.prev	// remove it
				if (s.prev)
					s.prev.next = s.next
				else
					s.p_v.sym = s.next
				vtb[v] = s.next
				continue
			}
			if (fl) {
				fl = 0;
				s.seqst = true
			}
			s.ts_prev = prev
			prev.ts_next = s
			prev = s

			vtb[v] = s.next
		}
		fl = wmin		/* start a new sequence if some width */
	}
}

// adjust some voice elements
function voice_adj(sys_chg) {
    var	p_voice, s, s2, v, sl

	// set the duration of the notes under a feathered beam
	function set_feathered_beam(s1) {
		var	s, s2, t, d, b, i, a,
			d = s1.dur,
			n = 1

		/* search the end of the beam */
		for (s = s1; s; s = s.next) {
			if (s.beam_end || !s.next)
				break
			n++
		}
		if (n <= 1) {
			delete s1.feathered_beam
			return
		}
		s2 = s;
		b = d / 2;		/* smallest note duration */
		a = d / (n - 1);	/* delta duration */
		t = s1.time
		if (s1.feathered_beam > 0) {	/* !beam-accel! */
			for (s = s1, i = n - 1;
			     s != s2;
			     s = s.next, i--) {
				d = ((a * i) | 0) + b;
				s.dur = d;
				s.time = t;
				t += d
			}
		} else {				/* !beam-rall! */
			for (s = s1, i = 0;
			     s != s2;
			     s = s.next, i++) {
				d = ((a * i) | 0) + b;
				s.dur = d;
				s.time = t;
				t += d
			}
		}
		s.dur = s.time + s.dur - t;
		s.time = t
	} // end set_feathered_beam()

	// terminate voice cloning
	if (curvoice && curvoice.clone) {
		parse.istart = parse.eol
		do_cloning()
	}

	for (v = 0; v < voice_tb.length; v++) {
		p_voice = voice_tb[v]
		if (!sys_chg) {			// if not %%score
			delete p_voice.eoln
			while (1) {		// set the end of slurs
				sl = p_voice.sls.shift()
				if (!sl)
					break
				s = sl.note.s
				for (s2 = s.next; s2; s2 = s2.next) {
					if (s2.bar_type && s2.bar_type[0] == ':')
						break
				}
				if (s2) {
					if (!s.sls)
						s.sls = []
					s.sls.push({
						ty: sl.ty,
						note: {s: s2}
					})
				} else {
//					error(1, s, "Lack of ending slur(s)")
					if (!s.sls)
						s.sls = []
					s.sls.push({
						ty: sl.ty,
						note: {s: s2},
						loc: 'o'	// no slur end
					})
				}
			}
		} // not %%score
		for (s = p_voice.sym; s; s = s.next) {
			if (s.time >= staves_found)
				break
		}
		for ( ; s; s = s.next) {

			// if the symbol has no sequence weight
			// and if there a time skip,
			// add a sequence marker before it
			if (!w_tb[s.type]
			 && s.type != C.STAVES
			 && (!s.prev || s.time > s.prev.time + s.prev.dur)) {
				s2 = {
					type: C.SM,
					v: s.v,
					p_v: s.p_v,
					time: s.time,
					dur:0,
					next: s,
					prev: s.prev,
					fmt: s.fmt
				}
				if (s.prev)
					s.prev.next = s2
				else
					voice_tb[s.v].sym = s2
				s.prev = s2
			}

			switch (s.type) {
			case C.GRACE:
				if (!cfmt.graceword)
					continue
				for (s2 = s.next; s2; s2 = s2.next) {
					switch (s2.type) {
					case C.SPACE:
						continue
					case C.NOTE:
						if (!s2.a_ly)
							break
						s.a_ly = s2.a_ly;
						s2.a_ly = null
						break
					}
					break
				}
				continue
			}

			if (s.feathered_beam)
				set_feathered_beam(s)
		}
	}
}

/* -- create a new staff system -- */
function new_syst(init) {
    var	st, v, sy_staff, p_voice,
	sy_new = {
		voices: [],
		staves: [],
		top_voice: 0
	}

	if (init) {				/* first staff system */
		cur_sy = par_sy = sy_new
		return
	}

	// update the previous system
	for (v = 0; v < voice_tb.length; v++) {
	    if (par_sy.voices[v]) {
		st = par_sy.voices[v].st
		sy_staff = par_sy.staves[st]
		p_voice = voice_tb[v]

		sy_staff.staffnonote = p_voice.staffnonote
		if (p_voice.staffscale)
			sy_staff.staffscale = p_voice.staffscale;
	    }
	}
	for (st = 0; st < par_sy.staves.length; st++) {
		sy_new.staves[st] = clone(par_sy.staves[st]);
		sy_new.staves[st].flags = 0
	}
	par_sy.next = sy_new;
	par_sy = sy_new
}

/* -- set the bar numbers -- */
// (possible hook)
Abc.prototype.set_bar_num = function() {
    var	s, s2, tim, rep_tim, k, n, b_typ,
	bar_num = gene.nbar,
	bar_tim = 0,			// time of previous repeat variant
	ptim = 0,			// time of previous bar
	wmeasure = voice_tb[cur_sy.top_voice].meter.wmeasure

	// don't count a bar at start of tune
	for (s = tsfirst; ; s = s.ts_next) {
		if (!s)
			return
		switch (s.type) {
		case C.METER:
			wmeasure = s.wmeasure
			// fall thru
		case C.CLEF:
		case C.KEY:
		case C.STBRK:
			continue
		case C.BAR:
			if (s.bar_num)
				bar_num = s.bar_num	// %%setbarnb)
			break
		}
		break
	}

	// at start of tune, check for an anacrusis
	for (s2 = s.ts_next; s2; s2 = s2.ts_next) {
		if (s2.type == C.BAR && s2.time) {
			if (s2.time < wmeasure) {	// if anacrusis
				s = s2
				bar_tim = s.time
			}
			break
		}
	}

	// set the measure number on the top bars
	for ( ; s; s = s.ts_next) {
		switch (s.type) {
		case C.METER:
			if (s.time == bar_tim)
				break		// already seen
		    if (wmeasure != 1)		// if not M:none
			bar_num += (s.time - bar_tim) / wmeasure
			bar_tim = s.time
			wmeasure = s.wmeasure
			break
		case C.BAR:
			if (s.invis)
				break
			if (s.time != tim)
				b_typ = 0
			tim = s.time
			k = s.text ? 1 : 2		// bar type: variant or not
			if (b_typ & k)
				break			// bar already seen
			b_typ |= k
			if (s.bar_num) {
				bar_num = s.bar_num	// (%%setbarnb)
				ptim = bar_tim = tim
				continue
			}
			if (wmeasure == 1) {		// if M:none
				if (s.bar_dotted)
					continue
				if (s.text) {
					if (!cfmt.contbarnb) {
						if (s.text[0] == '1')
							rep_tim = bar_num
						else
							bar_num = rep_tim
					}
				}
				s.bar_num = ++bar_num
				continue
			}

			n = bar_num + (tim - bar_tim) / wmeasure
			k = n - (n | 0)
			if (cfmt.checkbars
			 && ((k && !s.bar_dotted && s.next)
			  || (tim > ptim + wmeasure && s.prev.type != C.MREST)))
				error(0, s, "Bad measure duration")
			if (tim > ptim + wmeasure) {	// if more than one measure
				n |= 0
				k = 0
				bar_tim = tim		// re-synchronize
				bar_num = n
			}
			if (s.text) {
				if (s.text[0] == '1') {
					if (cfmt.contbarnb)
						rep_tim = bar_tim + k * wmeasure
					else
						rep_tim = tim
					if (!k)
						s.bar_num = n
				} else {
					if (cfmt.contbarnb)
						bar_tim = rep_tim
					else
						bar_tim += tim - rep_tim
					n = bar_num + (tim - bar_tim) / wmeasure
					if (n == (n | 0))
						s.bar_num = n
				}
			} else {
				if (k)
					n -= k
				s.bar_num = n
			}
			if (!k)
				ptim = tim
			break
		}
	}
}

// convert a note to ABC
function not2abc(pit, acc) {
    var	i,
	nn = ''

	if (acc && acc != 3) {
		if (typeof acc != "object") {
			nn = ['__', '_', '', '^', '^^'][acc + 2]
		} else {
			i = acc[0]
			if (i > 0) {
				nn += '^'
			} else {
				nn += '_'
				i = -i
			}
			nn += i + '/' + acc[1]
		}
	}
	nn += ntb[(pit + 75) % 7]
	for (i = pit; i >= 23; i -= 7)
		nn += "'"
	for (i = pit; i < 16; i += 7)
		nn += ","
	return nn
} // not2abc()

// note mapping
// %%map map_name note [print [note_head]] [param]*
function get_map(text) {
	if (!text)
		return

    var	i, note, notes, map, tmp, ns,
	ty = '',
	a = text.split(/\s+/)

	if (a.length < 3) {
		syntax(1, not_enough_p)
		return
	}
	ns = a[1]
	if (ns[0] == '*' || ns.indexOf("all") == 0) {
		ns = 'all'
	} else {
		if (ns.indexOf("octave,") == 0	// remove the octave part
		 || ns.indexOf("key,") == 0) {
			ty = ns[0]
			ns = ns.split(',')[1]
			ns = ns.replace(/[,']+/, '').toUpperCase() //'
			if (ns.indexOf("key,") == 0)
				ns = ns.replace(/[=^_]+/, '')
		}
		tmp = new scanBuf
		tmp.buffer = ns
		note = parse_acc_pit(tmp)
		if (!note) {
			syntax(1, "Bad note in %%map")
			return
		}
		ns = ty + not2abc(note.pit, note.acc)
	}

	notes = maps[a[0]]
	if (!notes)
		maps[a[0]] = notes = {}
	map = notes[ns]
	if (!map)
		notes[ns] = map = []

	// try the optional 'print' and 'heads' parameters
	a.shift()
	a.shift()
	if (!a.length)
		return
	a = info_split(a.join(' '))
	i = 0
	if (a[0].indexOf('=') < 0) {
		if (a[0][0] != '*') {
			tmp = new scanBuf;		// print
			tmp.buffer = a[0];
			map[1] = parse_acc_pit(tmp)
		}
		if (!a[1])
			return
		i++
		if (a[1].indexOf('=') < 0) {
			map[0] = a[1].split(',')	// heads
			i++
		}
	}

	for (; i < a.length; i++) {
		switch (a[i]) {
		case "heads=":
			if (!a[++i]) {
				syntax(1, not_enough_p)
				break
			}
			map[0] = a[i].split(',')
			break
		case "print=":
		case "play=":
			if (!a[++i]) {
				syntax(1, not_enough_p)
				break
			}
			tmp = new scanBuf;
			tmp.buffer = a[i];
			note = parse_acc_pit(tmp)
			if (a[i - 1][1] == 'r')
				map[1] = note
			else
				map[3] = note
			break
		case "color=":
			if (!a[++i]) {
				syntax(1, not_enough_p)
				break
			}
			map[2] = a[i]
			break
		}
	}
}

// set the transposition in the previous or first key signature
function set_transp() {
    var	s, transp, sndtran

	if (curvoice.ckey.k_bagpipe || curvoice.ckey.k_drum)
		return

	if (cfmt.transp && curvoice.shift)	// if %%transpose and shift=
		syntax(0, "Mix of old and new transposition syntaxes");

	if (cfmt.transp != undefined
	 || curvoice.transp != undefined
	 || curvoice.shift != undefined)
		transp = (cfmt.transp || 0) +	 // %%transpose
			(curvoice.transp || 0) + // score= / sound= / instrument=
			(curvoice.shift || 0)	 // shift=
	if (curvoice.sndtran != undefined
	 || curvoice.sndsh != undefined)
		sndtran = (curvoice.sndtran || 0) +
			(curvoice.sndsh || 0)
	if (transp == undefined) {
		if (sndtran == undefined)
			return
	} else {
		curvoice.vtransp = transp
	}

	if (is_voice_sig()) {			// if no symbol yet
		curvoice.okey.fmt = cfmt
		curvoice.key = s = clone(curvoice.okey)
	} else {
		s = curvoice.last_sym
		while (1) {	// set the transposition in the previous K:
			if (s.type == C.KEY)
				break
			s = s.prev
			if (!s) {
				s = curvoice.key	// first key
				break
			}
		}
	}
	if (transp != undefined)
		s.k_transp = transp
	if (sndtran != undefined)
		s.k_sndtran = sndtran
	curvoice.ckey = clone(s)
	if (curvoice.key.k_none)
		s.k_sf = 0
}

/* transpose a note / chord */
function note_transp(s, sk, note) {
    var	ak, an, d, b40,
	n = note.pit,
	a = note.acc

	if (typeof a == "object") {
		d = a[1]
		a = a[0]
		if (d != 2) {
			error(1, s, "Microtone transposition not coded")
			return
		}
	}
	if (!a && sk.k_a_acc)			// if accidental list
		a = sk.k_map[(n + 19) % 7]	// invisible accidental

	b40 = abc2svg.pab40(n, a) + sk.k_transp	// base-40 transposition

	note.pit = abc2svg.b40p(b40)		// new pitch

	if (!a) {				// if no old accidental
		if (!sk.k_a_acc			// if no accidental list
		 && !sk.k_none)			// and normal key
			return			// same accidental (in the key)
	}

	an = abc2svg.b40a(b40)			// new accidental
	if (a) {
		if (sk.k_a_acc) {		// if accidental list
			ak = sk.k_map[(note.pit + 19) % 7]
			if (ak == an)
				an = 0		// accidental in the key
		}
		if (!an)
			an = 3
	} else if (sk.k_none) {			// if no key
		if (acc_same_pitch(s, note.pit)) // and accidental from previous notes
			return			// no change
	} else if (sk.k_a_acc) {		// if accidental list
		if (acc_same_pitch(s, note.pit)) // and accidental from previous notes
			return			// no change
		ak = sk.k_map[(note.pit + 19) % 7]
		if (ak)
			an = 3		// natural
	} else {
		return			// same accidental (in the key)
	}
	if (d && an != a) {
		switch (Number(a)) {
		case -3:
			switch (an) {
			case -2:
				an = -1
				break
			case 3:			// natural
				an = -3
				note.pit++
				break
			case 2:
				an = -1
				break
			}
			break
		case -1:
			switch (an) {
			case -2:
				an = -3
				break
			case 3:			// natural
				an = 1
				break
			}
			break
		case 1:
			switch (an) {
			case -1:
				an = -3
				break
			case 3:			// natural
				an = -1
				break
			case 2:
				an = 3
				break
			}
			break
		case 3:
			switch (an) {
			case -1:
				an = 1
				break
			case 1:
				an = 1
				note.pit++
				break
			}
			break
		}
	}
	note.acc = d ? [an, d] : an
}

// adjust the pitches according to the transposition(s)
function pit_adj() {
    var	i, p_v, s, sk, g,
	nv = voice_tb.length

	while (--nv >= 0) {
		p_v = voice_tb[nv]
		if (p_v.vtransp == undefined)
			continue	// no transposition in this voice
		if (p_v.key.k_transp) {
			sk = p_v.key
			key_transp(sk)
			sk.k_old_sf = sk.k_sf	// no natural
		} else {
			sk = null
		}
		s = p_v.sym
		while (s) {

			// search a transposing key signature
			if (!sk) {
				for (; s; s = s.next) {
					if (s.type == C.KEY
					 && s.k_transp)
						break
				}
			}

			// transpose
			for (; s; s = s.next) {
				switch (s.type) {
				case C.GRACE:
					for (g = s.extra; g; g = g.next) {
						for (i = 0; i <= g.nhd; i++)
							note_transp(g, sk, g.notes[i])
					}
					continue
				case C.NOTE:
				case C.REST:
					for (i = 0; i <= s.nhd; i++)
						note_transp(s, sk, s.notes[i])
					if (s.a_gch)
						gch_transp(s, sk)
					continue
				case C.KEY:
					if (sk)
						s.k_sf = sk.k_sf
					key_transp(s)
					if (!s.k_transp) // end of transposition
						break
					sk = s
				default:
					continue
				}
				break
			}
			sk = null
		}
	}
} // pit_adj()

// set the control values (P: and Q:)
function set_ctrl() {
    var	s, tim, e

	for (tim in parse.ctrl) {
//		if (!parse.ctrl.hasOwnProperty(tim))
//			continue
		e = parse.ctrl[tim]
		s = tsfirst
		while (s.next && s.next.time < tim)
			s = s.next
		while (s && s.time < tim)
			s = s.ts_next
		if (!s) {
			//fixme: insert at the end
		} else {

			// put the P: and/or Q: on a note or rest
			while (s.time == tim && s.ts_next
			 && !s.dur)
				s = s.ts_next
			if (e.part) {
//				if (s.time != tim) {
//					// fixme: insert a part (?)
//				}
				s.part = e.part
			}
			if (e.tempo) {
				lkvsym(e.tempo, s)
				lktsym(e.tempo, s)
			}
		}
	}
} // set_ctrl()

// get a abcm2ps/abcMIDI compatible transposition value as a base-40 interval
// The value may be
// - [+|-]<number of semitones>[s|f]
// - <note1>[<note2>]  % <note2> default is 'c'
function get_transp(param) {
	if (param[0] == '0')
		return 0
	if ("123456789-+".indexOf(param[0]) >= 0) {	// by semi-tone
	    var	val = parseInt(param)
		if (isNaN(val) || val < -36 || val > 36) {
//fixme: no source reference...
			syntax(1, errs.bad_transp)
			return
		}
		val += 36
		return (((val / 12) | 0) - 3) * 40 +
			(param.slice(-1) == 'b' ?
					abc2svg.ifb40 :
					abc2svg.isb40)[val % 12]
	}
	// return undefined
} // get_transp()

/* -- process a pseudo-comment (%% or I:) -- */
// (possible hook)
Abc.prototype.do_pscom = function(text) {
    var	h1, val, s, cmd, param, n, k, b

	cmd = text.match(/[^\s]+/)
	if (!cmd)
		return
	cmd = cmd[0];

	// ignore the command if the voice is ignored,
	// but not if %%score/%%staves!
	if (curvoice && curvoice.ignore) {
		switch (cmd) {
		case "staves":
		case "score":
			break
		default:
			return
		}
	}

	param = text.replace(cmd, '').trim()

	if (param.slice(-5) == ' lock') {
		fmt_lock[cmd] = true;
		param = param.slice(0, -5).trim()
	} else if (fmt_lock[cmd]) {
		return
	}

	switch (cmd) {
	case "clef":
		if (parse.state >= 2) {
			s = new_clef(param)
			if (s)
				get_clef(s)
		}
		return
	case "deco":
		deco_add(param)
		return
	case "linebreak":
		set_linebreak(param)
		return
	case "map":
		get_map(param)
		return
	case "maxsysstaffsep":
	case "sysstaffsep":
		if (parse.state == 3) {
			val = get_unit(param)
			if (isNaN(val)) {
				syntax(1, errs.bad_val, "%%" + cmd)
				return
			}
			par_sy.voices[curvoice.v][cmd[0] == 'm' ? "maxsep" : "sep"] =
					val
			return
		}
		break
	case "multicol":
		if (parse.state >= 2) {
			curvoice = voice_tb[0]
			s = new_block("mc_" + param)
			break
		}
		switch (param) {
		case "start":
			multicol = {
				maxy: 0,
				lmarg: cfmt.leftmargin,
				rmarg: cfmt.rightmargin
			}
			break
		case "new":
			if (!multicol) {
				syntax(1, "%%multicol new without start")
				break
			}
			if (posy > multicol.maxy)
				multicol.maxy = posy;
			cfmt.leftmargin = multicol.lmarg;
			cfmt.rightmargin = multicol.rmarg;
			img.chg = true;
			set_page();
			posy = 0
			break
		case "end":
			if (!multicol) {
				syntax(1, "%%multicol end without start")
				break
			}
			if (posy < multicol.maxy)
				posy = multicol.maxy;
			cfmt.leftmargin = multicol.lmarg;
			cfmt.rightmargin = multicol.rmarg;
			multicol = undefined;
			blk_flush();
			img.chg = true;
			set_page()
			break
		default:
			syntax(1, "Unknown keyword '$1' in %%multicol", param)
			break
		}
		return
	case "ottava":
		if (parse.state != 3)
			return
		n = parseInt(param)
		if (isNaN(n) || n < -2 || n > 2) {
			syntax(1, errs.bad_val, "%%ottava")
			return
		}
		glovar.ottava = true;
		parse.ottava.push(n)
		return
	case "repbra":
		if (curvoice)
			curvoice.norepbra = !get_bool(param)
		return
	case "repeat":
		if (parse.state != 3)
			return
		if (!curvoice.last_sym) {
			syntax(1, "%%repeat cannot start a tune")
			return
		}
		if (!param.length) {
			n = 1;
			k = 1
		} else {
			b = param.split(/\s+/);
			n = parseInt(b[0]);
			k = parseInt(b[1])
			if (isNaN(n) || n < 1
			 || (curvoice.last_sym.type == C.BAR
			  && n > 2)) {
				syntax(1, "Incorrect 1st value in %%repeat")
				return
			}
			if (isNaN(k)) {
				k = 1
			} else {
				if (k < 1) {
					syntax(1, "Incorrect 2nd value in %%repeat")
					return
				}
			}
		}
		parse.repeat_n = curvoice.last_sym.type == C.BAR ? n : -n;
		parse.repeat_k = k
		return
	case "sep":
		var	h2, len, values, lwidth;

		set_page();
		lwidth = img.width - img.lm - img.rm;
		h1 = h2 = len = 0
		if (param) {
			values = param.split(/\s+/);
			h1 = get_unit(values[0])
			if (values[1]) {
				h2 = get_unit(values[1])
				if (values[2])
					len = get_unit(values[2])
			}
			if (isNaN(h1) || isNaN(h2) || isNaN(len)) {
				syntax(1, errs.bad_val, "%%sep")
				return
			}
		}
		if (h1 < 1)
			h1 = 14
		if (h2 < 1)
			h2 = h1
		if (len < 1)
			len = 90
		if (parse.state >= 2) {
			s = new_block(cmd);
			s.x = (lwidth - len) / 2 / cfmt.scale;
			s.l = len / cfmt.scale;
			s.sk1 = h1;
			s.sk2 = h2
			return
		}
		vskip(h1);
		output += '<path class="stroke"\n\td="M';
		out_sxsy((lwidth - len) / 2 / cfmt.scale, ' ', 0);
		output += 'h' + (len / cfmt.scale).toFixed(1) + '"/>\n';
		vskip(h2);
		blk_flush()
		return
	case "setbarnb":
		val = parseInt(param)
		if (isNaN(val) || val < 1) {
			syntax(1, "Bad %%setbarnb value")
			break
		}
		glovar.new_nbar = val
		return
	case "staff":
		if (parse.state != 3)
			return
		val = parseInt(param)
		if (isNaN(val)) {
			syntax(1, "Bad %%staff value '$1'", param)
			return
		}
		var st
		if (param[0] == '+' || param[0] == '-')
			st = curvoice.cst + val
		else
			st = val - 1
		if (st < 0 || st > nstaff) {
			syntax(1, "Bad %%staff number $1 (cur $2, max $3)",
					st, curvoice.cst, nstaff)
			return
		}
		delete curvoice.floating;
		curvoice.cst = st
		return
	case "staffbreak":
		if (parse.state != 3)
			return
		s = {
			type: C.STBRK,
			dur:0
		}
		if (param.slice(-1) == 'f') {
			s.stbrk_forced = true
			param = param.replace(/\sf$/, '')
		}
		if (param) {
			val = get_unit(param)
			if (isNaN(val)) {
				syntax(1, errs.bad_val, "%%staffbreak")
				return
			}
			s.xmx = val
		} else {
			s.xmx = 14
		}
		sym_link(s)
		return
	case "stafflines":
	case "staffscale":
	case "staffnonote":
		set_v_param(cmd, param)
		return
	case "staves":
	case "score":
		if (!parse.state)
			return
		if (parse.scores && parse.scores.length > 0) {
			text = parse.scores.shift();
			cmd = text.match(/([^\s]+)\s*(.*)/);
			param = cmd[2]
			cmd = cmd[1]
		}
		get_staves(cmd, param)
		return
	case "center":
	case "text":
		k = cmd[0] == 'c' ? 'c' : cfmt.textoption
		set_font("text")
		if (parse.state >= 2) {
			s = new_block("text")
			s.text = param
			s.opt = k
			s.font = cfmt.textfont
			return
		}
		write_text(param, k)
		return
	case "transpose":		// (abcm2ps compatibility)
		if (cfmt.sound)
			return
		val = get_transp(param)
		if (val == undefined) {		// accept note interval
			val = get_interval(param)
			if (val == undefined)
				return
		}
		switch (parse.state) {
		case 0:
			cfmt.transp = 0
			// fall thru
		case 1:
			cfmt.transp = (cfmt.transp || 0) + val
			return
		}
		for (s = curvoice.last_sym; s; s = s.prev) {
			switch (s.type) {
			case C.NOTE:		// insert a key
				s = clone(curvoice.okey);
				s.k_old_sf = curvoice.ckey.k_sf;
				sym_link(s)
				break
			case C.KEY:
				break
			default:
				continue
			}
			break
		}
		curvoice.transp = val
		set_transp()
		return
	case "tune":
//fixme: to do
		return
	case "user":
		set_user(param)
		return
	case "voicecolor":
		if (curvoice)
			curvoice.color = param
		return
	case "vskip":
		val = get_unit(param)
		if (isNaN(val)) {
			syntax(1, errs.bad_val, "%%vskip")
			return
		}
		if (val < 0) {
			syntax(1, "%%vskip cannot be negative")
			return
		}
		if (parse.state >= 2) {
			s = new_block(cmd);
			s.sk = val
			return
		}
		vskip(val);
		return
	case "newpage":
	case "leftmargin":
	case "rightmargin":
	case "pagescale":
	case "pagewidth":
	case "printmargin":
	case "scale":
	case "staffwidth":
		if (parse.state >= 2) {
			s = new_block(cmd);
			s.param = param
			return
		}
		if (cmd == "newpage") {
			blk_flush()
			if (user.page_format)
				blkdiv = 2	// start the next SVG in a new page
			return
		}
		break
	}
	self.set_format(cmd, param)
}

// treat the %%beginxxx / %%endxxx sequences
// (possible hook)
Abc.prototype.do_begin_end = function(type,
			opt,
			text) {
	var i, j, action, s

	switch (type) {
	case "js":
		js_inject(text)
		break
	case "ml":
		if (cfmt.pageheight) {
			syntax(1, "Cannot have %%beginml with %%pageheight")
			break
		}
		if (parse.state >= 2) {
			s = new_block(type);
			s.text = text
		} else {
			blk_flush()
			if (user.img_out)
				user.img_out(text)
		}
		break
	case "svg":
		j = 0
		while (1) {
			i = text.indexOf('<style', j)
			if (i < 0)
				break
			i = text.indexOf('>', i)
			j = text.indexOf('</style>', i)
			if (j < 0) {
				syntax(1, "No </style> in %%beginsvg sequence")
				break
			}
			style += text.slice(i + 1, j).replace(/\s+$/, '')
		}
		j = 0
		while (1) {
			i = text.indexOf('<defs>\n', j)
			if (i < 0)
				break
			j = text.indexOf('</defs>', i)
			if (j < 0) {
				syntax(1, "No </defs> in %%beginsvg sequence")
				break
			}
			defs_add(text.slice(i + 6, j))
		}
		break
	case "text":
		action = get_textopt(opt);
		if (!action)
			action = cfmt.textoption
		set_font("text")
		if (text.indexOf('\\') >= 0)
			text = cnv_escape(text)
		if (parse.state > 1) {
			s = new_block(type);
			s.text = text
			s.opt = action
			s.font = cfmt.textfont
			break
		}
		write_text(text, action)
		break
	}
}

/* -- generate a piece of tune -- */
function generate() {
    var s, v, p_voice;

	if (a_dcn.length) {
		syntax(1, "Decoration without symbol")
		a_dcn = []
	}

	if (parse.tp) {
		syntax(1, "No end of tuplet")
		s = parse.tps
		if (s)
			delete s.tp
		delete parse.tp
	}

	if (vover) {
		syntax(1, "No end of voice overlay");
		get_vover(vover.bar ? '|' : ')')
	}

	voice_adj();
	sort_all()			/* define the time / vertical sequences */

    if (tsfirst) {
	if (user.anno_start)
		anno_start = a_start
	if (user.anno_stop)
		anno_stop = a_stop
	self.set_bar_num()
	pit_adj()

	if (info.P)
		tsfirst.parts = info.P	// for play

	if (parse.ctrl)
		set_ctrl()		// set control values

	// give the parser result to the application
	if (user.get_abcmodel)
		user.get_abcmodel(tsfirst, voice_tb, abc2svg.sym_name, info)

	if (user.img_out)		// if SVG generation
		self.output_music()
    } // (tsfirst)

	// finish the generation
	set_page()			// the page layout may have changed
	if (info.W)
		put_words(info.W)
	put_history()
	parse.state = 0			// file header
	blk_flush()			// (force end of block)

	if (tsfirst) {		// if non void, keep tune data for upper layers
		tunes.push([tsfirst, voice_tb, info, cfmt])
		tsfirst = null
	}
}

// transpose a key
//fixme: transpose of the accidental list is not done
function key_transp(sk) {
	if (sk.k_a_acc || sk.k_none)		// same displayed key
		return
    var	d,
	k_b40 = sk.k_b40,
	n_b40 = (k_b40 + 200 + sk.k_transp) % 40

	d = abc2svg.b40k[n_b40] - n_b40
	if (d) {
		if (sk.k_transp > 0)
			sk.k_transp += d
		else
			sk.k_transp -= d
		n_b40 += d
	}
	sk.k_b40 = n_b40

   var sf = abc2svg.b40sf[n_b40]
	sk.k_old_sf = sk.k_sf
	sk.k_sf = sf
	sk.k_map = abc2svg.keys[sf + 7]	// map of the notes with accidentals
}

/*
 * for transpose purpose, check if a pitch is already in the measure or
 * if it is tied from a previous note, and return the associated accidental
 */
function acc_same_pitch(s, pit) {
    var	i, a,
	time = s.time

	for (s = s.prev; s; s = s.prev) {
		switch (s.type) {
		case C.BAR:
			if (s.time < time)
				return //undefined // no same pitch
			while (1) {
				s = s.prev
				if (!s)
					return //undefined
				if (s.type == C.NOTE) {
					if (s.time + s.dur == time)
						break
					return //undefined
				}
				if (s.time < time)
					return //undefined
			}
			for (i = 0; i <= s.nhd; i++) {
				if (s.notes[i].pit == pit
				 && s.notes[i].tie_ty) {
					a = s.notes[i].acc
					return a == undefined || a == 3
				}
			}
			return //undefined
		case C.NOTE:
			for (i = 0; i <= s.nhd; i++) {
				if (s.notes[i].pit == pit) {
					a = s.notes[i].acc
					return a == undefined || a == 3
				}
			}
			break
		}
	}
	return //undefined
}

/* -- get staves definition (%%staves / %%score) -- */
function get_staves(cmd, parm) {
    var	s, p_voice, p_voice2, i, flags, v, vid, a_vf,
	st, range,
	nv = voice_tb.length,
	maxtime = 0

	if (parm) {
		a_vf = parse_staves(parm)	// => array of [vid, flags]
		if (!a_vf)
			return
	}

	/* create a new staff system */
	for (v = 0; v < nv; v++) {
		p_voice = voice_tb[v]
		if (p_voice.time > maxtime)
			maxtime = p_voice.time
	}
	if (!maxtime) {				// if first %%staves
		par_sy.staves = []
		par_sy.voices = []
	} else {

		if (nv)					// if many voices
			voice_adj(true)
		/*
		 * create a new staff system and
		 * link the 'staves' symbol in a voice which is seen from
		 * the previous system - see sort_all
		 */
		for (v = 0; v < par_sy.voices.length; v++) {
			if (par_sy.voices[v]) {
				curvoice = voice_tb[v]
				break
			}
		}
		curvoice.time = maxtime;
		s = {
			type: C.STAVES,
			dur: 0
		}

		sym_link(s);		// link the staves in this voice
		par_sy.nstaff = nstaff;

		// if no parameter, duplicate the current staff system
		// and do a voice re-synchronization
		if (!parm) {
			s.sy = clone(par_sy, 1)
			par_sy.next = s.sy
			par_sy = s.sy
			staves_found = maxtime
			for (v = 0; v < nv; v++)
				voice_tb[v].time = maxtime
			curvoice = voice_tb[par_sy.top_voice]
			return
		}

		new_syst();
		s.sy = par_sy
	}

	staves_found = maxtime

	/* initialize the (old) voices */
	for (v = 0; v < nv; v++) {
		p_voice = voice_tb[v]
		delete p_voice.second
		delete p_voice.ignore
		delete p_voice.floating
	}
	range = 0
	for (i = 0; i < a_vf.length; i++) {
		vid = a_vf[i][0];
		p_voice = new_voice(vid);
		p_voice.time = maxtime;
		v = p_voice.v

		a_vf[i][0] = p_voice;

		// set the range and add the overlay voices
		while (1) {
			par_sy.voices[v] = {
				range: range++
			}
			p_voice = p_voice.voice_down
			if (!p_voice)
				break
			v = p_voice.v
		}
	}
	par_sy.top_voice = a_vf[0][0].v

	/* change the behavior from %%staves to %%score */
	if (cmd[1] == 't') {				/* if %%staves */
		for (i = 0; i < a_vf.length; i++) {
			flags = a_vf[i][1]
			if (!(flags & (OPEN_BRACE | OPEN_BRACE2)))
				continue
			if ((flags & (OPEN_BRACE | CLOSE_BRACE))
					== (OPEN_BRACE | CLOSE_BRACE)
			 || (flags & (OPEN_BRACE2 | CLOSE_BRACE2))
					== (OPEN_BRACE2 | CLOSE_BRACE2))
				continue
			if (a_vf[i + 1][1] != 0)
				continue
			if ((flags & OPEN_PARENTH)
			 || (a_vf[i + 2][1] & OPEN_PARENTH))
				continue

			/* {a b c} -> {a *b c} */
			if (a_vf[i + 2][1] & (CLOSE_BRACE | CLOSE_BRACE2)) {
				a_vf[i + 1][1] |= FL_VOICE

			/* {a b c d} -> {(a b) (c d)} */
			} else if (a_vf[i + 2][1] == 0
				&& (a_vf[i + 3][1]
					& (CLOSE_BRACE | CLOSE_BRACE2))) {
				a_vf[i][1] |= OPEN_PARENTH;
				a_vf[i + 1][1] |= CLOSE_PARENTH;
				a_vf[i + 2][1] |= OPEN_PARENTH;
				a_vf[i + 3][1] |= CLOSE_PARENTH
			}
		}
	}

	/* set the staff system */
	st = -1
	for (i = 0; i < a_vf.length; i++) {
		flags = a_vf[i][1]
		if ((flags & (OPEN_PARENTH | CLOSE_PARENTH))
				== (OPEN_PARENTH | CLOSE_PARENTH)) {
			flags &= ~(OPEN_PARENTH | CLOSE_PARENTH);
			a_vf[i][1] = flags
		}
		p_voice = a_vf[i][0]
		if (flags & FL_VOICE) {
			p_voice.floating = true;
			p_voice.second = true
		} else {
			st++;
			if (!par_sy.staves[st]) {
				par_sy.staves[st] = {
					stafflines: p_voice.stafflines || "|||||",
					staffscale: 1
				}
			}
			par_sy.staves[st].flags = 0
		}
		v = p_voice.v;
		p_voice.st = p_voice.cst =
				par_sy.voices[v].st = st;
		par_sy.staves[st].flags |= flags
		if (flags & OPEN_PARENTH) {
			p_voice2 = p_voice
			while (i < a_vf.length - 1) {
				p_voice = a_vf[++i][0];
				v = p_voice.v
				if (a_vf[i][1] & MASTER_VOICE) {
					p_voice2.second = true
					p_voice2 = p_voice
				} else {
					p_voice.second = true;
				}
				p_voice.st = p_voice.cst
						= par_sy.voices[v].st
						= st
				if (a_vf[i][1] & CLOSE_PARENTH)
					break
			}
			par_sy.staves[st].flags |= a_vf[i][1]
		}
	}
	if (st < 0)
		st = 0
	par_sy.nstaff = nstaff = st

	/* change the behaviour of '|' in %%score */
	if (cmd[1] == 'c') {				/* if %%score */
		for (st = 0; st < nstaff; st++)
			par_sy.staves[st].flags ^= STOP_BAR
	}

	nv = voice_tb.length
	st = 0
	for (v = 0; v < nv; v++) {
		p_voice = voice_tb[v]
		if (par_sy.voices[v])
			st = p_voice.st
		else
			p_voice.st = st	// (this avoids later crashes)

		// if first %%staves
		// update the staff of the symbols with no time
		if (!maxtime) {
			for (s = p_voice.sym; s; s = s.next)
				s.st = st
		}

		if (!par_sy.voices[v])
			continue

		// set the staff of the overlay voices
		p_voice2 = p_voice.voice_down
		while (p_voice2) {
			i = p_voice2.v
			p_voice2.st = p_voice2.cst =
					par_sy.voices[i].st = st
			p_voice2 = p_voice2.voice_down
		}

		par_sy.voices[v].second = p_voice.second;
		st = p_voice.st
		if (st > 0 && !p_voice.norepbra
		 && !(par_sy.staves[st - 1].flags & STOP_BAR))
			p_voice.norepbra = true
	}
	if (!maxtime && nv)		// if first staff system and many voices
		voice_adj(true)

	curvoice = parse.state >= 2 ? voice_tb[par_sy.top_voice] : null
}

	// get a voice or create a clone of the current voice
	function clone_voice(id) {
		var v, p_voice

		for (v = 0; v < voice_tb.length; v++) {
			p_voice = voice_tb[v]
			if (p_voice.id == id)
				return p_voice		// found
		}
		p_voice = clone(curvoice);
		p_voice.v = voice_tb.length;
		p_voice.id = id;
		p_voice.sym = p_voice.last_sym = null;

		p_voice.key = clone(curvoice.key)
		p_voice.sls = []

		delete p_voice.nm
		delete p_voice.snm
		delete p_voice.new_name
		delete p_voice.lyric_restart
		delete p_voice.lyric_cont
		delete p_voice.sym_restart
		delete p_voice.sym_cont
		delete p_voice.have_ly
		delete p_voice.tie_s

		voice_tb.push(p_voice)
		return p_voice
	} // clone_voice()

/* -- get a voice overlay -- */
function get_vover(type) {
    var	p_voice2, p_voice3, range, s, time, v, v2, v3, s2

	/* treat the end of overlay */
	if (type == '|'
	 || type == ')')  {
		if (!curvoice.last_note) {
			syntax(1, errs.nonote_vo)
			return
		}
		curvoice.last_note.beam_end = true
		if (!vover) {
			syntax(1, "Erroneous end of voice overlay")
			return
		}
		if (curvoice.time != vover.p_voice.time) {
			syntax(1, "Wrong duration in voice overlay");
			if (curvoice.time > vover.p_voice.time)
				vover.p_voice.time = curvoice.time
		}
		curvoice.acc = []		// no accidental anymore

		// if the last symbols are spaces, move them to the main voice
		p_voice2 = vover.p_voice	// main voice
		s = curvoice.last_sym
		if (s.type == C.SPACE && p_voice2.last_sym.type != C.SPACE) {
			s.p_v = p_voice2
			s.v = s.p_v.v
			while (s.prev.type == C.SPACE) {
				s = s.prev
				s.p_v = p_voice2
				s.v = s.p_v.v
			}
			s2 = s.prev
			s2.next = null
			s.prev = p_voice2.last_sym
			s.prev.next = s
			p_voice2.last_sym = curvoice.last_sym
			curvoice.last_sym = s2
		}

		curvoice = p_voice2
		vover = null
		return
	}

	/* treat the full overlay start */
	if (type == '(') {
		if (vover) {
			syntax(1, "Voice overlay already started")
			return
		}
		vover = {
			p_voice: curvoice,
			time: curvoice.time
		}
		return
	}

	/* (here is treated a new overlay - '&') */
	/* create the extra voice if not done yet */
	if (!curvoice.last_note) {
		syntax(1, errs.nonote_vo)
		return
	}
	curvoice.last_note.beam_end = true;
	p_voice2 = curvoice.voice_down
	if (!p_voice2) {
		p_voice2 = clone_voice(curvoice.id + 'o');
		curvoice.voice_down = p_voice2;
		p_voice2.time = 0;
		p_voice2.second = true;
		v2 = p_voice2.v;
		par_sy.voices[v2] = {
			st: curvoice.st,
			second: true
		}
		range = par_sy.voices[curvoice.v].range
		for (v = 0; v < par_sy.voices.length; v++) {
			if (par_sy.voices[v]
			 && par_sy.voices[v].range > range)
				par_sy.voices[v].range++
		}
		par_sy.voices[v2].range = range + 1
	}
	p_voice2.ulen = curvoice.ulen
	p_voice2.dur_fact = curvoice.dur_fact
	p_voice2.acc = []			// no accidental

	if (!vover) {				/* first '&' in a measure */
		time = p_voice2.time
		for (s = curvoice.last_sym; /*s*/; s = s.prev) {
			if (s.type == C.BAR
			 || s.time <= time)	/* (if start of tune) */
				break
		}
		vover = {
			bar: (s && s.bar_type) ? s.bar_type : '|',
			p_voice: curvoice,
			time: s.time
		}
	} else {
		if (curvoice != vover.p_voice
		 && curvoice.time != vover.p_voice.time) {
			syntax(1, "Wrong duration in voice overlay")
			if (curvoice.time > vover.p_voice.time)
				vover.p_voice.time = curvoice.time
		}
	}
	p_voice2.time = vover.time;
	curvoice = p_voice2
}

// check if a clef, key or time signature may go at start of the current voice
function is_voice_sig() {
	var s

	if (!curvoice.last_sym)
		return true
	if (curvoice.time)
		return false
	for (s = curvoice.last_sym; s; s = s.prev)
		if (w_tb[s.type])
			return false
	return true
}

// treat a clef found in the tune body
function get_clef(s) {
	if (!curvoice.time		// (force a clef when new voice)
	 && is_voice_sig()) {
		curvoice.clef = s
		s.fmt = cfmt
		return
	}

	// clef change
	sym_link(s);
	s.clef_small = true

	// move the clef before a (not right repeat) bar
    var	s2 = s.prev
	if (s2 && s2.type == C.BAR
	 && s2.bar_type[0] != ':') {
		s.next = s2
		s.prev = s2.prev
		if (s.prev)
			s.prev.next = s
		s2.prev = s
		s2.next = null
		curvoice.last_sym = s2
		if (s.soln) {
			delete s.soln
			curvoice.eoln = true
		}
	}
}

// treat K: (kp = key signature + parameters)
function get_key(parm) {
	var	v, p_voice, s, transp, sndtran,
//		[s_key, a] = new_key(parm)	// KO with nodejs
		a = new_key(parm),
		s_key = a[0];

	a = a[1]

	if (parse.state == 1) {		// in tune header (first K:)
		if (s_key.k_sf == undefined && !s_key.k_a_acc) { // empty K:
			s_key.k_sf = 0;
			s_key.k_none = true
			s_key.k_map = abc2svg.keys[7]
		}
		for (v = 0; v < voice_tb.length; v++) {
			p_voice = voice_tb[v];
			p_voice.key = clone(s_key);
			p_voice.okey = clone(s_key);
			p_voice.ckey = clone(s_key)
		}
		parse.ckey = s_key
		if (!glovar.ulen)
			glovar.ulen = C.BLEN / 8;
		if (a.length) {
			memo_kv_parm('*', a)
			a = ''
		}
		goto_tune()
		parse.state = 3			// in tune body
	}

	set_kv_parm(a)

	if (!curvoice.ckey.k_bagpipe && !curvoice.ckey.k_drum
	 && (cfmt.transp != undefined
	  || curvoice.transp != undefined
	  || curvoice.shift != undefined))
	    transp = (cfmt.transp || 0) +
		(curvoice.transp || 0) +
		(curvoice.shift || 0)
	if (curvoice.sndtran != undefined
	 || curvoice.sndsh != undefined)
		sndtran = (curvoice.sndtran || 0) +
			(curvoice.sndsh || 0)

	if (s_key.k_sf == undefined) {
		if (!s_key.k_a_acc
		 && transp == undefined) {
			if (sndtran == undefined)
				return		// not a key signature
			s_key.invis = true	// play only
		}
		s_key.k_sf = curvoice.okey.k_sf
	}

	curvoice.okey = clone(s_key)
	if (transp != undefined) {
		curvoice.vtransp = transp;
		s_key.k_transp = transp
	}
	if (sndtran != undefined)
		s_key.k_sndtran = sndtran

	s_key.k_old_sf = curvoice.ckey.k_sf;	// memorize the key changes

	if ((!s_key.k_a_acc || !s_key.k_a_acc.length)
	 && !s_key.k_sf && !s_key.k_old_sf)
		s_key.invis = true		// don't display the key signature

	if (!s_key.k_b40)
		s_key.k_b40 = curvoice.ckey.k_b40

	curvoice.ckey = s_key

	if (is_voice_sig()) {
		s_key.fmt = cfmt
		curvoice.key = clone(s_key)
		if (s_key.k_none)
			curvoice.key.k_sf = 0
	} else {
		sym_link(s_key)			// (don't move the key)
	}
}

// get / create a new voice
function new_voice(id) {
    var	v, p_v_sav,
	p_voice = voice_tb[0],
	n = voice_tb.length

	// if first explicit voice and no music, replace the default V:1
	if (n == 1
	 && p_voice.default) {
		delete p_voice.default
		if (!p_voice.time) {		// if no symbol yet
			p_voice.id = id
			delete p_voice.init	// reload the global attributes
			if (cfmt.transp	// != undefined
			 && parse.state >= 2) {
				p_v_sav = curvoice;
				curvoice = p_voice;
				set_transp();
				curvoice = p_v_sav
			}
			return p_voice		// default voice
		}
	}
	for (v = 0; v < n; v++) {
		p_voice = voice_tb[v]
		if (p_voice.id == id)
			return p_voice		// old voice
	}

	p_voice = {
		v: v,
		id: id,
		time: 0,
		new: true,
		pos: {
//			dyn: 0,
//			gch: 0,
//			gst: 0,
//			orn: 0,
//			stm: 0,
//			tup: 0,
//			voc: 0,
//			vol: 0
		},
		scale: 1,
//		st: 0,
//		cst: 0,
		ulen: glovar.ulen,
		dur_fact: 1,
		key: clone(parse.ckey),		// key at start of tune (parse / gene)
		ckey: clone(parse.ckey),	// current key (parse / gene)
		okey: clone(parse.ckey),	// key without transposition (parse)
		meter: clone(glovar.meter),
		wmeasure: glovar.meter.wmeasure,
		staffnonote: 1,
		clef: {
			type: C.CLEF,
			clef_auto: true,
			clef_type: "a",		// auto
			time: 0
		},
		acc: [],		// accidentals of the measure (parse)
		sls: [],		// slurs - used in parsing and in generation
		hy_st: 0
	}

	voice_tb.push(p_voice);

//	par_sy.voices[v] = {
//		range: -1
//	}

	return p_voice
}

// this function is called at program start and on end of tune
function init_tune() {
	nstaff = -1;
	voice_tb = [];
	curvoice = null;
	new_syst(true);
	staves_found = -1;
	gene = {}
	a_de = []			// remove old decorations
	cross = {}			// new cross voice decorations
}

// treat V: with many voices
function do_cloning() {
    var	i,
	clone = curvoice.clone,
	vs = clone.vs,
	a = clone.a,
	bol = clone.bol,
	eol = parse.istart,
	parse_sav = parse,
	file = parse.file

	delete curvoice.clone

	if (file[eol - 1] == '[')	// if stop on [V:xx]
		eol--

	// insert the music sequence in each voice
	include++;
	for (i = 0; i < vs.length; i++) {
		parse = Object.create(parse_sav) // create a new parse context
		parse.line = Object.create(parse_sav.line)
		get_voice(vs[i], a)
		tosvg(parse.fname, file, bol, eol)
	}
	include--
	parse = parse_sav	// restore the parse context
}

// treat a 'V:' info
function get_voice(parm) {
    var	v, vs,
	a = info_split(parm),
	vid = a.shift()

	if (!vid)
		return				// empty V:

	// if end of sequence with many voices, load the other voices
	if (curvoice && curvoice.clone)
		do_cloning()

	if (vid.indexOf(',') > 0)		// if many voices
		vs = vid.split(',')
	else
		vs = [vid]

	if (parse.state < 2) {			// memorize the voice parameters
		while (1) {
			vid = vs.shift()
			if (!vid)
				break
			if (a.length)
				memo_kv_parm(vid, a)
			if (vid != '*' && parse.state == 1)
				curvoice = new_voice(vid)
		}
		return
	}

	if (vid == '*') {
		syntax(1, "Cannot have V:* in tune body")
		return
	}

	curvoice = new_voice(vs[0])

	// if many voices, memorize the start of sequence
	if (vs.length > 1) {
		vs.shift()
		curvoice.clone = {
			vs: vs,
			a: a,
			bol: parse.iend
		}
		if (parse.file[curvoice.clone.bol - 1] != ']')
			curvoice.clone.bol++	// start of new line
	}

	set_kv_parm(a)

	v = curvoice.v
	if (curvoice.new) {			// if new voice
		delete curvoice.new
		if (staves_found < 0) {		// if no %%score/%%staves
			curvoice.st = curvoice.cst = ++nstaff;
			par_sy.nstaff = nstaff;
			par_sy.voices[v] = {
				st: nstaff,
				range: v
			}
			par_sy.staves[nstaff] = {
				stafflines: curvoice.stafflines || "|||||",
				staffscale: 1
			}
		} else if (!par_sy.voices[v]) {
			curvoice.ignore = 1	// voice not declared in %%staves
			return
		}
		set_transp()
	}

	if (!curvoice.filtered
	 && par_sy.voices[v]
	 && (parse.voice_opts
	  || parse.tune_v_opts)) {
		curvoice.filtered = true;
		voice_filter()
	}
}

// change state from 'tune header' to 'in tune body'
// curvoice is defined when called from get_voice()
function goto_tune() {
    var	v, p_voice

	set_page();
	write_heading();
	if (glovar.new_nbar) {
		gene.nbar = glovar.new_nbar	// measure numbering
		glovar.new_nbar = 0
	} else {
		gene.nbar = 1
	}

	// if no voice yet, create the default voice
	if (!voice_tb.length) {
		get_voice("1");
		curvoice.clef.istart = curvoice.key.istart;
		curvoice.clef.iend = curvoice.key.iend;
//		nstaff = 0;
		curvoice.default = true
	} else {
		curvoice = voice_tb[staves_found < 0 ? 0 : par_sy.top_voice]
	}

	// update some voice parameters
	for (v = 0; v < voice_tb.length; v++) {
		p_voice = voice_tb[v];
		p_voice.ulen = glovar.ulen
		if (p_voice.ckey.k_bagpipe
		 && !p_voice.pos.stm) {
			p_voice.pos = clone(p_voice.pos)
			p_voice.pos.stm &= ~0x07
			p_voice.pos.stm |= C.SL_BELOW
		}
	}

	// initialize the voices when no %%staves/score	
	if (staves_found < 0) {
		v = voice_tb.length
		par_sy.nstaff =
			nstaff = v - 1
		while (--v >= 0) {
			p_voice = voice_tb[v];
			delete p_voice.new;		// old voice
			p_voice.st = p_voice.cst = v;
			par_sy.voices[v] = {
				st: v,
				range: v
			}
			par_sy.staves[v] = {
				stafflines: p_voice.stafflines || "|||||",
				staffscale: 1
			}
		}
	}
}
