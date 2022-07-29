// sndgen.js - sound generation
//
// Copyright (C) 2019-2021 Jean-Francois Moine
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

// This script generates the play data which are stored in the music symbols:
// - in all symbols
//	s.ptim = play time
// - in BAR
//	rep_p = on a right repeat bar, pointer to the left repeat symbol
//	rep_s = on the first repeat variant, array of pointers to the next symbols,
//						indexed by the repeat number
// - in NOTE and REST
//	s.pdur = play duration
//	s.instr = bank + instrument
//	s.chn = MIDI channel
// - in the notes[] of NOTE
//	s.notes[i].midi

if (!abc2svg)
    var	abc2svg = {}

function ToAudio() {
 return {

   // generate the play data of a tune
   add: function(first,		// starting symbol
		voice_tb,	// voice table
		cfmt) {		// tune parameters
    var	toaud = this,
	C = abc2svg.C,
	p_time = 0,		// last playing time
	abc_time = 0,		// last ABC time
	play_fac = C.BLEN / 4 * 120 / 60, // play time factor - default: Q:1/4=120
	i, n, dt, d, v, c,
	s = first,
	rst = s,		// left repeat (repeat restart)
	rst_fac,		// play factor on repeat restart
	rsk,			// repeat variant array (repeat skip)
	b_tim,			// time of last measure bar
	b_typ,			// type of last measure bar
	instr = [],		// [voice] bank + instrument
	chn = []		// [voice] MIDI channel

	// build the information about the parts (P:)
	function build_parts(first) {
	    var	i, j, c, n, v,
		s = first,
		p = s.parts,
		st = [],
		r = ""

		// build a linear string of the parts
		for (i = 0; i < p.length; i++) {
			c = p[i]
			switch (c) {
			case '.':
				continue
			case '(':
				st.push(r.length)
				continue
			case ')':
				j = st.pop()
				if (j == undefined)
					j = r.length
				continue
			}
			if (c >= 'A' && c <= 'Z') {
				j = r.length
				r += c
				continue
			}
			n = Number(c)
//fixme:one digit is enough!
//			while (1) {
//				c = p[i + 1]
//				if (c < '0' || c > '9')
//					break
//				n = n * 10 + Number(c)
//				i++
//			}
			if (isNaN(n))
				break
			v = r.slice(j)
			if (r.length + v.length * n > 128)
				continue
			while (--n > 0)
				r += v
		}
		s.parts = r

		// build the part table in the first symbol
		// and put the reverse pointers in the P: symbols
		s.p_s = []			// pointers to the parts
		while (1) {
			if (!s.ts_next) {
				s.part1 = first	// end of tune = end of part
				break
			}
			s = s.ts_next
			if (s.part) {
				s.part1 = first		// reverse pointer
				v = s.part.text[0]	// 1st letter only
				for (i = 0; i < first.parts.length; i++) {
					if (first.parts[i] == v)
						first.p_s[i] = s
				}
			}
		}
	} // build_parts()

	// set the starting MIDI instruments and channels
	function midi_start() {
	    var	v, p_v, c, i, ii

		for (v = 0; v < voice_tb.length; v++) {
			p_v = voice_tb[v]
			ii = p_v.instr || 0		// instrument
			c = p_v.chn			// channel
			if (c == undefined)
				c = p_v.v < 9 ? p_v.v : p_v.v + 1
			else if (c == 9)		// percussion
				ii = (ii & ~0x7f) | 16384

			if (p_v.midictl) {		// if MIDI controls
				for (i in p_v.midictl) {
					switch (Number(i)) {
					case 0:		// MSB bank
						ii = (ii & 0x3fff) |
							(p_v.midictl[i] << 14)
						break
					case 32:	// LSB bank
						ii = (ii & 0x1fc07f) |
							(p_v.midictl[i] << 7)
						break
					}
				}
			}

			if ((ii & ~0x7f) == 16384) // if bank 128 (percussion)
				c = 9			// channel '10'
			chn[v] = c
			instr[c] = ii
		}
	} // midi_start()

	// handle a block symbol
	function do_block(s) {
	    var	v = s.v,
		p_v = s.p_v,
		c = chn[v],
		co = c

		switch (s.subtype) {
		case "midichn":
			break
		case "midictl":
			switch (s.ctrl) {
			case 0:			// MSB bank
				instr[c] = (instr[c] & 0x3fff) |
					(s.val << 14)
				break
			case 32:		// LSB bank
				instr[c] = (instr[c] & 0x1fc07f) |
					(s.val << 7)
				break
//			case 121:		// reset all controllers
//				instr = []
//				break
			}
			if ((instr[c] & ~0x7f) == 16384) { // if percussion
				instr[9] = instr[c]	// force the channel 10
				chn[v] = c = 9
			}
			s.chn = c
			// fall thru
		default:
			return
		case "midiprog":
			instr[c] = (instr[c] & ~0x7f) | s.instr
			break
		}
		if (s.chn == undefined)
			return			// same channel

		// update the channel of the voice
		// and the one of the overlay voices
		chn[v] = s.chn
		while (1) {
			p_v = p_v.voice_down
			if (!p_v)
				break
			v = p_v.v
			if (chn[v] == undefined || chn[v] == co)
				chn[v] = s.chn
		}
	} // do_block()

	// generate the grace notes
	function gen_grace(s) {
	    var	g, i, n, t, d, s2,
		next = s.next

		// before beat
		if (s.sappo) {
			d = C.BLEN / 16
		} else if ((!next || next.type != C.NOTE)
			&& s.prev && s.prev.type == C.NOTE) {
			d = s.prev.dur / 2

		// on beat
		} else {
			d = next.dur / 12
			if (!(d & (d - 1)))
				d = next.dur / 2	// no dot
			else
				d = next.dur / 3
			if (s.p_v.key.k_bagpipe)
				d /= 2
			next.time += d
			next.dur -= d
		}
//fixme: assume the grace notes in the sequence have the same duration
		n = 0
		for (g = s.extra; g; g = g.next)
			n++
		d /= n * play_fac
		t = p_time
		for (g = s.extra; g; g = g.next) {
			g.ptim = t
			g.pdur = d
			g.chn = chn[s.v]
			g.instr = instr[g.chn]
			t += d
		}
	} // gen_grace()

	// change the tempo
	function set_tempo(s) {
	    var	i,
		d = 0,
		n = s.tempo_notes.length

		for (i = 0; i < n; i++)
			d += s.tempo_notes[i]
		return d * s.tempo / 60
	} // set_tempo()

	function set_variant(rsk, n, s) {
	    var	d
		n = n.match(/[1-8]-[2-9]|[1-9,.]|[^\s]+$/g)
		while (1) {
			d = n.shift()
			if (!d)
				break
			if (d[1] == '-')
				for (i = d[0]; i <= d[2]; i++)
					rsk[i] = s
			else if (d >= '1' && d <= '9')
				rsk[Number(d)] = s
			else if (d != ',')
				rsk.push(s)	// last
		}
	} // set_variant()

	// add() main

	// if some MIDI stuff, load the associated module
	if (cfmt.chord) {
		if (!abc2svg.chord) {
			abc2svg.pwait = true	// don't start playing now

			abc2svg.loadjs("chord-1.js",
					function(){	// ok
						toaud.add(first, voice_tb, cfmt)
					},
					function(){	// KO
						cfmt.chord = null
						toaud.add(first, voice_tb, cfmt)
					})
			return
		}
		abc2svg.chord(first, voice_tb, cfmt)
	}

	if (s.parts)
		build_parts(s)

	// get the starting MIDI parameters
	midi_start()

	// set the time parameters
	rst_fac = play_fac
	while (s) {
		if (s.noplay) {			// in display macro sequence
			s = s.ts_next
			continue
		}

		dt = s.time - abc_time
		if (dt != 0) {		// may go backwards after grace notes
			p_time += dt / play_fac
			abc_time = s.time
		}
		s.ptim = p_time

		if (s.part) {			// new part
			rst = s			// new possible restart
			rst_fac = play_fac
		}
		switch (s.type) {
		case C.BAR:
			if (s.time != b_tim) {
				b_tim = s.time
				b_typ = 0
			}
			if (s.text && rsk		// if new variant
			 && s.text[0] != '1') {
				if (b_typ & 1)
					break
				b_typ |= 1
				set_variant(rsk, s.text, s)
				play_fac = rst_fac
				rst = rsk[0]		// reinit the restart
			}

			// right repeat
			if (s.bar_type[0] == ':') {
				if (b_typ & 2)
					break
				b_typ |= 2
				s.rep_p = rst		// :| to |:
				if (rsk && rst == rsk[0])
					s.rep_v = rsk	// to know the number of variants
			}

			// 1st time repeat
			if (s.text) {
			    if (s.text[0] == '1') {
				if (b_typ & 1)
					break
				b_typ |= 1
				s.rep_s = rsk = [rst]	// repeat skip
							// and memorize the restart
				if (rst.bar_type
				 && rst.bar_type.slice(-1) != ':')
					rst.bar_type += ':' // restart confirmed
				set_variant(rsk, s.text, s)
				rst_fac = play_fac
			    }

			// left repeat
			} else if (s.rbstop) {
				if (s.bar_type.slice(-1) == ':') {
					if (b_typ & 4)
						break
					b_typ |= 4
				} else {
					if (b_typ & 8)
						break
					b_typ |= 8
				}
				rst = s			// new possible restart
				rst_fac = play_fac
			}
			break
		case C.BLOCK:
			do_block(s)
			break
		case C.GRACE:
			if (s.time == 0		// if before beat at start time
			 && abc_time == 0) {
				dt = 0
				if (s.sappo)
					dt = C.BLEN / 16
				else if (!s.next || s.next.type != C.NOTE)
					dt = d / 2
				abc_time -= dt
			}
			gen_grace(s)
			break
		case C.REST:
		case C.NOTE:
			d = s.dur
			if (s.next && s.next.type == C.GRACE) {
				dt = 0
				if (s.next.sappo)
					dt = C.BLEN / 16
				else if (!s.next.next || s.next.next.type != C.NOTE)
					dt = d / 2
				s.next.time -= dt
				d -= dt
			}
			d /= play_fac
			s.pdur = d
			v = s.v
			c = chn[v]			// channel
			s.chn = c
			s.instr = instr[c]
			break
		case C.TEMPO:
			if (s.tempo)
				play_fac = set_tempo(s)
			break
		}
		s = s.ts_next
	} // loop

	// if playing was waiting for a resource, start it now
	if (abc2svg.pwait) {
		i = abc2svg.pwait
		delete abc2svg.pwait
		if (typeof i == "function")
			i()
	}
   } // add()
 } // return
} // ToAudio()

// play some next symbols
//
// This function is called to start playing.
// Playing is stopped on either
// - reaching the 'end' symbol (not played) or
// - reaching the end of tune or
// - seeing the 'stop' flag (user request).
//
// The po object (Play Object) contains the following items:
// - variables
//  - stop: stop flag
//		set by the user to stop playing
//  - s_cur: current symbol (next to play)
//		must be set to the first symbol to be played at startup time
//  - s_end: stop playing on this symbol
//		this symbol is not played. It may be null.
//  - conf
//    - speed: current speed factor
//		must be set to 1 at startup time
//    - new_conf: new speed factor
//		set by the user
// - internal variables
//  - stim: start time
//  - repn: don't repeat
//  - repv: variant number
//  - timouts: array of the current timeouts
//		this array may be used by the upper function in case of hard stop
//  - p_v: voice table used for MIDI control
// - methods
//  - onend: (optional)
//  - onnote: (optional)
//  - note_run: start playing a note
//  - get_time: return the time of the underlaying sound system
abc2svg.play_next = function(po) {

	// handle a tie
	function do_tie(not_s, d) {
	    var	i,
		s = not_s.s,
		C = abc2svg.C,
		v = s.v,
		end_time = s.time + s.dur,
		repv = po.repv

		// search the end of the tie
		while (1) {
			s = s.ts_next
			if (!s || s.time > end_time)
				break
			if (s.type == C.BAR) {
				if (s.rep_p) {
					if (!po.repn) {
						s = s.rep_p
						end_time = s.time
					}
				}
				if (s.rep_s) {
					if (!s.rep_s[repv])
						break
					s = s.rep_s[repv++]
					end_time = s.time
				}
				while (s.ts_next && !s.ts_next.dur)
					s = s.ts_next
				continue
			}
			if (s.time < end_time
			 || !s.ti2)			// if not end of tie
				continue

			i = s.notes.length
			while (--i >= 0) {
				note = s.notes[i]
				if (note.tie_s == not_s) {
					d += s.pdur / po.conf.speed
					return note.tie_e ? do_tie(note, d) : d
				}
			}
		}

		return d
	} // do_tie()

	// set the MIDI controls up to now
	function set_ctrl(po, s2, t) {
	    var	i,
		p_v = s2.p_v,
		tim = s2.time,
		s = {
			subtype: "midictl",
			p_v: p_v,
			v: p_v.v,
			chn: p_v.chn
		}

		for (i in p_v.midictl) { // MIDI controls at voice start time
			s.ctrl = Number(i)
			s.val = p_v.midictl[i]
			po.midi_ctrl(po, s, t)
		}
		for (s = p_v.sym; s && s.time <= tim; s = s.next) {
			if (s.subtype == "midictl")
				po.midi_ctrl(po, s, t)
		}
		po.p_v[s2.v] = true	// synchronization done
	} // set_ctrl()

    // start and continue to play
    function play_cont(po) {
    var	d, i, st, m, note, g, s2, t, maxt, now,
	C = abc2svg.C,
	s = po.s_cur

	if (po.stop) {
		if (po.onend)
			po.onend(po.repv)
		return
	}

	while (s.noplay) {
		s = s.ts_next
		if (!s || s == po.s_end) {
			if (po.onend)
				po.onend(po.repv)
			return
		}
	}
	t = po.stim + s.ptim / po.conf.speed	// start time
	now = po.get_time(po)

	// if speed change, shift the start time
	if (po.conf.new_speed) {
		po.stim = now - (now - po.stim) *
					po.conf.speed / po.conf.new_speed
		po.conf.speed = po.conf.new_speed
		po.conf.new_speed = 0
		t = po.stim + s.ptim / po.conf.speed
	}

	maxt = t + po.tgen		// max time = now + 'tgen' seconds
	po.timouts = []
	while (1) {
		if (!po.p_v[s.v])		// if new voice
			set_ctrl(po, s, t)	// set the MIDI controls
		switch (s.type) {
		case C.BAR:
			if (s.rep_p) {		// right repeat
				po.repv++
				if (!po.repn	// if repeat a first time
				 && (!s.rep_v	// and no variant (anymore)
				  || po.repv < s.rep_v.length)) {
					po.stim += (s.ptim - s.rep_p.ptim) /
							po.conf.speed
					s = s.rep_p	// left repeat
					while (!s.dur)
						s = s.ts_next
					t = po.stim + s.ptim / po.conf.speed
					po.repn = true
					break
				}
				po.repn = false
			}
			if (s.rep_s) {			// first variant
				s2 = s.rep_s[po.repv]	// next variant
				if (s2) {
					po.stim += (s.ptim - s2.ptim) /
							po.conf.speed
					s = s2
					t = po.stim + s.ptim / po.conf.speed
					po.repn = false
				} else {		// end of tune
					s = po.s_end
					break
				}
			}
			if (s.bar_type.slice(-1) == ':') // left repeat
				po.repv = 1

		    if (!s.part1) {
			while (s.ts_next && !s.ts_next.seqst) {
				s = s.ts_next
				if (s.part1)
					break
			}
			if (!s.part1)
				break
		    }
			// fall thru
		default:
			if (s.part1				// if end of part
			 && po.i_p != undefined) {
				s2 = s.part1.p_s[++po.i_p]	// next part
				if (s2) {
					po.stim += (s.ptim - s2.ptim) / po.conf.speed
					s = s2
					t = po.stim + s.ptim / po.conf.speed
				} else {
					s = po.s_end
				}
				po.repv = 1
			}
			break
		}
	    if (s && s != po.s_end) {
		switch (s.type) {
		case C.BAR:
			break
		case C.BLOCK:
			if (s.subtype == "midictl")
				po.midi_ctrl(po, s, t)
			break
		case C.GRACE:
			for (g = s.extra; g; g = g.next) {
				d = g.pdur / po.conf.speed
				for (m = 0; m <= g.nhd; m++) {
					note = g.notes[m]
					po.note_run(po, g,
						note.midi,
						t + g.ptim - s.ptim,
//fixme: there may be a tie...
						d)
				}
			}
			break
		case C.NOTE:
		case C.REST:
			d = s.pdur / po.conf.speed
		    if (s.type == C.NOTE) {
			for (m = 0; m <= s.nhd; m++) {
				note = s.notes[m]
				if (note.tie_s)		// end of tie
					continue	// already generated
				po.note_run(po, s,
					note.midi,
					t,
					note.tie_e ?
						do_tie(note, d) : d)
			}
		    }

			// follow the notes/rests while playing
			if (po.onnote && s.istart) {
				i = s.istart
				st = (t - now) * 1000
				po.timouts.push(setTimeout(po.onnote, st, i, true))
				if (d > 2)	// problem when loop on one long note
					d -= .1
				setTimeout(po.onnote, st + d * 1000, i, false)
			}
			break
		}
	    }
		while (1) {
			if (!s || s == po.s_end || !s.ts_next) {
				if (po.onend)
					setTimeout(po.onend,
						(t - now + d) * 1000,
						po.repv)
				po.s_cur = s
				return
			}
			s = s.ts_next
			if (!s.noplay)
				break
		}
		t = po.stim + s.ptim / po.conf.speed // next time
		if (t > maxt)
			break
	}
	po.s_cur = s

	// delay before next sound generation
	po.timouts.push(setTimeout(play_cont,
				(t - now) * 1000
					- 300,	// wake before end of playing
				po))
    } // play_cont()

    // search the index in the parts
    function get_part(po) {
    var	s, i, s_p
	for (s = po.s_cur; s; s = s.ts_prev) {
		if (s.parts) {
			po.i_p = -1
			return
		}
		s_p = s.part1
		if (!s_p || !s_p.p_s)
			continue
		for (i = 0; i < s_p.p_s.length; i++) {
			if (s_p.p_s[i] == s) {
				po.i_p = i	// index in the parts
				return
			}
		}
	}
    } // get_part()

    // --- play_next ---
	get_part(po)

	po.stim = po.get_time(po) + .3	// start time + 0.3s
			- po.s_cur.ptim * po.conf.speed
	po.p_v = []			// voice table for the MIDI controls
	if (!po.repv)
		po.repv = 1

	play_cont(po)			// start playing
} // play_next()

// nodejs
if (typeof module == 'object' && typeof exports == 'object')
	exports.ToAudio = ToAudio
