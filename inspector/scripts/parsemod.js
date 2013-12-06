//namespace scope
(function( DMI, $, undefined ){
		
var Utils = DMI.Utils;
var modconstants = DMI.modconstants;


//basic helpers for setting values
function argstr(a) {
	if (!a.s) throw "argument missing (string expected)";
	if (a.n1) throw "unexpected argument (number)";
	return a.s;
}
function argtrim(a) {
	if (!a.s) throw "argument missing (string expected)";
	if (a.n1) throw "unexpected argument (number)";
	return $.trim(a.s);
}
function _str(c,a,t) { 
	modctx[t][c] = argstr(a); 
}

function argnum(a) {
	if (!a.n1) throw "argument missing (number expected)";
	if (a.s) throw "unexpected argument (string)";
	return a.n1;
}
function argnum2(a) {
	if (!a.n2) throw "argument missing (number expected)";
	if (a.s) throw "unexpected argument (string)";
	return a.n2;
}
function _num(c,a,t) {
	modctx[t][c] = argnum(a);
}
function _num_def(n) {
	var n = n;
	return function(c,a,t) {
		if (a.s) throw "unexpected argument (string)";
		modctx[t][c] = a.n1 || String(n);
	}
}
function _str_num(c,a,t) { 
	if (!a.n1 && !a.s) throw "argument missing (number or string expected)";
	modctx[t][c] = (a.n1 || a.s);
}

function argref(a) { 
	if (!a.n1 && !a.s) throw "argument missing (number or string expected)";
	return a.n1 || $.trim(a.s.toLowerCase());
}
function _ref(c,a,t) { 
	modctx[t][c] = argref(a);
}
function _bool(c,a,t) {
	if (a.n1 || a.s) throw "unexpected argument (none expected)";
	modctx[t][c] = '1'; 
}
function _ignore(c,a,t) {  }


//truncates name till it is found in object. returns found value
function lookupTruncation(longname, object, minlen) {
	if (!minlen) minlen = 1;
	while (longname.length > minlen) { 
		longname = longname.substr(0, longname.length-1);
		if (object[longname])
			return longname; 
	}
	return null;
}

//is this a valid command?
function validCmdContexts(cmd) {
	var ctxs = [];
	var types = ['unit', 'spell', 'wpn', 'item', 'armor', 'nation', 'site'];		
	for (var j=0, type; type=types[j]; j++) {
		var cmdlookup = modctx[type+'commands'];
		var fn = cmdlookup[cmd];
		if (fn) ctxs.push(type);
	}
	return ctxs;
}

//base data object
var modctx = DMI.modctx = {

	//main mod command switch
	processCommand: function(cmd, args, warningFn) {
		var fullcmd = cmd;
		
		var types = ['unit', 'spell', 'wpn', 'item', 'armor', 'nation', 'site'];		
		for (var j=0, type; type=types[j]; j++) {
			if (modctx[type]) {
				//lookup cmd for open object
				var cmdlookup = modctx[type+'commands'];
				var fn = cmdlookup[cmd];
				
				//not found.. lookup shortened command
				if (!fn) {
					fn = cmdlookup[cmd = lookupTruncation(cmd, cmdlookup, 3)];
					if (fn) warningFn('unrecognised cmd.. assuming you meant: #'+cmd)
				}			
				//still not found.. abort
				if (!fn) throw 'command not found for ' + type + (modctx.commands[fullcmd] ? ' (missing #end ?)' : '');
			
				//process cmd
				fn(cmd, args, type, warningFn);
				return;
			}
		}
		//no open context
		var fn = modctx.commands[cmd];
		
		//not found.. lookup shortened command
		if (!fn) {
			fn = modctx.commands[cmd = lookupTruncation(cmd, modctx.commands, 3)];
			if (fn) warningFn('unrecognised cmd.. assuming you meant: #'+cmd)
		}			
		//still not found.. abort
		if (!fn) {
			var ctxs = validCmdContexts(fullcmd)
			if (ctxs.length)
				throw ('command out of context ('+(ctxs.join('/'))+' not open)');
			else
				throw 'unrecognised command';
		}			
		//process cmd
		fn(cmd, args, null, warningFn);
	},

	//helpers
	_checkContextClosed: function(fnwarn) {
		if (modctx.item || modctx.armor || modctx.wpn || modctx.unit || modctx.spell) {
			modctx.item = modctx.armor = modctx.wpn = modctx.unit = modctx.spell = null;
			fnwarn('missing #end');
		}
	},
	_select: function(cmd, args, tname, fnwarn) {
		modctx._checkContextClosed(fnwarn);
		
		var o;
		if (args.s)
			o = modctx[tname+'lookup'][$.trim(args.s.toLowerCase())];
		else if (args.n1)
			o = modctx[tname+'lookup'][args.n1];
		else throw 'missing argument (id/name expected)';
		
		if (!o) throw 'data not found';
		modctx[tname] = o;
	},      
	_new: function(cmd, args, tname, fnwarn) {
		modctx._checkContextClosed(fnwarn);
		if (!args.n1) throw 'missing argument (id expected)';		
		if (modctx[tname+'lookup'][args.n1]) throw 'id already in use';		
			
		var o = modctx[tname] = { id: args.n1 };
		modctx[tname+'data'].push(o);
		modctx[tname+'lookup'][args.n1] = o;
	},
	// _select_or_new: function(cmd, args, tname, fnwarn) {
	// 	try {
	// 		modctx._select(cmd, args, tname, fnwarn)			
	// 	}
	// 	catch(e) {
	// 		if (e == 'data not found' && args.n1)
	// 			modctx._new(cmd, args, tname,fnwarn)
	// 		else throw e;
	// 	}
	// },

	//global commands (nothing currently selected)
	commands: {
		//mod info
		modname: function(cmd, args) {
			modctx.loadedmods.push(argstr(args));
		},
		description: function(cmd, args) {},
		icon: _ignore,
		version: _ignore,
		domversion: _ignore,
		
		//general modding
		poppergold: _ignore,
		resourcemult: _ignore,
		supplymult: _ignore,
		unresthalfinc: _ignore,
		unresthalfres: _ignore,
		eventisrare: _ignore,
		turmoilincome: _ignore,
		turmoilevents: _ignore,
		deathincome: _ignore,
		deathsupply: _ignore,
		deathdeath: _ignore,
		slothincome: _ignore,
		slothresources: _ignore,
		coldincome: _ignore,
		coldsupply: _ignore,
		misfortune: _ignore,
		luckevents: _ignore,
		researchscale: _ignore,
		
		end: function(c,a,t,fnw) { fnw('no open object'); },
				
		//nation modding
		indepflag: _ignore,	
		
		//commands opening a context
		newitem:    function(c,a,t,fnw){
			//get first unused id
			var id = modctx.itemdata.length;
			while (modctx.itemlookup[id]) id++;
			
			modctx._new(c, {n1:id} ,'item', fnw);
			DMI.MItem.initItem(modctx.item);
		},
		selectitem: function(c,a,t,fnw){ modctx._select(c,a,'item',fnw); },
		
		newarmor: function(c,a,t,fnw) {
			modctx._new(c,a ,'armor',fnw);			
			DMI.MArmor.initArmor(modctx.armor);

			if (a.n1<200 || a.n1>399) throw 'invalid id';
		},
		selectarmor: function(c,a,t,fnw){ 
			try {
				modctx._select(c,a,'armor',fnw)
			}
			catch(e) {
				if (e == 'data not found' && a.n1) {
					modctx.commands.newarmor(c,a,t,fnw);
				}
				else throw e;
			}
		},
		//selectarmor: function(c,a,t,fnw){ modctx._select(c,a,'armor',fnw); },
		
		newweapon: function(c,a,t,fnw) {
			modctx._new(c,a ,'wpn',fnw);			
			DMI.MWpn.initWpn(modctx.wpn);
			
			if (a.n1<600 || a.n1>999) throw 'invalid id';
		},
		selectweapon: function(c,a,t,fnw){ 
			try {
				modctx._select(c,a,'wpn',fnw)
			}
			catch(e) {
				if (e == 'data not found' && a.n1) {
					modctx.commands.newweapon(c,a,t,fnw);
				}
				else throw e;
			}
		},
		newmonster: function(c,a,t,fnw) {
			modctx._new(c,a ,'unit',fnw); 

			// Utils.merge( modctx.unit, {
			// 		weapons: [],
			// 		armor:[],
			// 		randompaths:[],
					
			// 		hand: '2',
			// 		head: '1',
			// 		body: '1',
			// 		foot: '1',
			// 		misc: '2',
					
			// 		leader: '40',
			// 		sprite: { unitid: a.n1 }
			// });
			DMI.MUnit.initUnit(modctx.unit);
			
			if (a.n1<2200 || a.n1>3999) throw 'invalid id';
		},
		selectmonster: function(c,a,t,fnw){ modctx._select(c,a,'unit',fnw); },
		
		newspell: function(c,a,t,fnw) {
			//get first unused id
			var id = modctx.spelldata.length;
			while (modctx.spelllookup[id]) id++;
				
			modctx._new(c, {n1:id} ,'spell',fnw);
			DMI.MSpell.initSpell(modctx.spell);
		},
		selectspell: function(c,a,t,fnw){ modctx._select(c,a,'spell',fnw); },
		
		//
		//no newnation
		//
		selectnation: function(c,a,t,fnw){ //modctx._select(c,a,'nation')
			try {
				modctx._select(c,a,'nation',fnw)
			}
			catch(e) {
				if (e == 'data not found' && a.n1) {
					modctx._new(c,a,'nation',fnw);
					DMI.MNation.initNation(modctx.nation);	
				}
				else throw e;
			}
		},
		
		newsite: function(c,a,t,fnw) {
			modctx._new(c,a ,'site',fnw);
			DMI.MSite.initSite(modctx.site);

			if (a.n1<750 || a.n1>999) throw 'invalid id';
		},
		selectsite: function(c,a,t,fnw){ modctx._select(c,a,'site',fnw); }
	},

	//item selected
	itemcommands: {
		end: function(c,a,t){ modctx[t] = null; },
		name: function(c,a,t) {
			if (modctx.item.name) delete modctx.itemlookup[modctx.item.name];
			modctx.item.name = argtrim(a);
			modctx.itemlookup[argtrim(a).toLowerCase()] = modctx.item;
		},
		descr: 		_str, //function(a,t,c){  modctx[t][c] = a.s;  },
		type: 		function(c,a,t){ modctx[t][c] = modconstants['19'][argnum(a)]; },
		
		weapon: 	_ref,
		armor:		_ref,
		constlevel: 	_num,
		mainpath:	function(c,a,t){ modctx[t][c] = modconstants['16'][argnum(a)] },
		secondarypath: 	function(c,a,t){ modctx[t][c] = modconstants['16'][argnum(a)] },
		mainlevel: 	_num,
		secondarylevel: _num,
		copyspr: 	_ref
	},

	//weapon selected
	wpncommands: {
		end: function(c,a,t){
			var name = modctx.wpn.name; 
			modctx[t] = null;
			if (!name) throw 'unnamed weapon';
		},
		name: function(c,a,t) {
			if (modctx.wpn.name) delete modctx.wpnlookup[modctx.wpn.name.toLowerCase()];
			modctx.wpn.name = argtrim(a);
			modctx.wpnlookup[argtrim(a).toLowerCase()] = modctx.wpn;
		},
		def: 		_num,
		rcost: 		_num,
		
		dmg: 		_num,
		nratt: 		_num,
		att: 		_num,
		len: 		_num,
		range: 		_num,
		ammo: 		_num,
		
		twohanded:	_bool,
		armorpiercing:	_bool,
		armornegating:	_bool,
		magic:		_bool,
		
		
		dt_normal:	_bool,
		dt_stun:	_bool,
		dt_paralyze:	_bool,
		dt_poison:	_bool,
		dt_cap:		_bool,
		dt_demon:	_bool,
		dt_demononly:	_bool,
		dt_holy:	_bool,
		dt_magic:	_bool,
		dt_small:	_bool,
		dt_large:	_bool,
		dt_constructonly:_bool,
		dt_raise:	_bool,
		
		mind:		_bool,
		cold:		_bool,
		fire:		_bool,
		shock:		_bool,
		poison:		_bool,

		//fx
		flyspr:		_ignore,
		explspr:	_ignore,
		sound:		_ignore,
		
		//advanced weapon modding
		aoe:		_num,
		bonus:		_bool,
		charge:		_bool,
		flail:		_bool,
		nostr:		_bool,
		mrnegates:		_bool,
		mrnegateseasily:	_bool,
		
		secondaryeffect:	_ref,
		secondaryeffectalways:	_ref
	},

	//armor selected
	armorcommands: {
		end: function(c,a,t){
			var name = modctx.armor.name; 
			modctx[t] = null;
			if (!name) throw 'unnamed armor';
		},
		name: function(c,a,t) {
			if (modctx.armor.name) delete modctx.armorlookup[modctx.armor.name.toLowerCase()];
			modctx.armor.name = argtrim(a);
			modctx.armorlookup[argtrim(a).toLowerCase()] = modctx.armor;
		},
		type: 	_num,
		def:	_num,
		rcost: 	_num,
		prot: 	_num,
		enc: 	_num		
	},

	//unit selected
	unitcommands: {
		end: function(c,a,t){
			var name = modctx.unit.name; 
			modctx[t] = null;
			if (!name) throw 'unnamed unit';
		},
		name: function(c,a,t){
			//if (modctx.unit.name) delete modctx.unitlookup[modctx.unit.name.toLowerCase()];
			modctx.unit.name = argtrim(a);
			modctx.unitlookup[argtrim(a).toLowerCase()] = modctx.unit;
		},
		clear: function(c,a,t){
			var o = modctx.unit;
			var keepstats = {
			//KEEP
				modded:1,
				id:1,
				name:1,
				sprite:1
			};
			for (var k in o)
				if (!keepstats[k]) delete o[k];
			
			o.armor = [];
			o.weapons = [];
			o.randompaths = [];
		},
		clearweapons: function(c,a,t){ modctx.unit.weapons = []; },
		cleararmor: function(c,a,t){ modctx.unit.armor = []; },
		clearspec: function(c,a,t){ 
			var o = modctx.unit;
			var deletestats = {
			//CLEAR
				female:1, mounted:1, holy:1, animal:1, undead:1, demon:1, magic:1, stonebeing:1, inanimate:1, coldblood:1, immortal:1, blind:1, unique:1, shatteredsoul:1,
				immobile:1, aquatic:1, amphibian:1, pooramphibian:1, flying:1, stormimmune:1, sailing:1, forestsurvival:1, mountainsurvival:1, swampsurvival:1, wastesurvival:1,
				coldres:1, fireres:1, poisonres:1, shockres:1, darkvision:1, 
				stealthy:1, illusion:1, spy:1, assassin:1, seduce:1, succubus:1, beckon:1,
				startage:1, maxage:1, older:1, healer:1, heal:1, noheal:1, 
				startaff:1, supplybonus:1, neednoteat:1, uwdamage:1, homesick:1, 
				coldpower:1, firepower:1, stormpower:1, darkpower:1, 
				springpower:1, summerpower:1, fallpower:1, winterpower:1, 
				ambidextrous:1, banefireshield:1, berserk:1, 
				ethereal:1, standard:1, 
				animalawe:1, awe:1, fear:1, regeneration:1, reinvigoration:1, 
				fireshield:1, heat:1, cold:1, iceprot:1, 
				trample:1, entangle:1, eyeloss:1, 
				horrormark:1, poisonarmor:1, poisoncloud:1, diseasecloud:1, bloodvengeance:1, castledef:1, siegebonus:1, patrolbonus:1, pillagebonus:1, 
				researchbonus:1, forgebonus:1, douse:1, nobadevents:1, incunrest:1, spreaddom:1, leper:1, popkill:1, inquisitor:1, heretic:1
			};
			for (var k in deletestats)
				if (o[k]) delete o[k];
		},
		clearmagic: function(c,a,t){ 
			var o = modctx.unit;
			var deletestats = {
			//CLEAR
				F:1, A:1, W:1, E:1, S:1, D:1, N:1, B:1, H:1, 
				magicbonus:1
				//researchbonus:1,  //???
			};
			for (var k in deletestats)
				if (o[k]) delete o[k];
			
			o.randompaths = [];
		},
		copystats: function(c,a,t){
			var from = modctx.unitlookup[a.n1] || modctx.unitlookup[$.trim(a.s.toLowerCase())];
			if (!from) throw 'original unit not found';
			var ignorestats = {
			//IGNORE
				modded:1,
				id:1,
				sprite:1
				//
				//...
				//...
			};
			var to = modctx.unit;
			for (var k in to)   if (!ignorestats[k]) delete to[k];
			for (var k in from) if (!ignorestats[k]) to[k] = from[k];
				
			//deep copy arrays
			to.weapons = [];
			for (var i=0, m; m= from.weapons[i]; i++) to.weapons[i] = m;
			
			to.armor = [];
			for (var i=0, m; m= from.armor[i]; i++) to.armor[i] = m;
			
			to.randompaths = [];
			for (var i=0, m; m= from.randompaths[i]; i++) to.randompaths[i] = m;
		},
		copyspr: function(c,a,t){
			var from = modctx.unitlookup[a.n1] || modctx.unitlookup[$.trim((a.s || '-1').toLowerCase())];
			if (!from) throw 'original unit not found';
			
			modctx.unit.sprite = from.sprite; 
		},
		descr: _str,

		//fx
		spr1:	function(c,a,t){ modctx[t].sprite.spr1 = a.s; },
		spr2:	function(c,a,t){ modctx[t].sprite.spr2 = a.s; },
		speciallook: _ignore,
		
		ap:	_num,
		mapmove:_num,
		hp:	_num,
		prot:	_num,
		size:	_num,
		ressize:_num,
		str:	_num,
		enc:	_num,
		att:	_num,
		def:	_num,
		prec:	_num,
		mr:	_num,
		mor:	_num,
		gcost:	_num,
		rcost:	_num,
		
		weapon:	function(c,a,t){ modctx[t]['weapons'].push(argref(a)); }, //deferr lookups
		armor:	function(c,a,t){ modctx[t]['armor'].push(argref(a)); }, //deferr lookups
		
		eyes:	_num,
		
		//these should turn unit into a pretender.. right?
		pathcost: function(c,a,t){
			var o = modctx[t];
			if (!Utils.is(o.pathcost)) { //is new pretender
				for (var n, i=0; n=modctx.nationdata[i]; i++)
					Utils.joinArray(o.id, n.pretenders);
			}
			_num(c,a,t);
		},
		startdom: function(c,a,t){
			var o = modctx[t];
			if (!Utils.is(o.startdom)) { //is new pretender
				for (var n, i=0; n=modctx.nationdata[i]; i++)
					Utils.joinArray(o.id, n.pretenders);
			}
			_num(c,a,t);
		},
		restrictedgod: function(c,a,t){
			var o = modctx[t];
			//remove as pretender from current nations
			for (var n, i=0; n=modctx.nationdata[i]; i++)
				Utils.weedArray(o.id, n.pretenders);

			//and associate with just this one nation
			var nation = modctx.nationlookup[argnum(a)];
			if (nation) nation.pretenders.push(o.id);

			//can use this command on multiple nations??
		},
		
		female:		_bool,
		mounted:	_bool,
		holy:		_bool,
		animal:		_bool,
		undead:		_bool,
		demon:		_bool,
		magicbeing:	_bool,
		stonebeing:	_bool,
		inanimate: 	_bool,
		coldblood:	_bool,
		immortal:	_bool,
		blind:		_bool,
		unique:		_bool,
		shatteredsoul:	_num,
	
		immobile:	_bool,
		aquatic:	_bool,
		amphibian:	_bool,
		pooramphibian:	_bool,
		flying:		_bool,
		stormimmune:	_bool,
		sailing:	_bool,
		forestsurvival:	_bool,
		mountainsurvival:_bool,
		swampsurvival:	_bool,
		wastesurvival:	_bool,
		
		coldres:	_num,
		fireres:	_num,
		poisonres:	_num,
		shockres:	_num,
		darkvision:	_num,
	
		stealthy:	_num_def(0),
		illusion:	_bool,
		spy:		_bool,
		assassin:	_bool,
		seduce:		_num,
		succubus:	_num,
		beckon:		_num,
		
		startage:	_num,
		maxage:		_num,
		older:		_num,
		healer:		_num,
		heal:		_bool,
		noheal:		_bool,
		
		startaff:	_num,
		supplybonus:	_num,
		neednoteat:	_bool,
		uwdamage:	_num,
		homesick:	_num,

		coldpower:	_num,
		firepower:	_num,
		stormpower:	_num,
		darkpower:	_num,

		springpower:	_num,
		summerpower:	_num,
		fallpower:	_num,
		winterpower:	_num,
		
		ambidextrous:	_num,
		banefireshield:	_num,
		berserk:	_num,
		
		ethereal:	_bool,
		standard:	_num,
		
		animalawe:	_num,
		awe:		_num,
		fear:		_num,
		regeneration:	_num,
		reinvigoration:	_num,
		
		fireshield:	_num,
		heat:		_num_def(3),
		cold:		_num_def(3),
		iceprot:	_num,
		
		trample:	_bool,
		entangle:	_bool,
		eyeloss:	_bool,
		
		horrormark:	_bool,
		poisonarmor:	_bool,
		poisoncloud:	_num,
		diseasecloud:	_num,
		bloodvengeance:	_num,
		castledef:	_num,
		siegebonus:	_num,
		patrolbonus:	_num,
		pillagebonus:	_num,

		researchbonus:	_num,
		forgebonus:	_num,
		douse:		_num,
		nobadevents:	_num,
		incunrest:	_num,
		spreaddom:	_num,
		leper:		_num,
		popkill:	_num,
		inquisitor:	_bool,
		heretic:	_num,

		itemslots:	function(c,a,t){
			var bitfield = parseInt(argnum(a));
			var slotmasks = {
				2: 'hand',	4: 'hand',	8: 'hand',	16: 'hand',	32: 'hand',	64: 'hand',
				128: 'head',	256: 'head',	512: 'head',	
				1024: 'body',	
				2048: 'foot',		
				4096: 'misc',	8192: 'misc',	16384: 'misc',	32768: 'misc',	65536: 'misc',	131072: 'misc'
			};
			var o = modctx.unit;
			o.hand = o.head = o.body = o.foot = o.misc = '0';
			//if (! (bitfield & 1)) { 
				for (var k in slotmasks) {
					if (bitfield & parseInt(k)) {
						var slot = slotmasks[k]; 
						o[slot] = Utils.sum( o[slot], 1 );
					}
				}
			//}
		},
		noitem:	function(c,a,t){
			var o = modctx.unit;
			o.hand = '0'; 
			o.head = '0';
			o.body = '0'; 
			o.foot = '0';
			//just leaves misc
		},
		
		//magic
		magicskill: function(c,a,t){
			var pstr = modconstants[10][argnum(a)];
			if (!pstr) throw 'invalid magic index';
			
			for (var i=0, p; p= pstr.charAt(i); i++) {
				modctx.unit[p] = a.n2;
			}
		},
		magicboost: function(c,a,t){
			//each magicboost overwrites the last
			//any number 10+ affects all paths (this is a seperate magicboost)
			// #magicboost 3 1 --E+1
			// #magicboost 3 1 --E+1
			// #magicboost 53 1 --all+1
			// = E+2
			
			var pstr = modconstants[10][argnum(a)];
			if (!Utils.inArray(pstr, modconstants.pathkeys))
				pstr = 'all';
			
			modctx.unit['magicboost_'+pstr] = a.n2;
			
			// if (pstr == 'FAWESDNBH') pstr = '';
			// modctx.unit['magicboost'] = DMI.Format.Signed(a.n2) + pstr;
			// for (var i=0, p; p= pstr.charAt(i); i++) {
			// 	modctx.unit['magicboost_'+p] = Utils.sum(modctx.unit['magicboost_'+p], a.n2);
			// }
		},
		//random paths
		custommagic: function(c,a,t){
			var bitfield = parseInt(argnum(a)),  chance = parseInt(argnum2(a));
			var pmasks = modconstants[11];
			var pstr = '', levels = 1; 
			
			//bitfield to path string
			for (var k in pmasks)
				if (parseInt(k) & bitfield)  pstr += pmasks[k];
			
			//linked random
			while( chance > 100 ) {
				chance = chance - 100;
				levels += 1;
			}
			//create details object and push it onto unit
			modctx.unit.randompaths.push({ paths: pstr,  levels: levels,  chance: chance });
		},
		gemprod: function(c,a,t){
			var pstr = modconstants[10][argnum(a)];
			if (!pstr) throw 'invalid magic index';
			
			for (var i=0, p; p= pstr.charAt(i); i++) {
				modctx.unit['g'+p] = argnum2(a);
			}
		},
		onebattlespell: _ref,
		drainimmune: 	_bool,
		
		firstshape:	_ref,
		secondshape:	_ref,
		secondtmpshape:	_ref,
		shapechange:	_ref,
		landshape:	_ref,
		watershape:	_ref,
		forestshape:	_ref,
		plainshape:	_ref,
		
		domsummon:	function(c,a,t){ modctx[t]['domsummon'] = argref(a);  modctx[t]['n_domsummon'] = 'DRN' },
		domsummon2:	function(c,a,t){ modctx[t]['domsummon'] = argref(a);  modctx[t]['n_domsummon'] = 'DRN/2' },
		domsummon5:	function(c,a,t){ modctx[t]['domsummon'] = argref(a);  modctx[t]['n_domsummon'] = 'DRN/5' },
		domsummon10:	function(c,a,t){ modctx[t]['domsummon'] = argref(a);  modctx[t]['n_domsummon'] = 'DRN/10' },
		domsummon20:	function(c,a,t){ modctx[t]['domsummon'] = argref(a);  modctx[t]['n_domsummon'] = 'DRN/20' },
		
		makemonster1:	function(c,a,t){ modctx[t]['makemonster'] = argref(a);  modctx[t]['n_makemonster'] = '1' },
		makemonster2:	function(c,a,t){ modctx[t]['makemonster'] = argref(a);  modctx[t]['n_makemonster'] = '2' },
		makemonster3:	function(c,a,t){ modctx[t]['makemonster'] = argref(a);  modctx[t]['n_makemonster'] = '3' },
		makemonster4:	function(c,a,t){ modctx[t]['makemonster'] = argref(a);  modctx[t]['n_makemonster'] = '4' },
		makemonster5:	function(c,a,t){ modctx[t]['makemonster'] = argref(a);  modctx[t]['n_makemonster'] = '5' },
		
		summon1:	function(c,a,t){ modctx[t]['summon'] = argref(a);  modctx[t]['n_summon'] = '1' },
		summon2:	function(c,a,t){ modctx[t]['summon'] = argref(a);  modctx[t]['n_summon'] = '2' },
		summon3:	function(c,a,t){ modctx[t]['summon'] = argref(a);  modctx[t]['n_summon'] = '3' },
		summon4:	function(c,a,t){ modctx[t]['summon'] = argref(a);  modctx[t]['n_summon'] = '4' },
		summon5:	function(c,a,t){ modctx[t]['summon'] = argref(a);  modctx[t]['n_summon'] = '5' },
	
		nametype:	_num,
		
		noleader:	function(c,a,t){ modctx[t]['leader'] = 0; },
		poorleader:	function(c,a,t){ modctx[t]['leader'] = 10; },
		okleader:	function(c,a,t){ modctx[t]['leader'] = 40; },
		goodleader:	function(c,a,t){ modctx[t]['leader'] = 80; },
		expertleader:	function(c,a,t){ modctx[t]['leader'] = 120; },
		superiorleader:	function(c,a,t){ modctx[t]['leader'] = 160; },
		
		nomagicleader:		function(c,a,t){ modctx[t]['magicleader'] = 0; },
		poormagicleader:	function(c,a,t){ modctx[t]['magicleader'] = 10; },
		okmagicleader:		function(c,a,t){ modctx[t]['magicleader'] = 40; },
		goodmagicleader:	function(c,a,t){ modctx[t]['magicleader'] = 80; },
		expertmagicleader:	function(c,a,t){ modctx[t]['magicleader'] = 120; },
		superiormagicleader:	function(c,a,t){ modctx[t]['magicleader'] = 160; },
		
		noundeadleader:		function(c,a,t){ modctx[t]['undeadleader'] = 0; },
		poorundeadleader:	function(c,a,t){ modctx[t]['undeadleader'] = 10; },
		okundeadleader:		function(c,a,t){ modctx[t]['undeadleader'] = 40; },
		goodundeadleader:	function(c,a,t){ modctx[t]['undeadleader'] = 80; },
		expertundeadleader:	function(c,a,t){ modctx[t]['undeadleader'] = 120; },
		superiorundeadleader:	function(c,a,t){ modctx[t]['undeadleader'] = 160; }		
	},

	//spell selected
	spellcommands: {
		end: function(c,a,t){
			var name = modctx.spell.name; 
			modctx[t] = null;
			if (!name) throw 'unnamed spell';
		},
		name: function(c,a,t) {
			//if (modctx.spell.name) delete modctx.spelllookup[modctx.spell.name.toLowerCase()];
			modctx.spell.name = argtrim(a);
			modctx.spelllookup[$.trim(a.s.toLowerCase())] = modctx.spell;
		},
		descr: _str,
		
		clear: function(c,a,t){
			var o = modctx.spell;
			var keepstats = {
				//stats to keep (not cleared)
				modded:1,
				id:1,
				name:1
			};
			for (var k in o)
				if (!keepstats[k]) delete o[k];
			
			o.nations = [];
		},
		copyspell: function(c,a,t){
			var from = modctx.spelllookup[a.n1] || modctx.spelllookup[($.trim(a.s) || '-1').toLowerCase()];
			if (!from) throw 'original spell not found';
			var ignorestats = {
				//stats to NOT copy
				modded:1,
				id:1
				//name:1,
			};
			var to = modctx.spell;
			for (var k in to)   if (!ignorestats[k]) delete to[k];
			for (var k in from) if (!ignorestats[k]) to[k] = from[k];
				
			//deep copy arrays
			to.nations = [];
			for (var i=0, m; m= from.nations[i]; i++) to.nations[i] = m;
		},
		school:		_num,
		researchlevel:	_num,
		path: function(c,a,t){
			var index, pkey;
			//verify path1 or path2
			if (argnum(a) == '0') index = 'path1';
			else if (argnum(a) == '1') index = 'path2';
			else throw "invalid reqnr (expected 0 or 1)";
			
			if (argnum2(a) == '-1') {
				delete modctx[t][index];
				return;
			}
			if (!(pkey=  modconstants[10][argnum2(a)])) throw "invalid path number";
			modctx[t][index] = pkey;
		},
		pathlevel: function(c,a,t){
			var index;
			//verify path1 or path2
			if (argnum(a) == '0') index = 'pathlevel1';
			else if (argnum(a) == '1') index = 'pathlevel2';
			else throw "invalid reqnr (expected 0 or 1)";
			
			modctx[t][index] = argnum2(a);
		},
		aoe:		_num,
		damage:		_num,
		effect:		_num,
		fatiguecost:	_num,
		nextspell:	_ref,
		nreff:		_num,
		range:		_num,
		precision:	_num,
		spec:		_num,
		
		restricted: function(c,a,t){ modctx.spell.nations.push(argref(a)); }, //deferr lookups

		//fx
		flightspr:	_ignore,
		explspr:	_ignore,
		sound:		_ignore
	},

	//nation selected
	nationcommands: {
		end: function(c,a,t){ modctx[t] = null; },
		
		clearnation: function(c,a,t) {
			var o = modctx.nation;
			var keepstats = {
			//KEEP
				//????????????????????
				modded:1,
				id:1,
				name:1, epithet:1, era:1,
				descr:1, summary:1, brief:1,
				color:1, flag:1,

				//units
				//~ startcom:1, startscout:1,
				//~ startunittype1:1, startunitnbrs1:1,
				//~ startunittype2:1, startunitnbrs2:1,
				units:1,
				commanders:1
				//~ uwunits:1,
				//~ uwcom:1, 
				//~ heroes:1,
				//~ multiheroes:1,
				//~ defcom1:1, defunit1:1, defmult1:1, defunit1b:1, defmult1b:1, 
				//~ defcom2:1, defunit2:1, defmult2:1, defunit2b:1, defmult2b:1,

				//sites
				//startsites:1,
			};
			for (var k in o) {
				if (!keepstats[k]) {
					if ($.isArray(o[k])) o[k] = [];
					else delete o[k];
				}
			}
		},
		clearrec: function(c,a,t) {
			var o = modctx.nation;
			var clearstats = {
			//CLEAR
				units:1,
				commanders:1
			};
			for (var k in o) {
				if (clearstats[k]) {
					if ($.isArray(o[k])) o[k] = [];
					else delete o[k];
				}
			}
		},
		//units
		startcom: _ignore,//_ref,
		startscout: _ignore,//_ref,
		startunittype1: _ignore,//_ref,
		startunitnbrs1: _ignore,//_ref,
		startunittype2: _ignore,//_ref,
		startunitnbrs2: _ignore,//_ref,

		addrecunit: function(c,a,t){ modctx[t]['units'].push(argref(a)); },
		addreccom: function(c,a,t){ modctx[t]['commanders'].push(argref(a)); },
		
		uwunit1: function(c,a,t){ modctx[t]['uwunits'][1] = argref(a); },
		uwunit2: function(c,a,t){ modctx[t]['uwunits'][2] = argref(a); },
		uwunit3: function(c,a,t){ modctx[t]['uwunits'][3] = argref(a); },
		uwunit4: function(c,a,t){ modctx[t]['uwunits'][4] = argref(a); },
		uwunit5: function(c,a,t){ modctx[t]['uwunits'][5] = argref(a); },
		uwcom1: function(c,a,t){ modctx[t]['uwcoms'][1] = argref(a); },
		uwcom2: function(c,a,t){ modctx[t]['uwcoms'][2] = argref(a); },
		uwcom3: function(c,a,t){ modctx[t]['uwcoms'][3] = argref(a); },
		uwcom4: function(c,a,t){ modctx[t]['uwcoms'][4] = argref(a); },
		uwcom5: function(c,a,t){ modctx[t]['uwcoms'][5] = argref(a); },

		hero1: function(c,a,t){ modctx[t]['heroes'][1] = argref(a); },
		hero2: function(c,a,t){ modctx[t]['heroes'][2] = argref(a); },
		hero3: function(c,a,t){ modctx[t]['heroes'][3] = argref(a); },
		hero4: function(c,a,t){ modctx[t]['heroes'][4] = argref(a); },
		hero5: function(c,a,t){ modctx[t]['heroes'][5] = argref(a); },
		hero6: function(c,a,t){ modctx[t]['heroes'][6] = argref(a); },

		multihero1: function(c,a,t){ modctx[t]['multiheroes'][1] = argref(a); },
		multihero2: function(c,a,t){ modctx[t]['multiheroes'][2] = argref(a); },

		defcom1: _ignore,//_ref,
		defunit1: _ignore,//_ref,
		defmult1: _ignore,//_ref,
		defunit1b: _ignore,//_ref,
		defmult1b: _ignore,//_ref,
 
		defcom2: _ignore,//_ref,
		defunit2: _ignore,//_ref,
		defmult2: _ignore,//_ref,
		defunit2b: _ignore,//_ref,
		defmult2b: _ignore,//_ref,

		name: _str,
		epithet: _str,
		era: _num,
		descr: _ignore,//_str,
		summary: _ignore,//_str,
		brief: _ignore,//_str,

		//fx
		color: _ignore, //<red> <green> <blue>
		flag: _ignore, //“<imgfile>”
		templepic: _ignore, //Table 26 for some pic nbr values.
		mapbackground: _ignore, // “<imgfile>”
		
		labcost: _ignore,//_num,
		templecost: _ignore,//_num,

		clearsites: function(c,a,t){ modctx[t]['sites'] = [] },
		startsite:  function(c,a,t){ modctx[t]['sites'].push(argref(a)); },
		
		uwnation: _bool,
		bloodnation: _ignore, //Hint to AI player
		
		nopreach: _ignore,//_bool, //mictlan
		dyingdom: _ignore,//_bool, //mictlan
		sacrificedom: _ignore,//_bool, //mictlan
		
		nodeathsupply: _ignore,//_bool, //Abysia
		idealcold: _ignore,//_num,
		castleprod: _ignore,//_num, //MA Ulm: 25
		domkill: _ignore,//_num, //LA Ermor
		domunrest: _ignore,//_num,
		autoundead: _ignore,//_num, //LA Ermor
		
		zombiereanim: _ignore,//_bool, //LA Ermor
//~ Priests can reanimate ghouls, soulless and 
//~ longdead infantry and gain 15 points of undead 
//~ leadership per holy magic level.

		horsereanim: _ignore,//_bool, //LA Ermor
//~ Priests with holy magic of level 3 or higher can 
//~ reanimate longdead horsemen. Broken Empire 
//~ Ermor has this attribute.
		wightreanim: _ignore,//_bool, //LA Ermor
//~ Priests with holy magic of level 4 or higher can 
//~ reanimate undead Lictors. Ashen Empire Ermor 
//~ has this attribute.
		manikinreanim: _ignore,//_bool, //LA Pangaean
//~ Priests can reanimate carrion beasts, manikins 
//~ and other Pangaean undead.
//~ Nbr Site Effect
		tombwyrmreanim: _ignore,//_bool, //C'tis
//~ Priests can reanimate soulless of C'tis, longdead 
//~ of C'tis, tomb wyrms and other special C'tissian 
//~ undead. Desert Tombs C'tis has this attribute.

		startfort: _ignore,//_num,// Table 28 for a list of fort numbers
		defaultfort: _ignore,//_num,
		farmfort: _ignore,//_num,
		mountainfort: _ignore,//_num,
		forestfort: _ignore,//_num,
		swampfort: _ignore,//_num,
		uwfort: _ignore,//_num,
		deepfort: _ignore//_num
	},
	sitecommands: {
		end: function(c,a,t){
			var name = modctx[t].name; 
			modctx[t] = null;
			if (!name) throw 'unnamed '+t;
		},
		name: function(c,a,t) {
			if (modctx.site.name) delete modctx.sitelookup[modctx.site.name.toLowerCase()];
			modctx.site.name = argtrim(a);
			modctx.sitelookup[$.trim(a.s.toLowerCase())] = modctx.site;
		},
		
		clear: function(c,a,t) {
			var o = modctx.site;
			var keepstats = {
			//KEEP
				modded:1,
				id:1,
				name:1
			};
			for (var k in o) {
				if (!keepstats[k]) {
					if ($.isArray(o[k])) o[k] = [];
					else delete o[k];
				}
			}
		},
		gems: function(c,a,t){
			var pstr = modconstants[10][argnum(a)];
			if (!pstr) throw 'invalid magic index';

			//support 'SORCERY' and 'ALL'. overwrites gem income for each path. ?
			for (var p, i=0; p=pstr.charAt(i); i++) {
				modctx[t][p] = a.n2;
			}
		},
		
		path: _ignore, //function(c,a,t){ modctx[t][c] = modconstants['16'][a.n1] },
		level: _ignore,//_num,
		
		rarity: _ignore,//_num,
		loc: _ignore,//_num,

		homemon: function(c,a,t){ modctx[t]['capunits'].push(argref(a)); },
		homecom: function(c,a,t){ modctx[t]['capcommanders'].push(argref(a)); },

		mon: function(c,a,t){ modctx[t]['units'].push(argref(a)); },
		com: function(c,a,t){ modctx[t]['commanders'].push(argref(a)); },

		gold: _ignore,//_num,
		res: _ignore,//_num,
		incscale: _ignore,//_num, //See Table 21 (Scale)
		decscale: _ignore,//_num, //See Table 21 (Opposite)
		
		lab: _ignore,//_bool,
		heal: _ignore,//_num,
		curse: _ignore,//_num,
		disease: _ignore,//_num,
		horrormark: _ignore,//_num,
		holyfire: _ignore,//_num,
		holypower: _ignore,//_num,

		conjcost: _ignore,//_num,
		altcost: _ignore,//_num,
		evocost: _ignore,//_num,
		constcost: _ignore,//_num,
		enchcost: _ignore,//_num,
		thaucost: _ignore,//_num,
		bloodcost: _ignore    //_num,
	},

	
	//member data
	loadedmods: [],
	
	//will be filled before we parse mod
	itemdata: undefined,
	itemlookup: undefined,
	item: null,
	
	armordata: undefined,
	armorlookup: undefined,
	armor: null,
	
	wpndata: undefined,
	wpnlookup: undefined,
	wpn: null,
	
	spelldata: undefined,
	spelllookup: undefined,
	spell: null,

	unitdata: undefined,
	unitlookup: undefined,
	unit: null,
	
	nationdata: undefined,
	nationlookup: undefined,
	nation: null,
	
	sitedata: undefined,
	sitelookup: undefined,
	site: null,


	
	// setWpnDamageType: function(key) {
	// 	if (modctx.wpn) {
	// 		for (var k in modctx.dmg_types) {
	// 			delete modctx.wpn[k];
	// 		}
	// 		modctx.wpn[key] = '1';
	// 	} else throw 'command out of context';
	// },


	//duplicates unit. each unit has a duplicate for each nation it belongs to
	cloneUnit: function(o) {
		o.dupes = o.dupes || [o];
		
		var newo = {};
		for (k in o) { newo[k] = o[k]; }
		newo.id = o.id + (o.dupes.length * 0.01);

		o.dupes.push(newo);
		this.unitdata.push(newo);
		
		return newo;
	},
	getUnitOfType: function(o, type) {
		//find dupe of right type
		var dupes = o.dupes || [o];
		for (var i=0, dupe;  dupe= dupes[i]; i++) {
			if (dupe.type == type)
				return dupe;
		}
		return null;
	},
	getExactUnit: function(id) {
		//get unit by float id (not int)
		var o = this.unitlookup[parseInt(id)];
		var dupes = o.dupes || [o];
		for (var i=0, dupe;  dupe= dupes[i]; i++) {
			if (String(dupe.id) == String(id))
				return dupe;
		}
		return o;
	}
};


var modcom_re_multistr = /#(\w+)\s*"([^"]+)\r?$/;
var modcom_re_strcont  = /([^"]*)("|)/;
var modcom_re = /^\s*#(\w+)\s*("([^"]+)"|)(-?\d+\.?\d*|)\s*(-?\d+\.?\d*|)\s*(-?\d+\.?\d*|)\s*/;

modctx.parseMod = function(str, modnum, modname) {
	var lines = str.split('\n');
	for (var i=0; i<lines.length; i++) {
		var cstr = lines[i], linenum = i+1;
		var cmd, args;
		
		//check for open quote
		var a = modcom_re_multistr.exec(lines[i]);
		if (a) {
			cmd = a[1];
			args = { s: a[2] }
			
			//continue multi line string
			while(1) {
				i++;
				cstr += '\n' + lines[i];
				
				var a = modcom_re_strcont.exec(lines[i]);
				args.s += ' ' + a[1];
				//ended?
				if (a[2] == '"')
					break;
			}
			
		} else {
			//check single line commmand
			var a = modcom_re.exec(lines[i]);
			if (!a) continue;
			
			cmd = a[1];
			args = {
				s: a[3],
				n1: a[4],
				n2: a[5],
				n3: a[6]
			};
		}
		
		function modCmdWarning(msg) {
			emsg = "WARNING: " + msg;
			emsg += '\n'+modname+ ':' +linenum + '   ' + cstr;
			console.log(emsg);
		}
		
		//process command
		if (cmd) {
			var emsg = '';
			
			try {
				modctx.processCommand(cmd, args, modCmdWarning);
			}
			catch(err) {
				//rethrowing error loses stack trace so.. 
				//fuckit, just do it again
				if (typeof(err) != 'string') { 
					//throw(err);
					modctx.processCommand(cmd, args, function(_){});
				}					
				emsg = "ERROR: " + err;
				emsg += '\n'+modname+ ':' +linenum + '   ' + cstr;
				//emsg += '\n'+cstr;  
				console.log(emsg);
			}
			
			//debug.. attaches relevant mod commands to data (will show on details pane)
			if (DMI.Options['Show mod cmds'] && cmd && cmd!='descr') {
				var types =  ['unit', 'spell', 'wpn', 'item', 'armor', 'nation'];				
				for (var j=0, type; type=types[j]; j++) {
					if (modctx[type] && cmd!='select'+type) {
						if (emsg) modctx[type].modded += emsg.split('\n')[0] +'<br />!! ';

						if (!modctx[type].modded) modctx[type].modded = "";
						modctx[type].modded += modnum + ':' + linenum +'&nbsp;&nbsp;'+cstr+'<br />';
						//modctx.item.cmds += JSON.stringify(args)+'<br />';
					}
				}
			}
		}
	}
}




//namespace args
}( window.DMI = window.DMI || {}, jQuery ));