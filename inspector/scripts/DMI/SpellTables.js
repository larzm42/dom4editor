//namespace scope
(function( DMI, $, undefined ){
		
var MSpell = DMI.MSpell = DMI.MSpell || {};

var Format = DMI.Format;
var Utils = DMI.Utils;


//////////////////////////////////////////////////////////////
// SPELL VALUE TRANSLATION
/////////////////////////////////////////////////////////////

// used for spell fields: (damage, aoe, range, nbr_of_effects)
// each 1000 adds 1 per caster path (above base casting level)
//examples:
//  10     ->   10
//  1010   ->   10+
//  3010   ->   10+++
//  10010  ->   10+ [10/lvl]
//  -25025 ->   -25- [25/lvl]
function spellBonus(v) {
	v = parseInt(v || '0');
	
	//strip thousands
	var vbase = v % 1000;
	//count thousands
	var bonus = (v-vbase) / 1000;
	
	//baselvl is minimum caster lvl (add compulsory bonus to match ingame display) 
	//vbase = vbase + (baselvl * bonus)
	
	//support negative values
	var chr = '+',  suf = '';
	if (bonus < 0) {
		bonus = -bonus;
		chr = '-';
	}
	//10+ (4/lvl)
	if (bonus > 3)
		suf = chr+' ['+bonus+'/lvl]';
	//10+++
	else if (bonus > 0)
		suf = (new Array( bonus + 1 ).join( chr ));
	
	return String(vbase) + suf;
}

//format functions for spell fields
// using spellBonus + other field specific stuff
MSpell.format = {
	
	damage: function(v, o) {
		//damage appends some special flags inline (eg: armor piercing)
		var dmgflags = MSpell.bitfieldValues(o.spec, masks_special_dmg);
		
		v = (v=='999') ? 'death' : String(spellBonus(v));
		
		return v +' '+ Utils.renderFlags(dmgflags);
	},

	aoe: function(v, o) {
		//area of effect
		//values with special meaning
		switch (v) {
		case '0':  return (o.range == '0') ? 'caster' : 'single unit';		
		case '666': return 'entire battlefield';
		case '663': return '50% of battlefield';
		case '665': return '25% of battlefield';
		case '664': return '10% of battlefield';
		case '662': return '5% of battlefield';
		};
		
		//standard format
		return spellBonus(v);
	},
	
	range: spellBonus,	
	num: spellBonus,
	
	cloud: function(v, o) {
		//cloud that hangs on battlefield
		//stays 1 turn per 1000 added to effect value
		//eg:  effect=2005   ->   effect=5, cloud=2
		
		var neffect = parseInt(o.effect);
		
		if (neffect > 999 && (neffect < 10000 || neffect > 20000))
			return Math.floor(neffect / 1000) +' rounds';
		else
			return 0;
	},
	
	effect: function(neffect) {
		neffect = parseInt(neffect);
		if (neffect > 999 && (neffect < 10000 || neffect > 20000))
			return neffect % 1000;
		return neffect;
	}
}


//////////////////////////////////////////////////////////////
// SUMMONED UNITS
//   units for summoning spells listed here so they can be made to reference each other 
//     (effect lookups are deferred till rendering a spell) 
//   effects also look up units here (ensures types match)
/////////////////////////////////////////////////////////////

MSpell.summonsForSpell = function(o) {
	switch (MSpell.format.effect(o.effect)) {
		//combat
		case 1: //battle summons
		case 43://summon on borders
			return [ o.damage, 'combat summon' ];

		case 31://summon independent
			return [ o.damage, 'unit (indy summon)' ];
		
		
		//+commander???
		case 10038:  //send raiders
			return [ o.damage, 'unit (indy summon)' ];
		
		
		case 21: //ritual summon commander
		case 10050://send assassin
			return [ o.damage, 'combat summon (cmdr)' ];
			
		
		//rituals
		case 10001: //ritual summon
			return [ o.damage, 'unit (summoned)' ];
		
		//+commander???
		case 10037: //remote army
			return [ o.damage, 'unit (summoned)' ];
		
		case 10021: //ritual summon commander
			return [ o.damage, 'cmdr (summoned)' ];
			
		
		case 10076: //tartarian gate
			return [
				774, 'unit (summoned)',//'cmdr (summoned)', //cmdr?
				775, 'unit (summoned)',//'cmdr (summoned)', //cmdr?
				
				771, 'unit (summoned)',
				772, 'unit (summoned)',
				773, 'unit (summoned)',
				776, 'unit (summoned)',
				777, 'unit (summoned)'
			];
			
		case 10081: //globals
			return {
				80: 	/* The Kindly Ones */ [
					1296, 'cmdr (indy summon)',
					1297, 'cmdr (indy summon)',
					1298, 'cmdr (indy summon)'				
				]
			}[o.damage] || [];
		
		case 10089: //summon unique
			return {
				1:	/* Bind Ice Devil */ [
					306, 'cmdr (summoned)',
					821, 'cmdr (summoned)',
					822, 'cmdr (summoned)',
					823, 'cmdr (summoned)',
					824, 'cmdr (summoned)',
					825, 'cmdr (summoned)'
				],
				2:	/* Bind Arch Devil */ [
					305, 'cmdr (summoned)',
					826, 'cmdr (summoned)',
					827, 'cmdr (summoned)',
					828, 'cmdr (summoned)',
					829, 'cmdr (summoned)'
				],
				3:	/* Bind Heliophagus */ [
					492, 'cmdr (summoned)',
					818, 'cmdr (summoned)',
					819, 'cmdr (summoned)',
					820, 'cmdr (summoned)'
				],
				4:	/* King of Elemental Earth */ [
					906, 'cmdr (summoned)',
					469, 'cmdr (summoned)'					
				],
				5:	/* Father Illearth */ [
					470, 'cmdr (summoned)'
				],
				6:	/* Queen of Elemental Water */ [
					359, 'cmdr (summoned)',
					907, 'cmdr (summoned)',
					908, 'cmdr (summoned)'
				],
				7:	/* Queen of Elemental Air */ [
					563, 'cmdr (summoned)',
					911, 'cmdr (summoned)',
					912, 'cmdr (summoned)'
				],
				8:	/* King of Elemental Fire */ [
					631, 'cmdr (summoned)',
					910, 'cmdr (summoned)'					
				],
				9:	/* King of Banefires */ [
					909, 'cmdr (summoned)'
				],
				10:	/* Bind Demon Lord */ [
					446, 'cmdr (summoned)',
					810, 'cmdr (summoned)',
					900, 'cmdr (summoned)',
					1405, 'cmdr (summoned)'
				],
				11:	/* Awaken Treelord */ [
					621, 'cmdr (summoned)',
					980, 'cmdr (summoned)',
					981, 'cmdr (summoned)'
				],
				12:	/* Call Amesha Spenta */ [
					1375, 'cmdr (summoned)',
					1376, 'cmdr (summoned)',
					1377, 'cmdr (summoned)',
					1492, 'cmdr (summoned)',
					1493, 'cmdr (summoned)',
					1494, 'cmdr (summoned)'
				],
				13:	/* Summon Tlaloque */ [
					1484, 'cmdr (summoned)',
					1485, 'cmdr (summoned)',
					1486, 'cmdr (summoned)',
					1487, 'cmdr (summoned)'
				],
				14:	/* Release Lord of Civilization */ [
					2063, 'cmdr (summoned)',
					2065, 'cmdr (summoned)',
					2066, 'cmdr (summoned)',
					2067, 'cmdr (summoned)',
					2064, 'cmdr (summoned)',
					2062, 'cmdr (summoned)'
				]
			}[o.damage] || [ o.damage, 'cmdr (summoned)' ]; //devourer of souls (damage 1349, id 1349)

		case 10093: //mysterious summons (eater of the dead & daughter of typhon)
			return [ o.damage, 'unit (summoned)' ];
			
		case 10100: 
			return {
				1: [ //hidden in snow
					1201, 'cmdr (summoned)',
					1200, 'cmdr (summoned)',
					1202, 'unit (summoned)',
					1203, 'unit (summoned)'
				],
				2: [ //hidden in sand
					1979, 'cmdr (summoned)',
					1978, 'cmdr (summoned)',
					1980, 'unit (summoned)',
					1981, 'unit (summoned)'
				]				
			}[o.damage] || [];
	}
	return [];
}

//helpers used in effect table...

//summon (multiples of a single type) 
function summon_n(v, o) {
	//format num, eg: SummonUnit x 10+
	var nsuf = MSpell.format.num(o.nreff);
	nsuf = (nsuf != '1')  ?  ' x '+nsuf  :  '';
	
	
	//codes for generic undead
	if (o.damage == -1) return 'national soulless' +nsuf;
	if (o.damage == -2) return 'national longdead' +nsuf;
	if (o.damage == -3) return Utils.unitRef(1539)+' + '+Utils.unitRef(1540)+' + '+Utils.unitRef(1541);
	
	
	//lookup summons registed for this effect (in MSpell.summonsForSpell)
	var args = MSpell.summonsForSpell(o);
	if (!args.length) return '[summon not found]' +nsuf;
	
	return Utils.unitOfTypeRef(args[0], args[1]) +nsuf;
}
//summon list (multiple types) 
function list_summons(v, o) {
	//lookup summons
	var args = MSpell.summonsForSpell(o);
	if (!args.length) return '[summons not found]';
	
	//create array of refs
	var tokens = [];
	for (var i=0, uid, utype;  (uid= args[i]) && (utype= args[i+1]);  i+=2)
		tokens.push( Utils.unitOfTypeRef(uid, utype) );
	
	//comma separated & one per line
	return tokens.join(', <br />');
}


//used in effectlookup
var damage = 		MSpell.format.damage;
var damage_untested = 	function (v,o){ return damage(v,o) + ' ?'; }



//////////////////////////////////////////////////////////////
// SPELL EFFECT LOOKUP   <-- entry point
/////////////////////////////////////////////////////////////

MSpell.effectlookup = {
	'unknown':{	'unrecognised effect':	function (v,o){ return o.effect + '('+damage(v,o) + ' ?)'; }	},

	//BITFIELDS (MASKS LISTED IN LATER TABLES)
	// these effects can each have multiple sub-effects (based on damage value)  
	10:     function(o){ return MSpell.bitfieldValues( o.damage,    effect10_bitmasks    ); },
	11:     function(o){ return MSpell.bitfieldValues( o.damage,    effect11_bitmasks    ); },
	23:     function(o){ return MSpell.bitfieldValues( o.damage,    effect23_bitmasks    ); },
	10023:  function(o){ return MSpell.bitfieldValues( o.damage,    effect23_bitmasks    ); },
	
	
//COMBAT SPELLS (<10000)

	1:   {		'summon':			summon_n,		_hidekeys:'num,aoe,precision'	},
	2:   {		'damage':			damage	},
	3:   {		'stun':				damage	},
	4:   {		'fear':				damage	},
	7:   {		'damage over time':		damage	},
	8:   {		'restore fatigue':		damage	},
	13:  {		'heal':				damage	},
	15:  {		'returning':			'teleports to capital' },
	17:  {		//song of bravery
			'morale': 			'+5',
			'dissipates':			'-2 per turn'
	},
	//19 (phoenix power)
			//blink damage 30 = distance??
	20:  {		'blink':			'teleports to random position'	},
	21:  {		'summon cmdr':			summon_n,		_hidekeys:'num,aoe,precision'	},
	24:  {		'holy damage':			damage	},
			//magic duel???			
	27:  {		'effect':			'magic duel'	},

	28:  {		'enslave':			'take permanent control of unit',
			'servile':			'enslaved units lose commander status'
	},
	29:  {		'charm':			'take permanent control'	},
			//can dispell in combat????? 
	30:  {		'effect':			'dispel'	},
	31:  {		'summon independent':		summon_n,		_hidekeys:'num,aoe'	},
	43:  {		'summon on borders':		summon_n,		_hidekeys:'num,aoe,precision'	},
	54:  {		'polymorph into':		function(v,o){ return Utils.unitRef(o.damage) }	},
	66:  {		'paralyze':			damage	},
			//stream of life damage 25+++++ = 40%+++++ ????? something not adding up			
	72:  {		'effect':			'stream of life'	},
	73:  {		'damages magic beings':		damage	},
	81: function(o) {
		return Utils.merge({'battle enchantment': '(affects the entire battlefield for the duration of the battle)',  _hidekeys:'num,aoe,precision'},
			effects81to85_dmglookup[o.damage] || {
			'Unknown enchantment': 		damage_untested
		})
	},
			//Inanimate-only damage?
	96:  {		'damages lifeless beings':	damage	},
			//Petrify???
	99:  {		'petrify':			damage_untested	},
		
			//Damage = change in age (can be negative nreff, by extension with 10101?)
			//Damage of -10 is presumably how much it rejuvenates you.
	101:  {		'aging':			damage_untested	},
		
			//lifedrain ???
	103:  {		'drain life':			damage 	},

			//I'm guessing this is what pops illusions out of the eye of the void. Area is whole battlefield?
	105:  {		'disbelieve':			'dispells illusions',	_hidekeys:'num'	},
			
			//cleansing chime ???
			//called from some item
	106:  {		'cleansing chime':		damage_untested	},
		
			//Banishment. Damage is -13 for Cocytos, -12 for Inferno. ???
	108:  {		'banishment':			damage_untested	},
	
	500: function(o) {
		return {
		250: {	//Curse of the Desert / Dessication
			'stun':				'2-16 every round',
			'to free':			'MR check > 22'		
		},
		202: {	//Blood Vengeance ???
			//http://dom3.servegame.com/wiki/Ability#Blood_Vengeance
			'blood vengeance':		'+0'		
		}
		}[o.damage] || { 
			'invalid damage value': 		damage_untested 	
		};
	},
	599: function(o) { 
		return {
		198: {	'resist fire':			'+100%'	},
		199: {	'resist shock':			'+100%'	},
		200: {	'resist poison':		'+100%'	},
		201: {	'resist cold':			'+100%'	}
		}[o.damage] || { 
			'invalid damage value': 		damage_untested 
		};
	},
			//damage 261 ???
	600:  {		'horror mark':			damage_untested	},
			//damage 261 ???
	601:  {		'astral geyser':		damage_untested	},
	609:  {		//damage 299 ???
			'imprisoned':			'10hp of ice (protection 12)',	
			'stun':		 		'1-8 per round',
			'to free':			'destroy the ice (without weapons)'
	},
	
	
//RITUALS (>10000)

	10001:  {	'summon':			summon_n,		_hidekeys:'num,aoe'	},
	10019:  {	'effect':			'teleport'	},
			
	10021:  {	'summon cmdr':			summon_n,		_hidekeys:'num,aoe'	},
	10022:  {	'effect':			'Fate of Oedipus'	},//???
	10025:  {	'effect':			'Wish'	},//???
	10026:  {	'rebirth':			function(v,o){ return Utils.unitRef(o.damage) } 	},//******
	10030:  {	'dispel':			damage_untested	},
	10035:  {	'crossbreed':			damage_untested	},
			//+commander. chosen by nametype??			
	10037:  {	'summon remote army':		summon_n,		_hidekeys:'num,aoe',
			'plus commander':		'?'		},
	10038:  {	'send raiders':			summon_n,		_hidekeys:'num,aoe'	},
	10039:  {	'gift of reason':		damage_untested	},
			//seeking arrow (o.damage?)
	10040:  {	'remote assassination':		damage 	}, //??
	10041:  {	'remote damage (50% of army)':	damage 	}, //??
	10042: function(o) { 
		return {
		1: {	'send event':			'Black Death'	},
		2: {	'send event':			'Raging Hearts'	},
		3: {	'send event':			'Wolven Winter'	},
		4: {	'send event':			'Volcanic Eruption'	},
		5: {	'send event':			'Baleful Star'	},
		6: {	'send event':			'Rain of Toads'	},
		7: {	'send event':			'Hurricane'	},
		8: {	'send event':			'Locust Swarms'	},
		9: {	'send event':			'Blight'	},
		10: {	'send event':			'Tidal Wave'	},
		11: {	'send event':			'Monster Boar'	},
		12: {	'send event':			'Dream Horror'	}
		}[o.damage] || { 
			'invalid damage value': 		damage_untested 
		};
	},
	10044:  {	'transformation':		damage_untested	},
	
		//I tried modding some spells with this code and couldn't get it to work. It may need the remote targeting special ability, for one thing.			
	10048: function(o) { 
		return {
		0: {	'find sites':			Format.Gems('F')	},
		1: {	'find sites':			Format.Gems('A')	},
		2: {	'find sites':			Format.Gems('W')	},
		3: {	'find sites':			Format.Gems('E')	},
		4: {	'find sites':			Format.Gems('S')	},
		5: {	'find sites':			Format.Gems('D')	},
		6: {	'find sites':			Format.Gems('N')	},
		7: {	'find sites':			Format.Gems('B')	},
		8: {	'find sites':			Format.Gems('H')	},
		51: {	'find sites':			Format.Gems('FAWE')	},
		55: {	'find sites':			Format.Gems('FAWESDNBH')	}
		}[o.damage] || { 
			'invalid damage value': 		damage_untested 
		};
	},
	10049:  {	'wind ride':			damage_untested	},
	10050:  {	'send assassin':		summon_n,		_hidekeys:'num,aoe'	},
	10053:  {	'vengeance of the dead':	damage_untested	},
	10057:  {	'mind hunt':			damage_untested	},
	10057:  {	'manifestation':		damage_untested	},
	10063:  {	'build fort':			function(v,o){ return DMI.modconstants[28][o.damage]; } 	},
	10064:  {	'leprosy':			damage_untested	},

			// ""Damage probably doesn't do anything.???
			// ""We should check if effect 68 summons random animals in combat.
	10068:  {	'summon animals':		damage_untested	},
			
			//is this damage right??
	10070:  {	'reduce castle defence':	damage	}, //crumble
			
			//""Damage is 100, you could try changing it.???
	10075:  {	'raven feast':			damage 	},
			
			//does it select unit by id or by name "tartarian *" ???
	10076:  {	'summons one of':		list_summons,		_hidekeys:'num,aoe'	},
			
			//astral travel???
	10077:  {	'army teleport':		damage_untested	},
			
			//faery trod???
	10079:  {	'army travel to forest':	damage_untested	},
			
			//gateway???
	10080:  {	'gateway':			damage_untested	},

	10081: function(o) { //global
		return Utils.merge({ 'global enchantment': ' ',  _hidekeys:'num,aoe' },
			effects81to85_dmglookup[o.damage] || {
				'invalid damage value': 		damage_untested
		})
	},
	10082: function(o) { //dome
		return effects81to85_dmglookup[o.damage] || {
			'invalid damage value': 		damage_untested
		}
	},
	10084: function(o) { //dome
		return effects81to85_dmglookup[o.damage] || {
			'invalid damage value': 		damage_untested
		}
	},
	10085: function(o) { //scrying
		return effects81to85_dmglookup[o.damage] || {
			'invalid damage value': 		damage_untested
		}
	},
	10086: function(o) { //melancholia. oO
		return effects81to85_dmglookup[o.damage] || {
			'invalid damage value': 		damage_untested
		}
	},
			//selects unit by id.. right???
			//works in combat???
	10089:  {	'summon unique':		list_summons,		_hidekeys:'num,aoe'	},
		
			//Stygian Paths???
	10091:  {	'Stygian paths':		damage_untested	},
			
			//Precision is -1. This may mean something. ???
	10092:  {	'imprint souls':		damage_untested	},
			
	10093: {	'summon unique':		list_summons,		_hidekeys:'num,aoe'	},

			//damage 999 - may mean something. ???
	10094:  {	'beckoning':			damage_untested	},
			
			//Cloud Trapeze???
	10095:  {	'cloud trapeze':		damage_untested	},
			
			//No damage or # of effects to change. ???
	10098:  {	'winged monkeys':		damage_untested	},
			
			// False alarm earlier, this data is right. I don't know what's going on.
			// No damage or nreff of effects fields to change.
			// No data in the additional unknown bits to the right :(
			// Probably this means spells of the Hidden in Snow type are not modable.
			// A search through the executable finds no "Hidden in Snow" label where the additional effects of this spell might be stored.
			// The subsidiary effects, unfrozen lord, 4 unfrozen warriors, and 14 unfrozen (invisible spells used internally) point to eachother in what I believe is the "secondary effect" field for spells.
			// However, "Hidden in Snow" itself does not such thing.
			// It's possible that the damage of 1 refers to a spell-resolution chain somewhere else, but spells of this type do not at the moment appear modable.
	10100: function(o) { 
							var a= MSpell.summonsForSpell(o);
		return {
		1: {	//hidden in snow
			'summon commander': 		Utils.unitOfTypeRef(a[0],a[1]),			_hidekeys:'num,aoe',
			'commanders': 			Utils.unitOfTypeRef(a[2],a[3]) + ' x (0-2)',
			'units':			Utils.unitOfTypeRef(a[4],a[5]) + ' x (2-24)',
			'units ':			Utils.unitOfTypeRef(a[6],a[7]) + ' x (2-24)',
			'prefers':			'Magic, Cold, Order scales'
		},
		2: {	//hidden in sand
			'summon commander': 		Utils.unitOfTypeRef(a[0],a[1]),			_hidekeys:'num,aoe',
			'commanders': 			Utils.unitOfTypeRef(a[2],a[3]) + ' x (0-2)',
			'units':			Utils.unitOfTypeRef(a[4],a[5]) + ' x (2-24)',
			'units ':			Utils.unitOfTypeRef(a[6],a[7]) + ' x (2-24)',
			'prefers':			'Magic, Heat, Order scales'
		}
		}[o.damage] || { 
			'invalid damage value': 		damage_untested 
		};
	},
			//Damage = change in age (can be negative nreff, by extension with 10101?)
			//Damage of -10 is presumably how much it rejuvenates you.
	10101:  {	'aging':			damage_untested	},
		
			//Damage value (9) probably means something. Has strange data in unknown flags. ???
	10102:  {	'horror seed':			damage_untested	},
		
			//Has strange data in unknown flags.
	10110:  {	"dreams of R'lyeh":		damage_untested	},
	10112:	{	"damages caster":		damage }
};

//////////////////////////////////////////////////////////////
// EFFECTS 81-85 (10081-10085)
//    damage value refers to this table
/////////////////////////////////////////////////////////////

var effects81to85_dmglookup = {
	/*
	effect no
	 81:     //Battlefield Enchantments
	 10081:  //globals
	 10082:  //domes 1
	 10084:  //domes 2
	 10085:  //scrying
	 10086:  //vice admiral of crazy town
	*/		
	0: {		'effect':		"Scrying"	}, //assert 10085
	1: {		'effect':		"Storm"		}, //assert 81
	2: {		'effect':		"Friendly Currents"	}, //assert 81
	3: {		'effect':		"Astral Tempest"	}, //assert 81
	4: {		'effect':		"Rigor Mortis"	}, //assert 81
	5: {		'effect':		"Soul Drain"	}, //assert 81
	6: {		'effect':		"Heat from Hell"	}, //assert 81
	7: {		'effect':		"Rain"		}, //assert 81
	8:	{/*'Battlefield Enchantment':' ', _hidekeys:'num,aoe'*/},//used as empty effect in cbm
	9: {		'effect':		"Grip of Winter"	}, //assert 81
	10: {		'effect':		"Foul Air"	}, //assert 10081
	11: {		'effect':		"Acid Storm"	}, //assert 81
	12: {		'effect':		"Wailing Winds"	}, //assert 81
	13: {		'effect':		"Growing Fury"	}, //assert 81
	14: {		'effect':		"The Wrath of God"	}, //assert 10081
	15: {		'effect':		"The Eyes of God"	}, //assert 10081
	16: {		'effect':		"Perpetual Storm"	}, //assert 10081
	17: {		'effect':		"Eternal Pyre"	}, //assert 10081
	18: {		'effect':		"Wrath of the Sea"	}, //assert 10081
	19: {		'effect':		"Arcane Nexus"	}, //assert 10081
	20: {		'effect':		"Forge of the Ancients"	}, //assert 10081
	21: {		'effect':		"Solar Brilliance"	}, //assert 81
	24: {		'effect':		"Enchanted Forest"	}, //assert 10081
	25: {		'effect':		"Fire Storm"	}, //assert 81
	26: {		'effect':		"Purgatory"	}, //assert 10081
	27: {		'effect':		"Gift of Nature's Bounty"	}, //assert 10081
	28: {		'effect':		"Sea of Ice"	}, //assert 10081
	29: {		'effect':		"Burden of Time"	}, //assert 10081
	30: {		'effect':		"Gift of Health"	}, //assert 10081
	32: {		'effect':		"Dark Skies"	}, //assert 10081
	33: {		'effect':		"Well of Misery"	}, //assert 10081
	34: {		'effect':		"Wrathful Skies"	}, //assert 81
	35: {		'effect':		"Riches from Beneath"	}, //assert 10081
	36: {		'effect':		"Divination"	}, //assert 10086
	36: {		'effect':		"Astral Window"	}, //assert 10085
	37: {		'effect':		"Astral Projection"	}, //assert 10085
	40: {		'effect':		"Light of the Northern Star"	}, //assert 81
	41: {		'effect':		"Second Sun"	}, //assert 10081
	42: {		'effect':		"The Looming Hell"	}, //assert 10081
	43: {		'effect':		"Ghost Ship Armada"	}, //assert 10081
	44: {		'effect':		"Illwinter"	}, //assert 10081
	45: {		'effect':		"Foul Vapors"	}, //assert 81
	46: {		'effect':		"Mother Oak"	}, //assert 10081
	47: {		'effect':		"Maelstrom"	}, //assert 10081
	48: {		'effect':		"Earth Blood Deep Well"	}, //assert 10081
	49: {		'effect':		"Gale Gate"	}, //assert 10081
	50: {		'effect':		"Lure of the Deep"	}, //assert 10081
	51: {		'effect':		"Mechanical Militia"	}, //assert 10081
	52: {		'effect':		"Guardians of the Deep"	}, //assert 10081
	53: {		'effect':		"Fata Morgana"	}, //assert 10081
	54: {		'effect':		"Stellar Focus"	}, //assert 10081
	55: {		'effect':		"Haunted Forest"	}, //assert 10081
	56: {		'effect':		"Utterdark"	}, //assert 10081
	57: {		'effect':		"Astral Corruption"	}, //assert 10081
	58: {		'effect':		"Ark"		}, //assert 81
	59: {		'effect':		"Thetis' Blessing"	}, //assert 10081
	60: {		'effect':		"Dome of Flaming Death"	}, //assert 10082
	61: {		'effect':		"Dome of Solid Air"	}, //assert 10084
	62: {		'effect':		"Frost Dome"	}, //assert 10082
	64: {		'effect':		"Dome of Arcane Warding"	}, //assert 10082
	66: {		'effect':		"Forest Dome"	}, //assert 10084
	67: {		'effect':		"Dome of Corruption"	}, //assert 10082
	68: {		'effect':		"Blood Rain"	}, //assert 81
	69: {		'effect':		"Iron Walls"	}, //assert 10082
	70: {		'effect':		"Mist"		}, //assert 81
	71: {		'effect':		"Mists of Deception"	}, //assert 81
	72: {		'effect':		"Melancholia"	}, //assert 10086
	75: {		'effect':		"Wild Hunt"	}, //assert 10081
	77: {		'effect':		"Darkness"	}, //assert 81
	78: {		'effect':		"Shark Attack"	}, //assert 81
	79: {		'effect':		"Strands of Arcane Power"	}, //assert 10081
	80: {		'summon independents':	list_summons, 	_hidekeys:'num,aoe' /* the kindly ones */ }, //assert 10081
	81: {		'effect':		"Soul Gate"	}, //assert 10081
	82: {		'effect':		"Carrion Woods"	}, //assert 10081
	83: {		'effect':		"Demon Cleansing"	}, //assert 81
	84: {		'effect':		"Relief"	}, //assert 81
	85: {		'effect':		"Quagmire"	} //assert 81
};


//////////////////////////////////////////////////////////////
// EFFECT 10
//    damage value is a bitfield built from these masks
/////////////////////////////////////////////////////////////
var flags = Utils.renderFlags;
var effect10_bitmasks = {
	1:		{	'effect':		'blessing'	},
	2:		{	'effect':		'luck'		},
	4:		{ 	'precision': 		'+5'		},
	8:		{ 	'air shield': 		'80%'		},
	16:		{ 	'barkskin': 		'10 protection (+1 if already over 10)', 
				'resist fire': 		'-25%' 
	},
	32:		{	'regeneration': 	'10%'		},
	64:		{ 	'all armor': 		'+3 protection'	}, //legions of steel 
	128:		{ 	'strength bonus': 	'+4'		},
	256:		{ 	'berserk': 		'+0'		},
	512:		{ 	'transfer damage': 	'to blood slaves within 8'	},//pain transfer 
	1024:		{	'resist fire': 		'+50%' 	},
	2048:		{	'resist shock': 	'+50%'	},
	4096:		{	'resist cold': 		'+50%'	},
	8192:		{	//breath of winter
				'chill aura': 		'+6', 
				'resist cold': 		'+100%'
	},
	16384:		{	'heat aura': 		'+3'		},
	32768:		{	'morale': 		'+2'		},//sermon of courage
	65536:		{	'stoneskin': 		'15 protection (+2 if already over 15)', 
				'resist cold': 		'-50%'		
	},
	131072:		{	'effect':		'flight'	},
	262144:		{	'effect':		'quickness'	},
	524288:		{	'melee weapons':	'armor piercing' +flags('not magic damage')	},//weapons of sharpness
	1048576:	{	'effect':		'astral weapon'			},//???
	2097152:	{	'effect':		'dead raised as soulless'	},//life after death
	4194304:	{	'effect':		'holy avenger'			},//lacks detail
	8388608:	{	//flaming arrows
				'missile weapons': 	'count as magic weapons', 
				'bonus damage': 	'+8' +flags('armor piercing', 'fire'), 
				'affects':		'crossbows, bows, slings, javelins, shuriken, boulders, etc..'		
	},
	16777216:	{	'resist poison': 	'+50%'			},
	33554432:	{	'effect':		'become prophet'	},//???
	67108864:	{	'magic resistance': 	'+4'			},
	134217728:	{	'effect':		'etherealness'		},
	268435456:	{	'ironskin': 		'20 protection (+3 if already over 20)', 
				'resist shock': 	'-75%'		
	},
	536870912:	{	'effect':		'communion master'	},
	1073741824:	{	'effect':		'communion slave'	},
	2147483648:	{	'effect':		'no obvious effect???'	}
};

//////////////////////////////////////////////////////////////
// EFFECT 23 (10023)
//    damage value is a bitfield built from these masks
/////////////////////////////////////////////////////////////

var effect23_bitmasks = {	
	1:		{	'twist fate': 		'negates first hit'		},
	2:		{	'invulnerability': 	'25 protection', 
				'resist poison': 	'-100%'		
	},
	4:		{	'haste': 		'double movement speed'		},
	8:		{	'mossbody': 		'+10-20 protection, bursts when hurt ending protection', 
				'burst damage': 	'1' +flags('armor negating', 'poison'), 
				'burst area': 		'4'		
	},
	16:		{	'fear': 		'+0'		},//???	
	32:		{	'defence': 		'+5'		},//water shield
	64:		{	'effect':		'trample'	},//???
	128:		{	'magic bonus': 		Format.Paths('+FAWESDNBH')		},//power of the spheres
	256:		{	'effect':		'no obvious effect**?'	},
	512:		{	'effect':		'no obvious effect**?'	},
	1024:		{	'effect':		'become pretender'	},//???
	2048:		{	'soul vortex': 		'drains nearby life every round', 
				'life drain': 		'1' +flags('armor negating'), 
				'drain area': 		'5'		
	},
	4096:		{	//earth power
				'magic bonus': 		Format.Paths('+E'), 
				'reinvigoration': 	'+4'		
	}, 
	8192:		{	'magic bonus': 		Format.Paths('+A')+' (only during storm)'		},//storm power 
	16384:		{	'charge body': 		'Discharges when first struck, damaging attacker and self.', 
				'discharge damage': 	'20' +flags('armor negating', 'shock')		
	},
	32768:		{	'fire shield': 		'burns attackers before striking', 
				'counter damage': 		'7+' +flags('-weapon length', 'armor piercing', 'fire')		
	},
	65536:		{	'mistform': 		'max 1 damage when hit', 
				'cancelled by': 	'magic damage or by a hit that does 25 damage or 25%hp'		
	},
	131072:		{	//hell power
				'magic bonus': 		'+2 to all paths',  
				'protection': 		'+4',        
				'def': 			'+4', 
				'attack': 		'+4', 
				'strength': 		'+4', 
				'morale': 		'+4', 
				'precision': 		'+4', 
				'action points': 	'+4', 
				'reinvigoration': 	'+4', 
				'magic resistance': 	'+2', 
				'attracts horrors': 	'20% chance each turn'		
	},
	262144:		{	//fire power
				'magic bonus': 		Format.Paths('+F'), 
				'resist fire': 		'+50%'		
	}, 
	524288:		{	'magic bonus': 		Format.Paths('+W')		},//water power 
	1048576:	{	'magic bonus': 		Format.Paths('+N')		},//strength of gaia 
	2097152:	{	'effect':		'no obvious effect**?'		},
	4194304:	{	'twiceborn': 		'upon death in friendly dominion, revives in capital', 
				'revives as': 		Utils.ref('unit 299', 'Wight Mage')
	},
	8388608:	{	'returning': 		'teleports to capital when wounded'		},
	16777216:	{	'mirror image': 	'Striking images has no effect. Striking the true target cancels all images.', 
				'nreff of images': 	'2+ (2 per lvl)'		
	},
	33554432:	{	//unholy power
				'attack bonus': 	'+4', 
				'action points': 	'+4'		
	}, 
	67108864:	{	'inner sun': 		'explodes on death', 
				'explosion damage': 		'15' +flags('undead and demons only', 'MR negates'), 
				'explosion area': 	'35'		
	},
	134217728:	{	'phoenix pyre': 	'explodes on death', 
				'explosion damage': 		'50' +flags('armor piercing', 'half fire damage', 'half physical'), 
				'explosion area': 	'50', 
				'phoenix revival': 	'if under 100 fatigue, revives in a random position. costs 31-50 fatigue'
	},
	268435456:	{	'astral shield': 	'counters attackers before striking', 
				'counter paralyze':	'12+' +flags('-weapon length*2', 'armor negating', 'MR negates')
	},
	536870912:	{	'lifeless regeneration':'10%' 		},//regrowth
	1073741824:	{	'dragon master': 	'3 effects per summoning: '
							+Utils.ref('spell 424', 'Summon Cave Drake') + ', '
							+Utils.ref('spell 417', 'Summon Fire Drake') + ', '
							+Utils.ref('spell 422', 'Summon Ice Drake') + ', '
							+Utils.ref('spell 423', 'Summon Sea Serpent') + ', '
							+Utils.ref('spell 420', 'Summon Wyvern')
	},
	2147483648:	{	'effect':		'no obvious effect**?'		}
}


//////////////////////////////////////////////////////////////
// EFFECT 11
//    damage value is a bitfield built from these masks
/////////////////////////////////////////////////////////////

var effect11_bitmasks = {	
	1:		{	'effect':		'disease'	},
	2:		{	'effect':		'curse'		},
	4:		{	'effect':		'starvation'	},//???
	8:		{	'plague': 		'spreads to adjacent units each round', 
				  'hitpoint loss': 	'1-2 per round', 
				  'fatigue': 		'1-4 per round'		
	},
	16:		{	'maggot damage': 	'??' +flags('armor negating', 'damage over time')		},
	32:		{	'curse of stones': 	'each step costs +1 action point', 
				  'per step': 		'+1-4 fatigue', 
				  'per attack':		'+1-6 fatigue'		
	},
	64:		{	'entangle': 		'prevents movement and attacks', 
				  'to free': 		'strength check > 18', 
				  'modifiers': 		'-1 in forest, -growth scale, +1 in wastes'		
	},
	128:		{	'confusion': 		'50% chance to move randomly and attack an enemy or friend within range'		},
	256:		{	'decay': 		'ages by 5 years per round.', 
				  'if diseased':	'-5hp per round'		
	},
	512:		{	'on fire': 		'??' +flags('armor negating', 'damage over time')		},
	1024:		{	'destruction': 		'destroys non-magical armor'		},
	2048:		{	'rust': 		'Non-magical armor has a 50% chance of breaking when hit.', 
				  'lasts': 		'4 rounds'		
	},
	4096:		{	'affliction': 		Utils.afflictionRef('Blindness') },
	8192:		{	'effect':		'no obvious effect**?'		},
	16384:		{	'earth grip': 		'immobilisation', 
				  'to free':		'strength check > 22'		
	},
	32768:		{	'effect':		'no obvious effect**?'		},	
	65536:		{	'bonds of fire': 	'immobilisation', 
				  'to free': 		'morale check > 20', 
				  'on freedom':		'1-4 damage (armor negating)'	
	},
	131072:		{	'false fetters': 	'prevents movement and attacks', 
				  'to free': 		'MR check > 20'		
	},
	262144:		{	'affliction':		Utils.afflictionRef('Limp') },
	524288:		{	'affliction': 		Utils.afflictionRef('Eyeloss') },//???
	1048576:	{	'affliction': 		Utils.afflictionRef('Weakened') },//???
	2097152:	{	'affliction': 		Utils.afflictionRef('Battle Fright') },//???
	4194304:	{	'affliction': 		Utils.afflictionRef('Mute') },//???
	8388608:	{	'affliction': 		Utils.afflictionRef('Chest Wound') },
	16777216:	{	'affliction': 		Utils.afflictionRef('Crippled') },
	33554432:	{	'affliction': 		Utils.afflictionRef('Feeble Minded') },
	67108864:	{	'affliction': 		Utils.afflictionRef('Never Healing Wound') },//???
	134217728:	{	'slimed': 		'Halves attack, defence and action points.'		},
	268435456:	{	'frozen': 		'+2-12 fatigue per turn until thawed', 
				  'attack': 		'-3', 
				  'defence': 		'-3', 
				  'ap': 		'halved'	
	},
	536870912:	{	'effect':		'web**?'		},
	1073741824:	{	'affliction': 		Utils.afflictionRef('Arm Loss') },//???
	2147483648:	{	'effect':		'???'			}
}



//////////////////////////////////////////////////////////////
// SPECIAL
//    every spell has a 'special' value built from these masks
/////////////////////////////////////////////////////////////

var _original_masks_special = {
	1:		'add strength',
	2:		'2',//unused
	4:		'4',//unused
	8:		'only affects undead and demons',
	16:		'only affects magic beings',
	32:		'fire',
	64:		'armor piercing',
	128:		'armor negating',
	256:		'256',//unused
	512:		'cold',
	1024:		'1024',//unused
	2048:		'shock',
	4096:		'MR negates',
	8192:		'poison',
	16384:		'misses no nothing',//'missile'???
	32768:		'only affects sacred',
	65536:		'amphibians are immune',
	131072:		'mindless are immune',
	262144:		'only affects hostile',
	524288:		'undead are immune',
	1048576:	'resisted by defense skill',
	2097152:	'physical',
	4194304:	'only affects friendly units',
	8388608:	'works underwater',
	16777216:	'MR negates easily',
	33554432:	'only works underwater',
	67108864:	'only affects undead',
	134217728:	'134217728',//unused
	268435456:	'targets remote province',
	536870912:	'lifeless are immune',
	1073741824:	'bloodletting',//...demons are immune???'
	2147483648:	'2147483648'//unused
}

//following tables affect where masks are rendered and in what order
//  values are duplicated from above

//these ones show up next to damage (or not at all)
var masks_special_dmg = {
	1:	'add strength',
	128:	'armor negating',
	64:	'armor piercing',
	32:	'fire',
	512:	'cold',
	2048:	'shock',
	8192:	'poison',
	2097152:'physical'
	//16384:	'misses no nothing'//'ignore default damage type???'
}

//these ones show up right at the bottom (and only for the main effect)
MSpell.masks_final = {
	8388608:	'cannot be cast underwater', //flipped meaning
	33554432:	'only works underwater',
	268435456:	'targets remote province'
}

//updated table (about to be filled out from original)
MSpell.masks_special = {
	2:		'only affects demons'//hope this doesn't overwrite anythign important
};
//copy values from original that aren't in another table
for (var k in _original_masks_special) 
	if (!MSpell.masks_special[k] && !masks_special_dmg[k] && !MSpell.masks_final[k]) 
		MSpell.masks_special[k] = _original_masks_special[k];

//update a bitfield to work with new flags
MSpell.updateSpecialBitfield = function(v) {
	
	//'only affects undead and demons'  +  'undead immune'  = 'only affects demons'
	if (v & 8  &&  v & 524288) v ^= (8 | 524288 | 2);
	
	//flip 'works underwater' bit (now its 'cannot be cast underwater')
	v ^= 8388608; 
	
	return v;
}

MSpell.worksUnderwater = function(spell) {
       	return ((!spell.uw || spell.uw == 'y')); 
}
MSpell.worksOnDryLand = function(spell) { 
	return ((!spell.land || spell.land != 'n')); 
}


//namespace args
}( window.DMI = window.DMI || {}, jQuery ));
