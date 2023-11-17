### Mistakes were made 
```
(
Ndef(\mistakes_were_made,{
	var sig; 
	p=9/7**(..16); c=LinCongC; d=(c.ar(c.ar(p.atan)**2*4)**4*8).abs;
	t=c.ar(c.ar(d/p.atan).round(1/d)**4*d*8).sin;
	f=TRand.ar(0,64,t).ceil.midicps;
	sig=Splay.ar(
		LeakDC.ar(Pluck.ar(
			SinOsc.ar(f*p*p.log,0,SinOsc.ar(f*p*p.log/2)*2*d).sin,t,0.1,t.exprange/d/p,
			(t**1.5*f)*(d/p.scramble),(p*(1-t.exprange)).sin.exprange(0.9999,0.5)
		)),SinOsc.ar(f/d@0)).tanh*0.8;
	Out.ar(\out.kr(0),sig)
}))
```

### Grains, Daily.

```
(play{ /* #DailyGrain #SuperCollider */
	m=ar(n=StandardL,ar(LFDNoise1,1).exprange*8+0s,2).exprange(_,_);
	ar(LeakDC,
		ar(GrainFM,2,ar(TDuty,m.(0.5,8)/f={ar(n,m.(1,128),m.(1,3)).exprange(40,5e3)}),
			m.(0.1,4)/f,f,m.(1/4,4)*f,m.(1/4,4),ar(n,f/2)/2,
			sendCollection(Buffer,s,perc(Env,0s1,1).discretize),1024)).softclip} 
)
```

### Doomsday Patch
```
({ // #D👀M #SuperCollider 
	c=Buffer.allocConsecutive(12,s,4096);{|n|c[n].sine1(1/(1..n+1))}!12;
	Splay.ar(LeakDC.ar(
			VOsc.ar(
				i={LFDNoise1.kr(3!2).range(c.first.bufnum,c.last.bufnum-1)},
				f=(v=SinOsc.ar(5))*0.1/4+1*Duty.kr(1,0,Dseq([87,82,78,73],inf)),
				VOsc.ar(i,p=TChoose.kr(TDuty.kr(1!2),2**(..4)/2).lag3(1/8)*f,0,
					LFDNoise1.ar(3!2).lincurve(-1,1,0,1,LFDNoise1.kr(1,8))*p.sqrt),
			-12.dbamp)
		),v)}.play(s,\fade,12) 
)
```
### Spa Saw Shower Wash
```
(Ndef(\spa_saw_shower_wash,{
	c=Buffer.alloc(s,4096);c.sine1(1/(1..128));
	w=LFDNoise3.ar(_).lincurve(-1,1,80,6880,LFDNoise3.kr(1,6));
	o=Splay.ar(Osc.ar(c,
		f=SinOsc.ar({rrand(3.3,4.4)}!6)*0.01+1*[82,123,196,147,41,55],
		Osc.ar(c,f*3,0,SinOsc.ar(1/{rrand(33,45)}!6,0,pi)),1/5),
	SinOsc.ar(1/pi));
	o=[BHiPass4.ar(o,w.(1/7))+BPF.ar(o,w.(1/5))+MoogFF.ar(o,w.(1/3))].sum;
	4.do{o=AllpassC.ar(o,1,{rrand(1/16,1/32)}!2)};
	Out.ar(\out.kr(0),o);
}) /* #SuperCollider #Friday #SpaMusic #Washy #Soap */ )
```
### Dodgy Suspicious Jazz
```
(Ndef(\suspect_jazz,{
	x=HenonL.ar(1+LFDNoise1.kr(3).exprange*8).fold2;
	t={|n|PulseDivider.ar(x,8,7-n)}!8;
	d=1.5+x.exprange;
	e={|c|Env.perc(0s4,d,1,c).ar(0,t)};
	f=Demand.ar(t,0,
		Drand(c=[0,3,7,-2],inf)+Dxrand(48+flat({|o|o*12+c}!3),inf)
	).midicps;
	o=SinOsc.ar(
		BrownNoise.ar(0.015,1)*f,
		BrownNoise.ar(1/4*SinOsc.ar([3.25,0.5,2]*f,0,e.(-28))).sum,
		e.(-6)*SinOsc.ar(d+2,0,0.25,0.5)
	);
	Out.ar(\out.kr(0),Splay.ar(o,1/4));
}))
```
### Auto Collage of your .recordingsDir
```
(/* #SuperCollider #AutoCollage */ 
fork{f=SoundFile.collect(Platform.recordingsDir+/+"*.wav");0.2.wait};
fork{
	z=s.sampleRate;0.2.wait;
	loop{
		c=f.choose;a=c.numFrames;r=rand(a-z).clip(0,a);n=exprand(z/8,z*2);
		b=read(Buffer,s,c.path,r,n,play(b),0);
		do(60,{"|".post;(n/z/59).wait});0.1.wait;nl(Post)
	}}
);
```

### Simple sloppy reverb

```
(Ndef(\sloppy_verb,{
	var clap,slap,rnd;
	rnd = {LFDNoise3.ar(137,0.25,1)};
	clap=CuspN.ar*Env(flat([0,1]!4)++0,[0,0s/9,0,0s/9,0,0s7,0,1],
		[0,-1,0,-1,0,-1,0,-20]).ar(0,Dust.ar(rnd)
	);
	clap=BPF.ar(clap,[1210,2440,1480],[0.3,1,0.4]);
	clap=Splay.ar(clap);
	slap=clap;
	16.do{slap=AllpassC.ar(HPF.ar(slap,rnd*240),0.2,rnd*0.0044,rnd*2)};
	Out.ar(\out.kr(0),Splay.ar(-8.dbamp*slap+clap));
}) /* // #SuperCollider, #StupidSimple, #Reverb #Exercise */)
```

### Cheap Singing Synth 

A little bigger thing that i didn't manage to squeeze down to a more reasonable size

```
(
	var tab, dur, env, trg, freq, voc, snd, mel;

/*

	Losers Union
	A Stupid Simple Silly Stupid Singing Ting

	tab	is the formant table, sorted by			freq	Fundamental frequency
		vocal range. to sort by vowel try		voc     maps freq to a row in tab and pulls
		flatten({|n|tab[n,n+5..]}!5,1)				out the data. \vocLo and \vocHi sets
	dur 	generates duration data					the range into the formant table
	trg	trigger timed by dur data			voc[0] 	the 5 formants freqs.
	env 	adsr envelope					voc[1] 	the bandwiths.
	mel	gets some notes to be used for melody		voc[2] 	and the amplitudes.

	todo:	( ) Learn to code
		( ) Rewrite the mess that is the Ndef below

*/

tab=[

//	freq (Hz),			bw (Hz),		amp (dB)
	[[600,1040,2250,2450,2750],	[60,70,110,120,130],	[0,-7,-9,-9,-20]	], // a Bass
	[[400,1620,2400,2800,3100],	[40,80,100,120,120],	[0,-12,-9,-12,-18]	], // e
	[[250,1750,2600,3050,3340],	[60,90,100,120,120],	[0,-30,-16,-22,-28]	], // i
	[[400,750,2400,2600,2900],	[40,80,100,120,120],	[0,-11,-21,-20,-40]	], // o
	[[350,600,2400,2675,2950],	[40,80,100,120,120],	[0,-20,-32,-28,-36]	], // u

	[[650,1080,2650,2900,3250],	[80,90,120,130,140],	[0,-6,-7,-8,-22]	], // a Tenor
	[[400,1700,2600,3200,3580],	[70,80,100,120,120],	[0,-14,-12,-14,-20]	], // e
	[[290,1870,2800,3250,3540],	[40,90,100,120,120],	[0,-15,-18,-20,-30]	], // i
	[[400,800,2600,2800,3000],	[40,80,100,120,120],	[0,-10,-12,-12,-26]	], // u
	[[350,600,2700,2900,3300],	[40,60,100,120,120],	[0,-20,-17,-14,-26]	], // o

	[[660,1120,2750,3000,3350],	[80,90,120,130,140],	[0,-6,-23,-24,-38]	], // a Countertenor
	[[440,1800,2700,3000,3300],	[70,80,100,120,120],	[0,-14,-18,-20,-20]	], // e
	[[270,1850,2900,3350,3590],	[40,90,100,120,120],	[0,-24,-24,-36,-36]	], // i
	[[430,820,2700,3000,3300],	[40,80,100,120,120],	[0,-10,-26,-22,-34]	], // o
	[[370,630,2750,3000,3400],	[40,60,100,120,120],	[0,-20,-23,-30,-34]	], // u

	[[800,1150,2800,3500,4950],	[80,90,120,130,140],	[0,-4,-20,-36,-60]	], // a Alto
	[[400,1600,2700,3300,4950],	[60,80,120,150,200],	[0,-24,-30,-35,-60]	], // e
	[[350,1700,2700,3700,4950],	[50,100,120,150,200],	[0,-20,-30,-36,-60]	], // i
	[[450,800,2830,3500,4950],	[70,80,100,130,135],	[0,-9,-16,-28,-55]	], // o
	[[325,700,2530,3500,4950],	[50,60,170,180,200],	[0,-12,-30,-40,-64]	], // u

	[[800,1150,2900,3900,4950],	[80,90,120,130,140],	[0,-6,-32,-20,-50]	], // a Soprano
	[[350,2000,2800,3600,4950],	[60,100,120,150,200],	[0,-20,-15,-40,-56]	], // e
	[[270,2140,2950,3900,4950],	[60,90,100,120,120],	[0,-12,-26,-26,-44]	], // i
	[[450,800,2830,3800,4950],	[70,80,100,130,135],	[0,-11,-22,-22,-50]	], // o
	[[325,700,2700,3800,4950],	[50,60,170,180,200],	[0,-16,-35,-40,-60]	], // u
	];

Ndef(\aStupidSimpleSillySingTing,{
	dur=Duty.kr(x=LorenzL.ar(24).sin.range(1,4)/Diwhite(2,10),0,x);
	trg=TDuty.kr(dur);
	mel=(26..42).degreeToKey(Scale.choose);
	freq=(Demand.kr(trg,0,Dseq(mel.scramble,inf)));
	env=adsr(Env,dur/48,1/4,1,dur/2,1.2,-4,0.01).kr(0,Trig.kr(trg,dur*(LFNoise2.ar(4).range(0.5,1))));
	voc=SelectXFocus.kr(freq.linlin(mel.first,mel.last,\vocLo.kr(0),\vocHi.kr(tab.size)),tab,0.9,true);
	snd=Splay.ar(BPF.ar(
		SyncSaw.ar(
			LFNoise2.ar(voc@1,freq/(voc@0),freq.midicps),
			PinkNoise.ar((1-env),1)*LFNoise2.kr((voc@1),(env-dbamp(voc@2))/2,voc@0)),
		env-perc(Env,0,1/(2**(..5))/8,0.5).kr(0,trg)*voc@0+30,((voc@1)/(voc@0))
	).softclip*(voc@2).dbamp*env,LFTri.ar(freq)/2);
	Out.ar(\out.kr(0),snd)})
);

/* Mariah Carey  */	Ndef(\aStupidSimpleSillySingTing).set(\voLo,0,\vocHi,24);
/* Soprano       */	Ndef(\aStupidSimpleSillySingTing).set(\voLo,20,\vocHi,24);
/* Alto          */	Ndef(\aStupidSimpleSillySingTing).set(\voLo,15,\vocHi,19);
/* Countertenor  */	Ndef(\aStupidSimpleSillySingTing).set(\voLo,10,\vocHi,14);
/* Tenor         */	Ndef(\aStupidSimpleSillySingTing).set(\voLo,5,\vocHi,9);
/* Bass          */	Ndef(\aStupidSimpleSillySingTing).set(\voLo,0,\vocHi,4);
```

### Little Rhythm Thing using the VarSaw uGen

```
(play{ /* #Silly #Rhythm #SuperCollider */
	ar(Splay,(e=hanWindow(
		ar(VarSaw,(p=(2**(..6))/4)/4,0,
			(r=ar(w=SinOsc,1/(p.atan+2.5)).exprange)+0.1)**ar(w,7).exprange+0.1)**(1+(r*4)))
	*ar(w,f=110*p*p.log,ar(w,f/p)*e*ar(w,r.atan/p,0,p*ar(w,8/p/f,0,r*8+2))),
	ar(w,r@1*4))
})
```

### Fast Eddie "Acid Thunder" Homage to [one of the greatest](https://www.youtube.com/watch?v=AnHOelTxkCY)
```
( // #SuperColldier #FastEddie #Acid #Thunder
SynthDef(\fe,{|freq=110,gate=1,m=1|
	e=Env.adsr(0s/m,0s,1/2,1/8).ar(2,gate);
	o=Splay.ar(MoogFF.ar(SinOsc.ar(freq,SinOsc.ar(freq*2)**e*m).sign*e,m*e*8e3+10));
	Out.ar(\out.kr(0),o);
}).add;

Pbind(\instrument,\fe,
	\dur, Pseq([2,1,2,Rest(1),2,Rest(1),2,1,1,Rest(1),2]/8,inf),
	\note, Pseq([0,3,1,0,4,0,9,8,-1,0,0]-36,inf),
	\m,Pbrown(1/8,2,0s)
).play;
)
```
### Acid Over 

```
( //  #SuperCollider #Acid #Crazy
w=Window("",nil,1<0,1<0).background_(c=Color.clear).front;
u=UserView(w,r=Rect(0,0,d=400,d)).animate_(1>0).background_(c)
.drawFunc_({
	(p=Pen).rotate(u.frame/99,d/2,d/2).color_(Color(1,1)).addOval(r)
	.fill.color_(Color()).addOval(Rect.aboutPoint(150@125,25/2,50))
	.addOval(Rect.aboutPoint(250@125,12,50))
	.fill.addArc(200@200,150,0,pi).stroke});
CmdPeriod.doOnce{w.close};play{m=ar(o=SinOsc,ar(q=QuadC,1)*8);
	BPF.ar(ar(o,f={ar(q,m)*222+22},ar(o,m**f*2,0,m*6)).sign,m.max*4e3+22)!2
})
```
### Gendy2 Appreciation 

```
({ // #SuperCollider #Daily #Gendyn #Doom 
	l=LFDNoise3.ar(1!2).range(_,_);
	LeakDC.ar(Splay.ar(
		Gendy2.ar(2,3,3/pi,1,3,333,a:l.(0s1,0s),c:l.(0s1,0s)),l.(0s3,pi).sum%(2/3))) 
}.play)

({ // Bonus cruising of doom settings 
	l=LFDNoise0.ar(1/8!2).lag(1).range(_,_);
	LeakDC.ar(Splay.ar(
		Gendy2.ar(6,6,l.(0s1,1),l.(0s1,1),3,3333,l.(0s,1).linlin(0s,1,-1,1),
			l.(0s,1).linlin(0s,1,-1,1),12,l.(1,13),l.(0s1,0s),l.(0s1,pi)),
		l.(0s3,pi).sum%(2/3)))
}.play)
```

### Phase Modulation Nonsense
```
({ ar(Splay, // Phase Modulutaion Washer
	ar(PMOsc,
		f=(p=(1,3..64))/pi*p.degrad*(n=110),f*2,ar(o=SinOsc,f/n,0,ar(o,n/f,0,2)),
		ar(o,3/p,0,pi),ar(o,1/p)),ar(o,ar(o,0s/n)*8)/ar(o,1/n)/2
)}.play)
```

### Old Tweet.
```
({  #Something awfully old.
	RecordBuf.ar(LFDNoise3.ar+PinkNoise.ar(8),a=LocalBuf(8*2048,1),loop:0);
	Splay.ar(LeakDC.ar(
			CombC.ar(Warp1.ar(1,a,f={LFDNoise3.kr(0.001!7).tan.abs},
			f*0.03,Array.series(4,1/7,1/7)*0.12,-1,4,0,4),0.4,0.4,2
).softclip)
)}.play)
```
### Really Repetetive Chord
```
(Ndef(\61457,{ // The Chord
	t=TDuty.ar(1);c=Splay.ar((BPF.ar(
		WhiteNoise.ar*Env(flat([0,1]!4)++0,[0,0s/9,0,0s/9,0,0s7,0,0.5],
			[0,-1,0,-1,0,-1,0,-12]).ar(0,t),[1700,3540],3/4).sum));
	f=[131,311,196,233,65,349,292];e=Env.perc(0,1).ar(0,t);
	o=Splay.ar(VarSaw.ar(f,0,(1-e/2)**2));
	o=o*Env.asr(0s1,1,1/4,-7).ar(0,Trig.ar(t,3/4));
	Out.ar(\out.kr(0),
		DelayN.ar(MoogFF.ar(o,(log(e*LFDNoise3.ar(1,6,6)).sin.abs)*1e4+10),1,1/4)+c
	);
}))
```

### Drone for the Evening.
```
(s.options.numWireBufs_(512);s.reboot.waitForBoot(
	{(Ndef(\💿,{ar(Out,\out.kr(0),
			ar(Splay,ar(o=SinOsc,
			c=(p=(1,3..64))**(p/p.sum).atan*(f={ar(LFDNoise1,8,1/16,33).midicps})*p,
			ar(o,c*3)*ar(o,ar(o,p/c,0,8,8)/c,0,pi),ar(o,p/c)),ar(o,1/3,0,ar(o,1/32))))
	}))})
)
```

