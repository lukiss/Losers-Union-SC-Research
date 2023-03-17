/*

Here is an uncomplete collection #SuperCollider tweets i did before i gave up the Twitter platform completely. Feel free to refactor build further upon or whatever.

*/




// 27 Jan 2023

play{c={|n|(m={kr(TRand,0.5,2.5,d=kr(Dust,1)).lag3(1/16)})**n}!9;ar(Splay,ar(SinOsc,c.log10*c)*(log10(ar(LFSaw,c.tan/m).abs**kr(TChoose,kr(Dust,1+m),[c.sum,n=c/c.sum,c.round(32),n.atan2(m),c.sin,c.tan,n.hypot(c)]).max).sin.abs**(10)).tanh,ar(SinOsc,m*m,0,0.5))}

// 20 jan. 2023
play{l=ar(TChoose,ar(Dust,3!2),ar(CuspN,1s**(..5)).tanh).exprange(_,_);a=perfectShuffle({l.(0s1,1)}!3)++({l.(1,0s1)}!3);e=ar(EnvGen,Env(a,a/a.sum,a.neg*9).circle,1,1,0,1/f=(l.(12,999)));ar(Splay,tanh(e.blend(sin(e*f),1-g=ar(EnvGen,perc(Env,0s5,1b/f),e>0.1))+g))} // #SuperCollider

// 14 nov. 2022

play{ar(Splay,ar(LeakDC,ar(GrainFM,2,ar(TDuty,1/d=Dseq((1,3..21),inf)),2+ar(p=LFPar,0.3)/d*2,[2,3],f=Ddup(ar(p,0.05)*d/[3,7,5,1],Dseq((9..42).degreeToKey(Scale.phrygian),inf).midicps),f%d/12,ar(p)/2,Buffer.sendCollection(s,Env.perc.discretize))*0.23))} // #SuperCollider #Ballad

play{ar(Splay,ar(LeakDC,ar(GrainFM,2,ar(TDuty,1/d=Dxrand((1,3..21),inf)),2+ar(p=LFPar,0.3)/d*2,[2,3],f=Ddup(ar(p,0.05)*d/[3,7,5,1],Dseq((9..42).degreeToKey(Scale.phrygian),inf).midicps),f%d/12,ar(p)/2,Buffer.sendCollection(s,Env.perc.discretize))*0.23))} // #SuperCollider #rndrmx

// Collab with f0 1 nov. 2022

play{FFT(b=Buffer.alloc(s,n=1024),ar(LFTri,ar(CuspN,1).abs*5e3));ar(IFFT,b)/6};i=Image(32);UserView(w,r=400).front.animate_(1>0).drawFunc={b.getn(0,n,{|v|defer{i.setPixels(as({|p|colorToPixel(Image,gray(Color,v@p))}!n,Int32Array))}});i.drawInRect(asRect(r@r))}//#SuperCollider #f0

// Catfarm 27 oktober 2022

play{/* #SuperCollider */ f=ar(o=SinOsc,a=[41,73,123,196]);ar(Splay,ar(LeakDC,{|i|a=a*5.midiratio;f=ar(o,f*a.rotate(i+3),0,ar(o,f)).tanh}!9;f.tanh),kr(o,f.sum.abs))}

// Additive https://youtu.be/C2C0nqtIHRg   Interpretation  19 okt 2022

play{c=LFPar;r={ar(LFDNoise3,4).abs};ar(c,(k=(t=ar(TDuty,Dwhite(1,8)/8)).lag3(1e-4,0.03))*90+50,0,k.lag3(0.03,1/4)).tanh+ar(c,80*(p=(1..60))*(p*p*6e-4),0,p.log2*1e-3*(sin(1-log2(p*t.lag3(r*1e-2,1-r)))**10)/2).sum!2}//#SuperCollider

// Listen to scsynt

(fork{f=File(Platform.resourceDir+/+"scsynth","rb");c=[];a=f.getInt8;while{a.notNil}{c=c.add(a);a=f.getInt8};b=loadCollection(Buffer,s,c);1.wait;b.duration.post+" seconds of scsynth.".postln;play{0.5*tanh(ar(PlayBuf,1,b,1!2))}}/* #SuperCollider #Harsch #Noise */)

// Ambient Ligeti inspired soundscape + fix

(play{tanh(ar(Splay,ar(LeakDC,ar(FreeVerb,ar(Warp1,1,read(Buffer,s,Platform.resourceDir+/+"sounds/a11wlk01.wav"),(r={abs(ar(LFDNoise3,1/86))})!10,midiratio([-36,-9,-14,0,-19,-5,3,-2,-24,-7]),8*r!10+8/86,-1,12,1/4*r!10,4,r!10),r!10,0.5+r!10,r!10)),3/4))})//#SuperCollider #Ambient

(play({tanh(ar(Splay,ar(LeakDC,ar(FreeVerb,ar(Warp1,1,read(Buffer,s,Platform.resourceDir+/+"sounds/a11wlk01.wav"),(r={abs(ar(LFDNoise3,1/86))})!9,midiratio([-36,-9,-14,0,-19,-5,3,-2,-24,-7]),8*r!9+8/86,-1,12,1/4*r!9,4,r!9),r!9,0.5+r!9,r!9)),2/4))},s,\fade,12)/*#SuperCollider*/)

// Standard Acid August 17 2022

play{l=LFDNoise1;tanh(ar(LeakDC,ar(MoogFF,ar(LFPulse,(f={ar(StandardN,kr(l,1,8,8).round(4)).abs}).value.exprange(4,80).round(1).midicps.lag(0.05)),kr(LFSaw,-8!2*f).exprange(100,f*8000),kr(l,1,2,2).abs)))}//#SuperCollider #Standard #Acid

// 5 sep. 2020

(l={abs(kr(LFDNoise3,1,1))};r=abs(kr(Gendy3,6,6,l,l,_,l,l,5,l*5,_));play{-12.dbamp*ar(Splay,{|n|ar(CombC,ar(LeakDC,ar(FM7,{[{80.rand.midicps},0,abs(kr(Blip,r.(l,24),r.(l,228)))]}!6,r.(0.02,0.5)!6!6).slice([n])),0.5,0.05+r.(0.04,l))}!6
)}) // #Supercollider

// 25 aug 2022

play{i=Impulse;l=LFSaw;t=ar(StandardN,kr(TRand,0,4,kr(i,4!2)).round(2)).round(1/2).abs;ar(Splay,ar(PMOsc,(d=kr(Demand,kr(Dust,t*8),0,Drand(((10..46).degreeToKey(lydian(Scale))),inf)!2).midicps),d*t,t*kr(l,t.neg*4).range(0,4),0,kr(l,t.neg*4).max)*0.3,kr(l,9))} // #SuperCollider

// Stupid simple Vinyl 22 aug 2022

play{ar(BrownNoise,dbamp(-55)!2)+ar(Splay,ar(Crackle,2!3,kr(LFDNoise1,11!3)).excess(-5.2.dbamp))} /* Stupid simple #SuperCollider */

// 12 juli 2019

play{f=ring1(z=LFDNoise3.kr(_,_),z%0.01).hypot(z);{Formant.ar(f.(f.(3,13),220),f.(f.(2,10),f.(f.(3,13),2320)),f.(f.(3,5),2500),f.(f.(2,12),1).excess(f.(f.(1,14),f.(4.4,0.5)+0.3))!2).softclip}} // #SuperCollider

// Sawing 22 nov. 2021

(n={1+scramble((1e-05*(1..7)))};play{ar(Splay,ar(VarSaw,33*n,0,ar(LFSaw,1/3*n,0,ar(LFSaw,n/3)),abs(ar(LFSaw,5/3,0,tan(ar(LFSaw,7/3*n.neg))))).tanh)})

// 30 apr. 2020 tweet for you!

(t={kr(TDuty,Drand((1..8)/16,inf))};r=kr(TRand,_,_,t);play{ar(Normalizer,ar(CompanderD,ar(LeakDC,ar(LorenzL,22050,r.(19,11),r.(1,18),r.(0.1,5),r.(0.01,0.06!2))),0.8,r.(0.01,0.5),r.(0.01,0.5),0.01,0.075),-2.dbamp,0.02).fold2(0.8)})

// 23 mars

play{d={kr(Gendy3,freq:kr(LFNoise1,2,1,1)).abs.round(1/16)};c=4096;l=LocalBuf;a=new(l,c,1);b=new(l,c,1);FFT([a,b],ar(StandardL,5e3*d!2));PV_BinScramble([a,b],d,d,t=kr(Dust,d!5/2));ar(Splay,ar(FreeVerb,ar(LeakDC,ar(IFFT,[a,b])*lag3ud(t,0.01,d!5)),d/3,d,d/2),0.5)}//#Supercollider

// FitzNoise 29 apr. 2020

(g={|a=1e-3,b=1,c=pi|kr(TRand,a,b,kr(Impulse,{kr(LFDNoise1,1/4*c,c+c).lag3.abs}))};
play{ar(LeakDC,ar(Splay,ar(LPF,ar(FitzHughNagumo,ar(Impulse,g.(a:1)),g.(b:0.1),g.(b:0.02),g.(a:1,b:0.02),g.(a:0.1,b:2)).tanh,2e3)))})

// 25 sep. 2020

fork{loop{n=[" ","/","\\","|","-","_"].choose.post;play{ar(Saw,n.ascii.midicps,kr(Env.perc(0,0.2,0.5!2,-20),doneAction: 2))};0.1.wait}}//#SuperCollider


// 25 aug 2022

play{t=max(ar(CuspN,8,mul:2).round(1/2));ar(FreeVerb,ar(SinOscFB,(d={kr(Demand,kr(Impulse,t*10),0,Dseq(((20..40).degreeToKey(Scale.lydian)).scramble,inf)!2)}).midicps,kr(LFSaw,t.neg*10).exprange(0.1,2.4),kr(LFSaw,neg(t*10)).max.lag))} // #SuperCollider #silly


// Drone 22 maj 2021

(n=4;l=kr(LFDNoise3,0.05!n,_).abs;d={ar(Demand, ar(TDuty,(30..53).choose),0,Dxrand((7..53).degreeToKey([0,1,4,5,7,9,10],12).midicps,inf))};play{ar(Splay,ar(HPF,ar(CombC,ar(PMOsc,d!n,d!n,l.(2),l.(0.5),l.(0.25)+1/4),1/4,1/4-l.(1/16),7),110,-12.dbamp))}) // #SuperCollider #drone


// 1 sep.2022

(/* #Nasty #Harsh #Ass #interactive #SuperCollider #nonsense #SC3Plugins needed */ t={kr(TRand,pi,0.5,kr(Dust,ar(Amplitude,ar(SoundIn))*5))};z={ar(Standard2DC,101,44100,t,t*3,t*3).tanh};play{ar(LeakDC,z+(z.raddeg>>ar(SoundIn,[0,1])).tanh.softclip*dbamp(-12))})


// Hand Clapping 18 aug.

play{ar(LeakDC,ar(FreeVerb,kr(EnvGen,Env([0,1,0,1,0,1,0,0],0.007*[0,1,1,1,1,18,0]),t=kr(TDuty,Dseq(Dser([2,1,3,3,1,2,3,2,1,2]/9,Dseq((0..8),inf)),inf)))*ar(CuspL,kr(Decay2,t!2,0,1/3,9e3,8e3))))} // #SuperCollider #ClapYourHands

// 23 mars

(l=LFDNoise1;w={|a|kr(l,13).exprange(0.001,kr(l,a,20*a).abs).lag3(0.001,abs(kr(l,1)))};{ar(Splay,ar(LeakDC,ar(Gendy1,6,6,w.(0.2),w.(0.2),0.1,1130,w.(0.1).squared,w.(0.1).squared,6,w.(6)!2).softclip),kr(l,5))}.play) // #SuperCollider

// 9 jan. 2019

play{|a=40|f={|b|{LFNoise1.kr(1).exprange(10,b)}};Splay.ar(LeakDC.ar(Formant.ar(({a=a+5}!6).midicps*{f.(29).round(7)!6}.midiratio,{f.(1000)}!6,f.(500)!6)))} // #Supercollider

// 3 nov. 2018

(play{|a=9|f={|b|LFNoise2.kr(b.sign).exprange(9,b)};Splay.ar(LeakDC.ar(Formant.ar(({a=a+7}!6).midicps*{f.(99).round(12)!6}.midiratio,{f.(2200)}!6,f.(300)!6,SinOscFB.kr (f.(0.05),{f.(0.5)}!6.abs))))})

// 11 apr.2022

play{n=LFDNoise3;l=ar(n,ar(n,2,10,10)).linlin(0,1,_,_);f=FFT({LocalBuf(1024)}!2,ar(BrownNoise,0.7!2));v={l.(-1,1).lag3(0,5e-3)};f=PV_Freeze(f,0.5-kr(Dust,l.(1,13)));PV_Cutoff(f,v);ar(FreeVerb,ar(LeakDC,ar(IFFT,f).tanh,0.999,v),0.1)} // #Supercollider + needs #SC3Plugins

// 12 Maj 2020

(/* #sctweet,#supercollider,#pi,#gongscale */l=kr(LFDNoise3,_,_).abs;play{i=1/16;p=pi;ar(CombC,ar(Splay,{ar(SinOsc,kr(Demand,t=kr(Impulse,l.(p,p)),0,Drand((9..42).degreeToKey([0,2,4,7,9],p*p),inf)).midicps,0,t.lag3(0,i)+(t.lag3(i+l.(9,i*4),l.(p,p))/p))}!5),i,i,pi)})

// Poor peoples Gendy 11 april 2021

e=alloc(Buffer,s,256);e.sendCollection(c=(0,1/255..1));a=plot(c).editMode_(true);a.editFunc={|v|e.sendCollection(v.value)};l={abs(ar(LFDNoise3,2))};r={lag3(ar(BufRd,1,e,l.abs*255))};play{ar(Splay,ar(LeakDC,tanh(ar(Gendy1,6,6,r,r!2,1,r.linexp(0,1,222,5e3),r,r,10,1-r*10))),l)}

// Poor peoples Pulsar Twitter Edition 13 apr. 2021

(e=alloc(Buffer,s,256);c=(0,1/255..1);a=plot(c).editMode_(true);a.editFunc={|v|e.sendCollection(v.value)};l={kr(LFDNoise1,4)};r=kr(BufRd,1,e,l*255,1,4).linexp(0,1,_,_);play{ar(Splay,tanh(ar(GrainFM,2,ar(Impulse,r.(0.1,99)),r.(0.004,5),r.(40,2e3),r.(40,2e3),r.(0.1,9),l,e))*0.1)})