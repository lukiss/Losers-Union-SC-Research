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
