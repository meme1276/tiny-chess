import json,os,urllib.request as U
V=dict(zip('PNBRQKpnbrqk',(100,320,330,500,900,0)*2))
N=[-33,-31,-18,-14,14,18,31,33];R=[-16,-1,1,16];D=[-17,-15,15,17];G=R+D
S=[i for i in range(128)if not i&136]
T={};n=[0];I=10**9
def e(b):
 s=0
 for i in S:
  q=b[i]
  if q>'.':
   r,f=i>>4,i&7;c=6-abs(3.5-f)-abs(3.5-r)
   if q in'Pp':
    t=(r,7-r)[q>'Z'];v=t*6
   else:v=c*(-7 if q in'Kk'else 3)
   v+=V[q];s+=(v,-v)[q>'Z']
 return s
def a(b,s,w):
 d=16-32*w
 for J,c,sl in(((d-1,d+1),'P',0),(N,'N',0),(G,'K',0),(R,'RQ',1),(D,'BQ',1)):
  W=(c,c.lower())[1-w]
  for dd in J:
   t=s+dd
   while not t&136:
    q=b[t]
    if q>'.':
     if q in W:return 1
     break
    if not sl:break
    t+=dd
 return 0
def g(b,w,ca,ep):
 ms=[];A=ms.append
 for s in S:
  q=b[s]
  if q=='.'or(q<'a')!=w:continue
  u=q.upper()
  if u=='P':
   d=32*w-16;pr=7*w;Q='QRBN'if w else'qrbn'
   for dd in(d-1,d,d+1):
    t=s+dd
    if t&136:continue
    if dd==d:
     if b[t]!='.':continue
     if t>>4==pr:ms+=[(s,t,z)for z in Q];continue
     A((s,t,''))
     if s>>4==6-5*w and b[s+2*d]=='.':A((s,s+2*d,''))
    elif(b[t]>'.'and(b[t]<'a')!=w)or t==ep:
     if t>>4==pr:ms+=[(s,t,z)for z in Q]
     else:A((s,t,''))
  elif u in'NK':
   for dd in{'N':N}.get(u,G):
    t=s+dd
    if not t&136 and(b[t]=='.'or(b[t]<'a')!=w):A((s,t,''))
   if u=='K':
    h=116-112*w
    if not a(b,s,1-w):
     if'Kk'[1-w]in ca and b[h+1]==b[h+2]=='.'and not a(b,h+1,1-w)and not a(b,h+2,1-w):A((s,h+2,''))
     if'Qq'[1-w]in ca and b[h-1]==b[h-2]==b[h-3]=='.'and not a(b,h-1,1-w)and not a(b,h-2,1-w):A((s,h-2,''))
  else:
   for dd in{'R':R,'B':D}.get(u,G):
    t=s+dd
    while not t&136 and b[t]=='.':A((s,t,''));t+=dd
    if not t&136 and(b[t]<'a')!=w:A((s,t,''))
 return ms
def m(st,mv):
 b,w,ca,ep,hf=st;fr,to,pr=mv;b=b[:];q=b[fr];cp=b[to]
 if q in'Pp'and to==ep and cp=='.':b[to-16 if w else to+16]='.'
 b[to]=pr or q;b[fr]='.'
 if q in'Kk'and abs(to-fr)==2:
  x,y=(fr+3,fr+1)if to>fr else(fr-4,fr-1);b[y]=b[x];b[x]='.'
 c=ca
 if q in'Kk':c=c.replace(q,'').replace('Qq'[q>'Z'],'')
 for sq,ch in zip((0,7,112,119),'QKqk'):
  if sq in(fr,to):c=c.replace(ch,'')
 return b,1-w,c or'-',(fr+to)//2 if q in'Pp'and abs(to-fr)==32 else-1,0 if q in'Pp'or cp>'.'else hf+1
def k(st):return''.join(st[0]),st[1],st[2],st[3]
def s(st,d,A,B,p):
 n[0]+=1
 if n[0]>L:raise TimeoutError
 b,w,ca,ep,hf=st;sg=2*w-1
 kg='Kk'[1-w];ch=a(b,b.index(kg),1-w)
 if d<=0:
  if not ch:
   sc=e(b)*sg;A=max(A,sc)
   if A>=B:return B
  ms=[x for x in g(b,w,ca,ep)if ch or b[x[1]]>'.'or x[2]]
  lg=0
  for x in ms:
   ns=m(st,x)
   if a(ns[0],ns[0].index(kg),1-w):continue
   lg+=1;v=-s(ns,0,-B,-A,p+1)
   if v>=B:return B
   if v>A:A=v
  if ch and not lg:return-10**5+p
  return A
 d+=ch;kk=k(st)
 en=T.get(kk);tm=en[3]if en else None
 if en and en[0]>=d and en[1]==0:return en[2]
 ms=g(b,w,ca,ep);ms.sort(key=lambda x:-(x==tm)*I-(V[b[x[1]]]if b[x[1]]>'.'else 0))
 ao=A;bs=-I;lg=0
 for x in ms:
  ns=m(st,x)
  if a(ns[0],ns[0].index(kg),1-w):continue
  lg+=1;v=-s(ns,d-1,-B,-A,p+1)
  if v>bs:
   bs=v;bm=x
   if v>A:
    A=v
    if A>=B:
     T[kk]=(d,1,bs,bm);return bs
 if not lg:return-10**5+p if ch else 0
 T[kk]=(d,0 if bs>ao else 2,bs,bm)
 return bs
def B(st):
 if len(T)>4e5:T.clear()
 n[0]=0
 for d in range(1,12):
  try:s(st,d,-I,I,0)
  except TimeoutError:break
  bm=T.get(k(st),(0,0,0,0))[3]
 return bm
def f(z):
 y=z.split();b=['.']*128;r=7;c=0
 for ch in y[0]:
  if ch=='/':r-=1;c=0
  elif ch.isdigit():c+=int(ch)
  else:b[r*16+c]=ch;c+=1
 return b,y[1]>'v',y[2],(int(y[3][1])-1)*16+ord(y[3][0])-97 if y[3]>'-'else-1,int(y[4])
def P(z,bd):
 for i in range(9):
  try:
   with U.urlopen(U.Request(os.environ["MAKEFUN_ENV_URL"]+z,data=json.dumps(bd).encode()))as x:return json.load(x)
  except U.URLError:pass
j=P("/game",{})
while j["status"]!="done":
 while j["status"]=="your_move":
  L=(10**5,20000)[j["plies"]>99]
  st=f(j["fen"])
  x=B(st);mv=f"{chr(97+(x[0]&7))}{1+(x[0]>>4)}{chr(97+(x[1]&7))}{1+(x[1]>>4)}{x[2].lower()}"if x else''
  lm=j["legal_moves"]
  if mv not in lm:mv=lm[0]
  j=P("/move",{"game":j["game"],"move":mv})
 j=P("/game",{})
