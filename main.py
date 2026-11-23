import json,os,urllib.request as U
V=dict(zip('PNBRQKpnbrqk',(100,320,330,500,900,0)*2))
N=[-33,-31,-18,-14,14,18,31,33];R=[-16,-1,1,16];D=[-17,-15,15,17];G=R+D
S=range(128)
T={};Y=[[0,0]]*64;n=[0];I=9**9
def e(b):
 s=0
 for i in S:
  q=b[i]
  if q>'.':
   r,f=i>>4,i&7;c=6-abs(3.5-f)-abs(3.5-r)
   if q in'Pp':v=(r,7-r)[q>'Z']*6
   else:v=c*(-7 if q in'Kk'else 3)
   v+=V[q];s+=(v,-v)[q>'Z']
 return s
def a(b,s,w):
 d=16-32*w
 for J,c in(((d-1,d+1),'P'),(N,'N'),(G,'K'),(R,'RQ'),(D,'BQ')):
  W=(c,c.lower())[1-w]
  for dd in J:
   t=s+dd
   while not t&136:
    q=b[t]
    if q>'.':
     if q in W:return 1
     break
    if len(c)<2:break
    t+=dd
 return 0
def g(b,w,c,ep):
 M=[];A=M.append
 for s in S:
  q=b[s]
  if q=='.'or(q<'a')!=w:continue
  if q in'Pp':
   d=32*w-16;Q='QRBN'if w else'qrbn'
   for dd in(d-1,d,d+1):
    t=s+dd
    if t&136:continue
    if dd==d and b[t]!='.':continue
    if dd!=d and not(b[t]>'.'and(b[t]<'a')!=w or t==ep):continue
    if t>>4==7*w:M+=[(s,t,z)for z in Q]
    else:
     A((s,t,''))
     if dd==d and s>>4==6-5*w and b[s+2*d]=='.':A((s,s+2*d,''))
  elif q in'NnKk':
   for dd in N if q in'Nn'else G:
    t=s+dd
    if not t&136 and(b[t]=='.'or(b[t]<'a')!=w):A((s,t,''))
   if q in'Kk'and not a(b,s,1-w):
    h=116-112*w
    for dd,ch in((2,'Kk'),(-2,'Qq')):
     u,v=h+dd//2,h+dd
     if ch[1-w]in c and b[u]==b[v]=='.'and not(a(b,u,1-w)or a(b,v,1-w))and(dd>0 or b[h-3]=='.'):A((s,v,''))
  else:
   for dd in{'R':R,'B':D}.get(q.upper(),G):
    t=s+dd
    while not t&136 and b[t]=='.':A((s,t,''));t+=dd
    if not t&136 and(b[t]<'a')!=w:A((s,t,''))
 return M
def m(t,v):
 b,w,c,ep=t;fr,to,pr=v;b=b[:];q=b[fr]
 if q in'Pp'and to==ep and b[to]=='.':b[to-16 if w else to+16]='.'
 b[to]=pr or q;b[fr]='.'
 if q in'Kk'and abs(to-fr)==2:
  x,y=(fr+3,fr+1)if to>fr else(fr-4,fr-1);b[y]=b[x];b[x]='.'
 if q in'Kk':c=c.replace(q,'').replace('Qq'[q>'Z'],'')
 for sq,ch in zip((0,7,112,119),'QKqk'):c=c.replace(ch,'')if sq in(fr,to)else c
 return b,1-w,c or'-',(fr+to)//2 if q in'Pp'and abs(to-fr)==32 else-1
def s(t,d,A,B,p):
 n[0]+=1
 if n[0]>L:raise TimeoutError
 b,w,c,ep=t;sg=2*w-1;kg='Kk'[1-w];ch=a(b,b.index(kg),1-w)
 if d<=0:
  if p>14:return e(b)*sg
  if not ch:
   sc=e(b)*sg;A=max(A,sc)
   if A>=B:return B
  M=sorted((x for x in g(b,w,c,ep)if ch or b[x[1]]>'.'or x[2]),key=lambda x:-V.get(b[x[1]],0))
  lg=0
  for x in M:
   if not ch and not x[2]and sc+V[b[x[1]]]+200<A:continue
   ns=m(t,x)
   if a(ns[0],ns[0].index(kg),1-w):continue
   lg+=1;v=-s(ns,0,-B,-A,p+1)
   if v>=B:return B
   if v>A:A=v
  if ch and not lg:return-10**5+p
  return A
 d+=ch;kk=(''.join(b),w,c,ep);en=T.get(kk);tm=en and en[3]
 if en and en[0]>=d and en[1]==0:return en[2]
 M=g(b,w,c,ep);M.sort(key=lambda x:-((x==tm)*I+(10*V[b[x[1]]]if b[x[1]]>'.'else 9000 if x[2]else 3000 if x in Y[p]else 0)))
 bs=-I;lg=0
 for x in M:
  ns=m(t,x)
  if a(ns[0],ns[0].index(kg),1-w):continue
  lg+=1;v=-s(ns,d-1,-B,-A,p+1)
  if v>bs:
   bs=v;bm=x
   if v>A:
    A=v
    if A>=B:
     T[kk]=(d,1,bs,bm)
     if b[x[1]]=='.'and not x[2]:Y[p]=[x,Y[p][0]]
     return bs
 if not lg:return-10**5+p if ch else 0
 T[kk]=(d,0 if bs>=A else 2,bs,bm)
 return bs
def B(t):
 if len(T)>4e5:T.clear()
 n[0]=0;bm=0
 for d in range(1,12):
  try:s(t,d,-I,I,0)
  except TimeoutError:break
  bm=T.get((''.join(t[0]),)+t[1:],(0,)*4)[3]
 return bm
def f(z):
 y=z.split();b=['.']*128;r=7;c=0
 for ch in y[0]:
  if ch=='/':r-=1;c=0
  elif'/'<ch<'9':c+=int(ch)
  else:b[r*16+c]=ch;c+=1
 return b,y[1]>'v',y[2],16*int(y[3][1])+ord(y[3][0])-113 if y[3]>'-'else-1
def P(z,b):
 for i in range(9):
  try:
   with U.urlopen(U.Request(os.getenv("MAKEFUN_ENV_URL")+z,data=json.dumps(b).encode(),headers={"content-type":"application/json"}))as x:return json.load(x)
  except U.URLError:
   if i==8:raise
j=P("/game",{})
while j["status"]!="done":
 while j["status"]=="your_move":
  L=75000;x=B(f(j["fen"]));mv=chr(97+x[0]%8)+str(1+x[0]//16)+chr(97+x[1]%8)+str(1+x[1]//16)+x[2].lower()if x else''
  if mv not in j["legal_moves"]:mv=j["legal_moves"][0]
  j=P("/move",{"game":j["game"],"move":mv})
 j=P("/game",{})
