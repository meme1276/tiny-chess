import json,os,urllib.request as U
V=dict(zip('PNBRQKpnbrqk',(100,320,330,500,900,0)*2));V['.']=0
N=[-33,-31,-18,-14,14,18,31,33];R=[-16,-1,1,16];D=[-17,-15,15,17];G=R+D
T={};n=[0];I=9**9;H=set()
def e(b):
 s=30*((b.count('B')>1)-(b.count('b')>1))
 for i,q in enumerate(b):
  if q>'.':
   r,f=i>>4,i&7;c=6-abs(3.5-f)-abs(3.5-r);z=q>'Z';rr=(r,7-r)[z]
   v=V[q]+(rr*6+10*(1<f<6 and 1<rr<6)+25*(rr>5)if q in'Pp'else c*5 if q in'Nn'else c*3 if q in'Bb'else 12*(rr>5)if q in'Rr'else c*2 if q in'Qq'else c*(6-13*('Q'in b or'q'in b)))
   s+=(v,-v)[z]
 return s
def a(b,s,w):
 d=16-32*w
 for J,c in(((d-1,d+1),'P'),(N,'N'),(G,'K'),(R,'RQ'),(D,'BQ')):
  W=(c,c.lower())[1-w]
  for dd in J:
   t=s+dd
   while not t&136:
    q=b[t]
    if q>'.'and q in W:return 1
    if q>'.'or c in'PNK':break
    t+=dd
 return 0
def g(b,w,c,ep):
 M=[];A=M.append
 for s,q in enumerate(b):
  if s&136 or q=='.'or(q<'a')!=w:continue
  if q in'Pp':
   d=32*w-16;Q='QRBN'if w else'qrbn'
   for dd in(d-1,d,d+1):
    t=s+dd
    if t&136 or dd==d and b[t]!='.'or dd!=d and not(b[t]>'.'and(b[t]<'a')!=w or t==ep):continue
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
     if ch[1-w]in c and b[u]+b[v]=='..'and a(b,u,1-w)==a(b,v,1-w)==0 and(dd>0 or b[h-3]=='.'):A((s,v,''))
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
 b,w,c,ep=t;kg='Kk'[1-w];ch=a(b,b.index(kg),1-w)
 if d<=0:
  if not ch:
   sc=e(b)*(2*w-1);A=max(A,sc)
   if A>=B:return B
  M=[x for x in g(b,w,c,ep)if ch or b[x[1]]>'.'or x[2]];M.sort(key=lambda x:-V[b[x[1]]])
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
 d+=ch;kk=(''.join(b),w,c,ep)
 if p and kk in H:return 0
 en=T.get(kk);tm=en and en[3]
 if en and en[0]>=d and(en[1]==0 or en[1]==1 and en[2]>=B or en[1]==2 and en[2]<=A):return en[2]
 M=g(b,w,c,ep);M.sort(key=lambda x:-((x==tm)*I+V[b[x[1]]]))
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
     T[kk]=(d,1,bs,bm);return bs
 if not lg:return-10**5+p if ch else 0
 T[kk]=(d,bs<A and 2,bs,bm)
 return bs
def B(t):
 if len(T)>4e5:T.clear()
 n[0]=0
 for d in range(1,9):
  try:s(t,d,-I,I,0)
  except TimeoutError:break
  bm=T.get((''.join(t[0]),)+t[1:],[0]*4)[3]
 return bm
def f(z):
 y=z.split();b=['.']*128;r=7;c=0
 for ch in y[0]:
  if ch=='/':r-=1;c=0
  elif ch<'9':c+=int(ch)
  else:b[r*16+c]=ch;c+=1
 return b,y[1]>'v',y[2],16*int(y[3][1])+ord(y[3][0])-113 if y[3]>'-'else-1
def P(z,b):
 for i in range(9):
  try:
   with U.urlopen(U.Request(os.getenv("MAKEFUN_ENV_URL")+z,data=json.dumps(b).encode()))as x:return json.load(x)
  except U.URLError:pass
j=P("/game",{})
while j["status"]!="done":
 while j["status"]=="your_move":
  L=50000;t=f(j["fen"]);H.add((''.join(t[0]),)+t[1:]);x=B(t);mv=chr(97+x[0]%8)+str(1+x[0]//16)+chr(97+x[1]%8)+str(1+x[1]//16)+x[2].lower()if x else''
  Z=j["legal_moves"]
  if mv not in Z:mv=Z[0]
  j=P("/move",{"game":j["game"],"move":mv})
 j=P("/game",{})
