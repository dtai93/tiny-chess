import json,os,sys,urllib.request,urllib.error
E=os.environ.get("MAKEFUN_ENV_URL")or sys.exit(1)
A="abcdefgh";M=dict(P=100,N=320,B=330,R=500,Q=900,K=2e4)
P=[0]*8+[50]*8+[10,10,20,30,30,20,10,10,5,5,10,25,25,10,5,5,0,0,0,20,20,0,0,0,5,-5,-10,0,0,-10,-5,5,5,10,10,-20,-20,10,10,5]+[0]*8
N=[-50,-40,-30,-30,-30,-30,-40,-50,-40,-20,0,0,0,0,-20,-40,-30,0,10,15,15,10,0,-30,-30,5,15,20,20,15,5,-30,-30,0,15,20,20,15,0,-30,-30,5,10,15,15,10,5,-30,-40,-20,0,5,5,0,-20,-40,-50,-40,-30,-30,-30,-30,-40,-50]
B=[-20,-10,-10,-10,-10,-10,-10,-20]+[-10,0,0,0,0,0,0,-10]*2+[-10,5,5,10,10,5,5,-10,-10,0,10,10,10,10,0,-10,-10,10,10,10,10,10,10,-10,-10,5,0,0,0,0,5,-10]+[-20,-10,-10,-10,-10,-10,-10,-20]
R=[0]*8+[5,10,10,10,10,10,10,5]+[-5,0,0,0,0,0,0,-5]*5+[0,0,0,5,5,0,0,0]
Q=[-20,-10,-10,-5,-5,-10,-10,-20]+[-10,0,0,0,0,0,0,-10]*2+[-5,0,5,5,5,5,0,-5,0,0,5,5,5,5,0,-5,-10,5,5,5,5,5,0,-10,-10,0,5,0,0,0,0,-10]+[-20,-10,-10,-5,-5,-10,-10,-20]
K=[-30,-40,-40,-50,-50,-40,-40,-30]*4+[-20,-30,-30,-40,-40,-30,-30,-20,-10,-20,-20,-20,-20,-20,-20,-10,20,20,0,0,0,0,20,20,20,30,10,0,0,10,30,20]
T=dict(P=P,N=N,B=B,R=R,Q=Q,K=K);n=0;L=50000
def post(p,b):
 r=urllib.request.Request(E+p,data=json.dumps(b).encode(),headers={"content-type":"application/json"})
 try:
  with urllib.request.urlopen(r)as x:return json.load(x)
 except urllib.error.HTTPError as e:sys.exit("%s %s"%(p,e.code))
def parse(f):
 b=[0]*64;x=0;y=7
 for c in f.split()[0]:
  if c=="/":y-=1;x=0
  elif c.isdigit():x+=int(c)
  else:b[y*8+x]=c;x+=1
 return b,f.split()[1]=="w"
def U(i):return A[i&7]+str((i>>3)+1)
def S(m):return ord(m[0])-97+(int(m[1])-1)*8,ord(m[2])-97+(int(m[3])-1)*8
def ev(b,w):
 s=0
 for i,c in enumerate(b):
  if not c:continue
  u=c.upper();v=M[u]+T[u][i if c.isupper()else i^56];s+=v if c.isupper()else -v
 return s if w else -s
def put(b,m):
 nb=b[:];a,c=S(m);p=nb[a];nb[a]=0
 if p in"Kk" and abs(c-a)==2:
  rs,rd=(a+3,c-1)if c>a else(a-4,c+1);nb[rd]=nb[rs];nb[rs]=0
 if p in"Pp" and not nb[c] and a%8!=c%8:nb[c+(-8 if p=="P"else 8)]=0
 if len(m)==5:p=("QNBR"if p=="P"else"qnbr")["qnbr".index(m[4])]
 nb[c]=p;return nb
def gen(b,w):
 ms=[];Z={"N":(17,15,10,6,-6,-10,-15,-17),"K":(9,8,7,1,-1,-7,-8,-9),"B":(9,7,-7,-9),"R":(8,1,-1,-8),"Q":(9,8,7,1,-1,-7,-8,-9)}
 for i,c in enumerate(b):
  if not c or c.isupper()!=w:continue
  u=c.upper();f=i&7
  if u=="P":
   s=8 if w else -8;o=i+s
   if 0<=o<64 and not b[o]:
    pr=o>>3==(7 if w else 0)
    ms+=[U(i)+U(o)+q for q in"qrbn"] if pr else [U(i)+U(o)]
    if(w and i>>3==1 or (not w)and i>>3==6)and not b[i+2*s]:ms+=[U(i)+U(i+2*s)]
   for d in(-1,1):
    o=i+s+d
    if 0<=o<64 and abs((o&7)-f)==1 and b[o]and b[o].isupper()!=w:
     pr=o>>3==(7 if w else 0)
     ms+=[U(i)+U(o)+q for q in"qrbn"] if pr else [U(i)+U(o)]
   continue
  sl=u in"BRQ"
  for d in Z[u]:
   j=i+d
   while 0<=j<64:
    if abs((j&7)-((j-d)&7))>1 and abs(d)&7:break
    if u=="N" and abs((j&7)-f)not in(1,2):break
    if not b[j]:ms+=[U(i)+U(j)]
    else:
     if b[j].isupper()!=w:ms+=[U(i)+U(j)]
     break
    if not sl:break
    j+=d
 return ms
def ab(b,w,d,a,z):
 global n;n+=1
 if n>L:return ev(b,w)
 if d==0:
  best=ev(b,w)
  for m in gen(b,w):
   a0,c0=S(m)
   if not b[c0]:continue
   v=-ab(put(b,m),not w,0,-z,-a)
   if v>best:best=v
   if best>a:a=best
   if a>=z:break
  return best
 best=-1e9
 ms=gen(b,w);ms.sort(key=lambda m:-(M[b[S(m)[1]].upper()]if b[S(m)[1]]else 0))
 for m in ms:
  v=-ab(put(b,m),not w,d-1,-z,-a)
  if v>best:best=v
  if best>a:a=best
  if a>=z:break
 return best
def choose(fen,legal):
 global n;n=0;b,w=parse(fen);best=legal[0];bv=-1e9
 def key(m):
  a,c=S(m);t=b[c];return -(M[t.upper()]if t else 0)
 for m in sorted(legal,key=key):
  v=-ab(put(b,m),not w,3,-1e9,1e9)
  if v>bv:bv=v;best=m
 return best
while 1:
 g=post("/game",{})
 if g["status"]=="done":break
 while g["status"]=="your_move":
  g=post("/move",{"game":g["game"],"move":choose(g["fen"],g["legal_moves"])})
 print(g.get("outcome"),file=sys.stderr)
print(g.get("wins"),g.get("draws"),g.get("losses"),file=sys.stderr)
