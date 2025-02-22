/* ............... Creates adjacency matrix from list of interactions ........... */

from scipy import *
f=open('Adjacency_1.0.txt','r')
name=[]
connection=[]
for line in f:
	l=line.split('\t')
	connection.append([l[0],l[1][0:(len(l[1])-1)]])
	if l[0] not in name:
		name.append(l[0])
	if l[1] not in name:
		name.append(l[1][0:(len(l[1])-1)])

temp=set(name)
name=list(temp)
n= len(name)
print n
f1=open('Index_1.0.txt','w')
f2=open('junk.dat','w')
f3=open('matrix_1.0.dat','w')


for i in range(n):
	f1.write(name[i]+'\t'+str(i+1)+'\n')

element=[]

for i in range(len(connection)):
	temp=[]	
	for j in range(n):
		if connection[i][0]==name[j]:
			f2.write(str(j)+'\t')
			temp.append(j)	
		
	for j in range(n):
		if connection[i][1]==name[j]:
			f2.write(str(j)+'\n')
			temp.append(j)
	element.append(temp)

mat=zeros((n,n))
for j in range(len(element)):
	mat[element[j][0]][element[j][1]]=1
	mat[element[j][1]][element[j][0]]=1


for j in range(n):
	mat[j][j]=0;

for j in range(n):
	for i in range(n):
		f3.write(str(int(mat[j][i]))+' ')
	f3.write('\n')














