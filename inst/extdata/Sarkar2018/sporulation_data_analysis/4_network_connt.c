  /* ................ generates connected network (and renumbers indices) .......... */

  #include<iostream>
  #include<iomanip>
  #include<fstream>
  #include<cstdlib>
  #include<math.h>
  using namespace::std;
  using std::ios;
  int main()
{ 
   int N;
   cout<<"please enter original dimension of matrix"<<endl;
   cin>>N;
   ifstream infil("matrix_1.0.dat",ios::in);
   ofstream outfil("Renum_1.0.dat",ios::out); // gives renumbered nodes
   
  int **mdArray = new int*[N];
	    for (int r = 0; r < N; r++)
	    {
	        mdArray[r] = new int[N];
	        for(int c = 0; c < N; c++)
	        {  
                    infil>>mdArray[r][c];
                }
            } 
  
 int *iptr = new int [N];
   delete iptr;
 int size[N],Nvr[N],m=0,b=0;
  
   for( int f=0; f<N; f++)
 {
   Nvr[f]=0;
   size[f]=0;
 }
  for ( int j=0; j<N; j++)
 {
   for( int l=0; l<N; l++)
  {
     if( mdArray[j][l]==1)
     size[j]++;
     //cout<<size[j]<<endl;
  }
 } 
  int max=0;
   for( int a=0; a<N; a++)
 {  //cout<<size[a]<<endl;
   
   if( max<=size[a])
    max=size[a];
  
   
 } cout<<max<<endl;
  int numm=0;
 
  for( int c=0; c<N; c++)
 {
    if( max==size[c])
   {
     numm++;
   
    }
 }
   
  int count[numm][2];
  for( int i=0; i<numm; i++)
{
   for( int j=0; j<2 ; j++)
{
  count[i][j]=0;
}
}
   // cout<<"max="<<setw(10)<<"num"<<num<<endl;
  int  num=0;
   int counter=0;
  for( int c=0; c<N; c++)
 {
    if( max==size[c])
   {
     num=c;
     count[counter][0]=c;
    cout<<c<<endl;
    int i=num;
    b=0,m=0;
    do
   {
   // count=0;
    
    for(int k=0; k<N; k++)
   {
      if(mdArray[i][k]==1)

     { 
      int r=0;
      for( int s=0; s<=count[counter][1]; s++)
     {
       if(k==Nvr[s])
      {
         r++;
         continue;
       }
      }
      if(r>0)
      continue;
      else
     {
       //cout<<k<<endl;
       Nvr[count[counter][1]]=k;
       //size[b]++;
       count[counter][1]++;
       b++;
       cout<<"k="<<k<<endl;
      }
     }
    }
   i= Nvr[m];
   m++;
 
  }while( m<count[counter][1]);

   cout<<"connected="<<count[counter][1]<<endl;
    counter++;
  
}

}
  int maximum=0;
  for (int i=0; i<counter; i++)
{
    if( maximum<=count[i][1])
    maximum=count[i][1];
}

  for( int f=0; f<N; f++)
 {
   Nvr[f]=0;
  //size[f]=0;
 }
   num=0,m=0,b=0;
for (int i=0; i<counter; i++)
{
    if(count[i][1]==maximum)
    num=count[i][0];
}

   cout<<"num"<<num<<endl;
  int i=num,countt=0;
    do
   {
   // count=0;
    
    for(int k=0; k<N; k++)
   {
      if(mdArray[i][k]==1)

     { 
      int r=0;
      for( int s=0; s<countt; s++)
     {
       if(k==Nvr[s])
      {
         r++;
         continue;
       }
      }
      if(r>0)
      continue;
      else
     {
       //cout<<k<<endl;
       Nvr[countt]=k;
       //size[b]++;
       countt++;
       b++;
       
      }
     }
    }
   i= Nvr[m];
   m++;
 
  }while( m<countt);

  cout<<"connected="<<countt<<endl;
  
  if(countt<N)
{
   int node_notconn[N];
   int d=1,ne,y=0;
  //cout<<"network is not connected"<<endl;
  int g=0;
  do
 {
   
     ne=0;
    for( int r=0; r<countt; r++)
   {
     if( g == Nvr[r] )
      ne++;
    }
    //if( ne==i)
   //{
    //node_notconn[y]=i;
    
  //  cout<<"not conn. nodes are="<<node_notconn[y]<<endl;
    //y++;
   
    //}
    //else
    if( ne>0)
   { 
      outfil<<g+1<<"\t";
     // cout<<g+1<<setw(20);
    //Nvr[r] = d;
    //node[cluster[p][r]]= d; 
      outfil<<d<<endl;
     // cout<<d<<endl;
    //cout<<cluster[e][w]<<"="<<d<<endl;
      d++;
   }
   
  
     g++;
  }while(g<N);
  
      
  }
       
 }


