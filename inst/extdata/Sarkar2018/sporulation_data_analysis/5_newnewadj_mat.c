 /*...........creates largest connected cluster (LCC) with renumbered indices ..........*/

#include<iostream>
 #include<iomanip>
 #include<fstream>
 #include<math.h>
 #include<cstdlib>
  using namespace::std;
  using std::ios;
  int main()
 {
   int N;
   cout<<"please enter the original dimension of the matrix"<<endl;
   cin>>N;
   
   int data1,data2;

   ifstream infil("matrix_1.0.dat",ios::in);
   ifstream mat2("Renum_1.0.dat",ios::in);    //renumbered nodes
   ofstream outfl("Adj_list_1.0.dat",ios::out); // creates LCC with new index
   ofstream output("ori_indexLCC_adj_1.0.dat",ios::out); // creates LCC with original index

   int **mdArray = new int*[N];
	    for (int r = 0; r < N; r++)
	    {
	        mdArray[r] = new int[N];
	        for(int c = 0; c < N; c++)
	        {  
                    infil>>mdArray[r][c];
                }
            } 
   
  
    int datain1,datain2;
  
    int nrows2=0, ncols2=0,col_count=0;
    const char EOL = '\n'; float temp;
	while ( mat2.peek()!=EOL)
    	{
     		mat2>>temp; 
     		ncols2++;
    	}
	while (!mat2.eof())
    	{ 
    		if (mat2.peek()==EOL)
        	{ 
   			mat2.ignore(3,EOL);
   			nrows2++;
 		}
    		else
  		{ 
    			while (mat2.peek()!=EOL)
  			{
    				mat2 >>temp;
  				col_count++; 
  				if (mat2.eof())
   				break;
  			}
  		}
 	}
    	mat2.close();
     	cout<<nrows2<<" "<<ncols2<<endl;

   	int **node= new int *[nrows2];
   // int node[nrows2][ncols2],new_adj[nrows2][ncols2]; 
    	ifstream mat4("Renum_0.7.dat",ios::in);
    	for ( int row=0; row<nrows2; row++)
  	{
     		node[row]= new int [ncols2];
   		for (int col=0; col<ncols2; col++)
   		{
     			mat4 >> node[row][col];
   		}
  	}
  
  
   	int l=0,m;
   
    	int k=0;
    	do
 	{
   
    	m=node[k][0];
    //cout<<m<<endl;
     	for( int j=0; j<N; j++)
  	{
     //cout<<m<<setw(5)<<j<<setw(5)<<G[m-1][j]<<endl;
   		if( mdArray[m-1][j] ==1) 
 		{
    			output<<m<<setw(10)<<j+1<<endl;
   		for( int d=0; d<nrows2; d++)
   		{
    			if( node[d][0] == j+1)
    			{
      			outfl<<node[k][1]<<"\t"<<node[d][1]<<endl;
      //cout<<node[k][1]<<"\t"<<node[d][1]<<endl;
      //rand[node[k][1]-1][node[d][1]-1]=1;
      			break;
   			}
     			else
   			{
     //rand[node[k][1]-1][node[d][1]-1]=0;
     				continue;
    			}
  		}

    
  		}
	}
      
   	k++;
 
 
   	}while( k<nrows2);

 

}
  
