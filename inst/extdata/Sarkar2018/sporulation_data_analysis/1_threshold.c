/* ....... Identifies genes which surpass the threshold in a particular timepoint ..............*/ 

#include <iostream>
#include <string>
#include <fstream>

int main()
{
using namespace std;
	ifstream file1("cond.dat");            //fold change gene expression values of a timepoint
	ifstream file2("SK1_gene_names.txt");  //names of all genes in the same order as their fold change expression values
	ofstream outfile("Threshold_1.0.txt"); //names of genes that surpass the threshold
      	double myarray1[6926];
      	string myarray2[6926];

        	for(int i = 0; i < 6926; ++i)
        	{
             		file1>>myarray1[i];
        	}
		for(int i=0; i<6926; i++)
		{
			file2>>myarray2[i];
		}
		
		for(int i=0; i< 6926; i++)
          	{
              		int num=0; 
            		for(int j=0; j<172523; j++)
			{
				if(myarray1[i] > 1.0 || myarray1[i] <(-1.0))
                 		{
                			num +=1;
              			}
            			else
           			{
              				num+=0;
           			}
			}
               		if(num>0)
                	{                              
                                
                   		outfile<<myarray2[i]<<endl; //This file contains those genes which have >1.0 or <-1.0 expression value
				
                	}
              		else
               		{
                  		continue; 
               		}  
				
           	}
}
