/* ............generates the list of interactions by overlaying the number of genes surpassing threshold on the base interactions obtained from SGD.............*/

#include <iostream>
#include <string>
#include <fstream>

int main()
{
using namespace std;
	ifstream file3("Base_final_left.txt");  //left column of base interactions file "Base_final_noselfloop_nodupedge"
	ifstream file4("Base_final_right.txt"); //right column of base interactions file "Base_final_noselfloop_nodupedge"
	ifstream infile("Threshold_1.0.txt");
	ofstream outf("Adjacency_1.0.txt");
      	string myarray3[172523];
	string myarray4[172523];
	string threshold[6926];
	int T;
	cout << "No. of genes beyond threshold" << endl;
	cin >> T;

		for(int i = 0; i < 172523; ++i)
        	{           
             		file3>>myarray3[i];             
        	}
		for(int i = 0; i < 172523; ++i)
        	{           
             		file4>>myarray4[i];             
        	}
		for(int i = 0; i < T; ++i)
        	{           
             		infile>>threshold[i];             
        	}

      
           	for(int i=0; i<T; i++)
            	{
               		for(int j=i+1; j<T; j++)
           	       	{
                      		for(int k=0; k<172523; k++)
                       		{                           
                          		if(threshold[i] ==myarray3[k])
                           		{
                                  		if(threshold[i] == myarray3[k] && threshold[j] == myarray4[k])
                                  		outf<<threshold[i]<<"\t"<<threshold[j]<<endl; // This file contains those pairs of genes which are both up(down)regulated
                                 	}
				}
			}
		}
}
		
