#include <iostream>
#include <string>
#include <fstream>

using namespace std;

// GlobalAlign:
// Input: 2 strings, alpha, beta, gamma, q (question number)
// Output: Matching score, output an file of matching route OR longest common sequence

int GlobalAlign(string As, string Bs, int alpha, int beta, int gamma, int q)
{
	char ign = 0; // The value for representing vacant
	// Read data from string.
	int a = As.size();
	char * A = new char [a+1];
	for (int i = 0; i < a; i++)
    {
        A[i] = As[i];
    }
	int b = Bs.size();
	char * B = new char [b+1];
	for (int i = 0; i < b; i++)
    {
        B[i] = Bs[i];
    }

	// Initialize revenue matrix: Best revenue at each position.
	int ** M = new int * [a+1];
	for (int i = 0; i < a+1; i++)
		M[i] = new int [b+1];

	for (int i = 0; i < a+1; i++) 
	{
		M[i][0] = i*gamma;
	}

	for (int i = 0; i < b+1; i++) 
	{
		M[0][i] = i*gamma;
	}

	// Initialize pointer matrix: For backtracking the optimum route.
	int ** ptx = new int * [a+1];
	for (int i = 0; i < a+1; i++)
		ptx[i] = new int [b+1];

	int ** pty = new int * [a+1];
	for (int i = 0; i < a+1; i++)
		pty[i] = new int [b+1];


	for (int i = 1; i < a+1; i++)
	{
		ptx[i][0] = i-1;
		pty[i][0] = 0;
	}
	for (int i = 1; i < b+1; i++)
	{
		ptx[0][i] = 0;
		pty[0][i] = i-1;
	}
	ptx[0][0] = -1;
	pty[0][0] = -1;


	// Initialize representation matrix: Matrix for representing align.
	int ** MrepA = new int * [a+1];
	for (int i = 0; i < a+1; i++)
		MrepA[i] = new int [b+1];	

	int ** MrepB = new int * [a+1];
	for (int i = 0; i < a+1; i++)
		MrepB[i] = new int [b+1];	

	for (int i = 1; i < a+1; i++)
	{
		MrepA[i][0] = A[i-1];
		MrepB[i][0] = ign;
	}
	for (int i = 1; i < b+1; i++)
	{
		MrepA[0][i] = ign;
		MrepB[0][i] = B[i-1];
	}

	// Main part of algorithm: Fill in the revenue matrix.
	// The bracket wrapping a+1 is necessary.
	for (int i = 1; i < a+1; i++)
	{
		for (int j =1; j < b+1; j++)
		{
			int x, y, z;
			// calculate x,y,z
			if (A[i-1] == B[j-1]) 
			{
				x = M[i-1][j-1] + alpha;
			}
			else
			{
				x = M[i-1][j-1] + beta;
			}
			y = M[i-1][j] + gamma;
			z = M[i][j-1] + gamma;


			// Compare the values.
			if (y >=x && y >= z)
			{
				M[i][j] = y;
				ptx[i][j] = i-1;
				pty[i][j] = j;
				MrepA[i][j] = A[i-1];
				MrepB[i][j] = ign;
			}
			else if (z >= x  && z >= y)
			{
				M[i][j] = z;
				ptx[i][j] = i;
				pty[i][j] = j-1;
				MrepA[i][j] = ign;
				MrepB[i][j] = B[j-1];
			}
			else
			{
				M[i][j] = x;
				ptx[i][j] = i-1;
				pty[i][j] = j-1;
				MrepA[i][j] = A[i-1];
				MrepB[i][j] = B[j-1];
			}
		} // end j
	} // end i

	// Backtrack the optimum route.
	int pxn = a;
	int pyn = b;

	// Allocate enough space to speed up.
	int * px = new int [a+b+10];
	int * py = new int [a+b+10];

	int i = 0;
	px[i] = pxn;
	py[i] = pyn;
	while(pxn != 0 || pyn != 0)
	{
		i ++;
		// prevent change of pxn -> change of pyn
		int pxn1 = ptx[pxn][pyn];
		int pyn1 = pty[pxn][pyn];
		pxn = pxn1;
		pyn = pyn1;
		px[i] = pxn;
		py[i] = pyn;
	}
	int len = i+1;

	// Output sequence to file.
	if (q == 1)
	{
		FILE * f = fopen("Answer1.txt", "w");
		for (int i = len - 2; i >= 0; i--)
		{
			char output = MrepA[px[i]][py[i]];
			if (output == 0)
			{
				fprintf(f,"-");
			}
			else
			{
				fprintf(f,"%c",output);
			}
		}
	
		fprintf(f,"\n");

		for (int i = len - 2; i >= 0; i--)
		{
			char output = MrepB[px[i]][py[i]];
			if (output == 0)
			{
				fprintf(f,"-");
			}
			else
			{
				fprintf(f,"%c",output);
			}
		}

		fclose(f);	
	}
	else // if q == 2
	{
		FILE * f = fopen("Answer2.txt", "w");
		for (int i = len - 2; i >= 0; i--)
		{
			char output1 = MrepA[px[i]][py[i]];
			char output2 = MrepB[px[i]][py[i]];
			int same = output1*output2;
			if (same != 0)
				fprintf(f,"%c",output1);
		}
		fclose(f);
	}

	return(M[a][b]);
}

int main()
{
	// Question 1.
	int q = 1;

	// The value of parameters.
	int alpha = 1;
	int beta = -1;
	int gamma = -1;
	
	//Read data from file.
	string As;
	string Bs;
	ifstream myfile("Test1.txt");
	getline(myfile, As);
	getline(myfile, Bs);
	myfile.close();

	int score = GlobalAlign(As, Bs, alpha, beta, gamma, q);

	printf("Score1: %d",score);




	// Question 2.
	q = 2;

	// The value of parameters.
	alpha = 1;
	beta = 0;
	gamma = 0;
	
	//Read data from file.
	ifstream myfile2("LongestCommonSeq.txt");
	getline(myfile2, As);
	getline(myfile2, Bs);
	myfile2.close();

	score = GlobalAlign(As, Bs, alpha, beta, gamma, q);

	printf("\nScore2: %d",score);

	return 0;
}
