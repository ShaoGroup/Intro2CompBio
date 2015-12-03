#include <string>
#include <iostream>
#include <fstream>

using namespace std;

//This program is aimed to solve Question 2 in Homework 1,so it's actually a simplified version of global alignment,which means some parameters it uses are fixed

//return the maxium of three numbers
int max3(int a, int b, int c)
{
	int max1 = a > b ? a : b;
	int max2 = c > max1 ? c : max1;
	return max2;
}


int main()
{
	ifstream infile;
	ofstream outfile;
	string seq1="";
	string seq2="";

	//reading sequence
	infile.open("D://LongestCommonSeq.txt");
	getline(infile, seq1);
	getline(infile, seq2);
	//output the length of sequence
	if (seq1 != "" && seq2 != "")
		cout << "Read the file successfully" << endl << "sequence length: " << seq1.length() << "\t" << seq2.length() << endl;
	else
		return 0;

	int height = seq1.length() + 1;
	int width = seq2.length() + 1;
	int **data;
	int **path;
	data = new int*[height];
	path = new int*[height];
	int max = 0;
	int position_i;
	int position_j;
	int percent = height / 10 + 1;

	//Initialize the matrix
	for (int i = 0; i < height; i++)
	{
		data[i] = new int[width];
		path[i] = new int[width];
		data[i][0] = 0;
		path[i][0] = 2;
	}
	for (int j = 0; j < width; j++)
	{
		data[0][j] = 0;
		path[0][j] = 3;
	}
	path[0][0] = 0;

	cout << "Start filling~" << endl;

	//Filling the matrix
	for (int i = 1; i < height; i++)
	{
		for (int j = 1; j < width; j++)
		{
			path[i][j] = 0;
			data[i][j] = max3(data[i - 1][j - 1] + (seq1[i - 1] == seq2[j - 1] ? 1 : 0), data[i - 1][j], data[i][j - 1]);
			if (data[i][j] == data[i - 1][j - 1] + 1)
				path[i][j] = 1;
			if (data[i][j] == data[i - 1][j])
				path[i][j] = 2;
			if (data[i][j] == data[i][j - 1])
				path[i][j] = 3;
			if (data[i][j] == data[i - 1][j - 1])
				path[i][j] = 4;

			if (data[i][j] > max)
			{
				max = data[i][j];
				position_i = i;
				position_j = j;
			}
		}
		//Output the rate of progress
		if (i % percent == 0)
			cout << "Table has constructed " << (100 * i) / height << "%" << endl;
	}

	//output the maxium score
	cout << "Score: " << max << endl;
	string seq1_final = "";
	
	//build the common sequence using the path matrix
	while (position_i > 0 || position_j > 0)
	{
		if (path[position_i][position_j] == 1)
		{
			seq1_final += seq1[position_i - 1];
			position_i--;
			position_j--;
		}
		else if (path[position_i][position_j] == 2)
		{
			position_i--;
		}
		else if (path[position_i][position_j] == 3)
		{
			position_j--;
		}
		else if (path[position_i][position_j] == 4)
		{
			position_i--;
			position_j--;
		}
		if (path[position_i][position_j] == 0)
			break;
	}
	//output the actual length of the sequence for debug
	cout << "actual lengh:" << seq1_final.length() << endl;
	//output the common sequence
	cout << "Longest Common Word:";
	outfile.open("D://final.txt");
	for (int i = seq1_final.length() - 1; i >= 0; i--)
	{
		cout << seq1_final[i];
		outfile << seq1_final[i];
	}
	cout << endl;
	outfile << "\n";
	outfile.close();
}