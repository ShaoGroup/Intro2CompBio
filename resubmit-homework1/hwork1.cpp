#include <iostream>
#include <string.h>
#include <iomanip>
#include <stdio.h>
using namespace std;

int score_matrix[12000][12000];

int score;
int route[12000][12000];
int match(char seq1[],char seq2[],int i,int j,int alpha,int beta)
{
	int m = 0;
	if(seq1[j - 1] == seq2[i - 1]){
		m = alpha;
		}
	else{
		m = beta;
		}
		return m;
}
void align(char seq1[],char seq2[],int len1,int len2,int alpha,int beta,int gamma)
{
	int a = 0, b = 0,c = 0;
	int i,j;
	for(i = 0;i <= len1; i++){
	score_matrix[0][i] = 0 + i * gamma;
	route[0][i] = 1;
	}
	for(j = 0;j <= len2; j++)
	{
		score_matrix[j][0] = 0 + j * gamma;
		route[j][0] = 2;
	}
		score_matrix[0][0] = 0;
		route[0][0] = 0;
		for(i = 1; i <= len2; i++)
		{
			for(j = 1; j <= len1; j++)
			{ 
				//分数从三个数中的最大值选取
				a = score_matrix[i][j - 1] + gamma;
				b = score_matrix[i - 1][j] + gamma;
				c = score_matrix[i - 1][j - 1] + match(seq1,seq2,i,j,alpha,beta);
				if(a >= b && a >= c)
				{
					score_matrix[i][j] = a;
					route[i][j] = 1;
				}
					else if(b >= a && b >= c)
					{
						score_matrix[i][j] = b;
						route[i][j] = 2;
					}
						else
						{
							score_matrix[i][j] = c;
							route[i][j] = 3;
						}
			}
	    }
	    score=score_matrix[len2][len1];
}

//输出函数
void print_outline(string seq1,string seq2,int len1,int len2)
{
char ans1[12000];
char ans2[12000];
int j = len1;
int i = len2;
int m = 0;
int n = 0;
int ans1_point = len1;
int ans2_point = len2;

while(route[i][j] != 0)
{
if(route[i][j] == 3)
{
ans1[m] = seq1[ans1_point - 1];
m++;
ans1_point--;
ans2[n] = seq2[ans2_point - 1];
n++;
ans2_point--;
i--;
j--;
}
else if(route[i][j] == 2)
{
ans1[m] = '-';
m++;
ans2[n] = seq2[ans2_point - 1];
n++;
ans2_point--;
i--;
}
else
{
ans1[m] = seq1[ans1_point - 1];
m++;
ans1_point--;
ans2[n] = '-';
n++;
j--;
}
}
char fin1[100];
char fin2[100];

for(int w = 0;w<m;w++)
{
fin1[m-w-1] = ans1[w];
}
for(int w = 0;w<n;w++)
{
fin2[n-w-1] = ans2[w];
}
fin1[m] = '\0';
fin2[n] = '\0';
cout<<"seq1:"<<fin1<<endl;
cout<<"seq2:"<<fin2<<endl;
}


void output(char seq1[],char seq2[],int len1,int len2)
{
char ans[12000];
int j = len1;
int i = len2;
int t = 0;
int ans1_point = len1;
int ans2_point = len2;
while(route[i][j] != 0)
{
if(route[i][j] == 3)
{
ans[t] = seq1[ans1_point - 1];
ans1_point--;;
ans2_point--;
i--;
j--;
t++;
}
else if(route[i][j] == 2)
{
ans2_point--;
i--;
}
else
{
ans1_point--;
j--;
}
}
char fin[12000];
for(int w = 0; w<t;w++)
{
fin[t-w-1] = ans[w];
}
fin[t] = '\0';
cout<<"longest common subsequence:"<<fin<<endl;
cout<<"length:"<<t<<endl;
}
int main()
{
int alpha,beta,gamma;
char seq11[] = "ACC";
char seq12[] = "ACTC";
alpha=1;
beta=-1;
gamma=-1;
cout<<seq11<<endl;
cout<<seq12<<endl;
int len1 = sizeof(seq11);
int len2 = sizeof(seq12);
align(seq11,seq12,len1,len2,alpha,beta,gamma);
cout<<"score:"<<score<<endl;
print_outline(seq11,seq12,len1,len2);


char seq21[12000];
char seq22[12000];
FILE *Long_C = fopen("LongestCommonSeq.txt","r");
fgets(seq21,12000,Long_C);
fgets(seq22,12000,Long_C);
len1 = strlen(seq21);
len2 = strlen(seq22);
alpha = 1;
beta = 0;
gamma = 0;

align(seq21,seq22,len1,len2,alpha,beta,gamma);
output(seq21,seq22,len1,len2);
return 0;
}

//  reference:
//  http://blog.csdn.net/hugeheadhuge/article/details/6329558  中的Needleman-Wunsch 算法部分
