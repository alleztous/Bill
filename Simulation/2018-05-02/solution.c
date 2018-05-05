
#include<stdio.h>
#include<math.h>
#include<stdlib.h>
#include<ctime>

int sum1,sum2,sum3,sum;
int list[20],fn[28]={0};
int Notfind13=0; 

void InsertSort(int R[],int n)
{
	int i,j,temp;
	for(i=1;i<n;i++)
	{
		if(R[i]<R[i-1])
		{
			temp=R[i];
			j=i-1;
			do{
				R[j+1]=R[j];
				j--;
			}while(j>=0&&R[j]>temp);
			R[j+1]=temp;
		}
	}
} 

int flag()
{
	int i,j;
	for(j=0;j<10000000;j++)
	{
	for(i=0;i<20;i++)
	list[i]=rand()%80+1;	
	InsertSort(list,20);	

	sum1=0;sum2=0;
	for(i=0;i<6;i++)
	{
		sum1+=list[i];
		sum2+=list[i+6];
	}
	sum1=sum1%10;			
	sum2=sum2%10;
	sum3=0;
	for(i=12;i<20;i++)
	{
		sum3+=list[i];
	}
	sum3=sum3%10;
	sum=sum1+sum2+sum3;
	
	if(sum!=13) Notfind13++;
	while(Notfind13==20) return 1;
	fn[sum]++;
    }
}
int main()
{
	srand(time(0)); 
	int trytimes = 50;
	int i,j,k,Apear[100]={0};
	//scanf("%d",&trytimes); 
	if(flag())
	{
    for(k=0;k<trytimes;k++)		
	{
	for(j=0;j<10000000;j++)
	{
		
	for(i=0;i<20;i++)
	list[i]=rand()%80+1;	
	InsertSort(list,20);	

	sum1=0;sum2=0;
	for(i=0;i<6;i++)
	{
		sum1+=list[i];
		sum2+=list[i+6];
	}
	sum1=sum1%10;			
	sum2=sum2%10;
	sum3=0;
	for(i=12;i<20;i++)
	{
		sum3+=list[i];
	}
	sum3=sum3%10;
	sum=sum1+sum2+sum3;
	
	   if(sum==13) 
	   {
	    Apear[j]++;
	    break;
        }
    }
	}
    } 
    for(k=0;k<100;k++)
    {
    	printf("%d: %d\n",k+1,Apear[k]) ;
	}
	
	return 0;
}
