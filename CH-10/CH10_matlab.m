
clear;clc;
%  读入数据并计算收益序列
P=xlsread('CH-10-02.xls','sheet3');
for i=1:2;
    R(:,i)=diff(log(P(:,i)));             % 计算收益序列
end
T=length(R(:,1));
R=R(2:T,:);                                                  % 去掉滞后项中的NA码
T=length(R(:,1));

% % 计算描述统计量
% RMean=mean(R);
% RStd=std(R,0);
% RSkew=skewness(R);
% RKurt=kurtosis(R);
% for i=1:11;
%     [h(i),p(i),j(i)] = jbtest(R(:,i));
% end


%  进行BEKK模型参数估计
data=[R(:,1),R(:,2)];
p=1;q=1;
[parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores]  = full_bekk_mvgarch(data,p,q,[]);

% 还原模型的参数设置
N=size(data,2);
Nstar=N*(N+1)/2;
B0_star=ivech(parameters(1:Nstar));                              % 还原为矩阵形式
B1_star=reshape(parameters(Nstar+1:Nstar+N*N),N,N);
B2_star=reshape(parameters(Nstar+N*N+1:Nstar+2*N*N),N,N);

% 作出波动率的图形
for i=1:T;
  Vol1(i)=Ht(1,1,i);
  Vol2(i)=Ht(2,2,i);
end
figure;
subplot(1,2,1);plot(Vol1);title('上证综指条件波动率估计');
subplot(1,2,2);plot(Vol2);title('深证成指条件波动率估计');


B0=zeros(3,1);
B0(1)=B0_star(1,1)^2;
B0(2)=B0_star(1,1)*B0_star(2,1);
B0(3)=B0_star(2,1)^2+B0_star(2,2)^2;
B1=BEKK2VGARCH(B1_star);
B2=BEKK2VGARCH(B2_star);
[V,D]=eig(B1+B2)
% s=solve(' 0.6016+2*0.5767*x+0.5528*x^2=0')  %
% 样本区间2006-2010，具有协同持续性，存在三个单位根
s=solve('0.9991+2*0.0420*x+0.0021*x^2=0')   %
% 样本区间2006-2013，不具有协同持续性，只存在一个单位根，作为书稿例子
% s=solve(' 0.9988+2*-0.0488*x+0.0025*x^2=0') %
% 样本区间2007-2013，不具有协同持续性，存在一个单位根
%save ResultFor_CH10.2_ex10.9;
save F:\CH-10\Result B0 B1 B2 V D s

help  full_bekk_mvgarch
