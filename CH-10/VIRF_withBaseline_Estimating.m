% Program for VIRF with baseline caculation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VIRF based on research by Hafner(2001), which expand work of Hafner(1998)
% VIRF of VGARCH model based on BEKK model with baseline
% Code established by XUQIFA at 26th Oct. 2005.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear;clc;
%  读入已经保存的变量文件
load ResultForBEKK.mat;

% 作出波动率的图形
for i=1:498;
  Vol1(i)=Ht(1,1,i);
  Vol2(i)=Ht(2,2,i);
end
figure;
subplot(1,2,1);plot(Vol1);title('上证综指条件波动率估计');
subplot(1,2,2);plot(Vol2);title('深证成指条件波动率估计');

% 计算波动脉冲响应函数
T=20;                             % 脉冲响应时期
C=zeros(3,1);
C(1)=Cstar(1,1)^2;
C(2)=Cstar(1,1)*Cstar(2,1);
C(3)=Cstar(2,1)^2+Cstar(2,2)^2;
A1=BEKK2VGARCH(Astar);
B1=BEKK2VGARCH(Bstar);


InitKesai1=[-2:0.1:2]';
I=length(InitKesai1);
InitKesai2(I,1)=0;
InitKesai=[InitKesai1(:),InitKesai2(:)];       % 这里冲击是作了随机模拟,实际中可以以某个事实冲击为基础
vechSigma=inv(eye(Nstar)-A1-B1)*C;
Sigma=ivech(vechSigma);                        % 这里Sigma0取了平稳的Sigma,它还可以取在某个时刻的条件协方差矩阵

VIRFt(I,T,Nstar)=0;
DN=[1 0 0;0 1 0;0 1 0;0 0 1];
InvDN=pinv(DN);
for i=1:I;
    VIRFt(i,1,:)=A1*InvDN*kron(sqrtm(Sigma),sqrtm(Sigma))*DN*vech(InitKesai(i,:)*InitKesai(i,:)'-eye(N));
end

Evol(:,:,1)=eye(Nstar);
for i=1:I;
    for t=2:T;
        Evol(:,:,t)=Evol(:,:,t-1)*(A1+B1);               %  卷积(A1+B1)^t
        VIRFt(i,t,:)=Evol(:,:,t-1)*A1*InvDN*kron(sqrtm(Sigma),sqrtm(Sigma))*DN*vech(InitKesai(i,:)*InitKesai(i,:)'-eye(N));
    end
end

time=[1:1:T]';
figure;
subplot(3,1,1);mesh(time,InitKesai1,VIRFt(:,:,1));title('上证综指条件波动的脉冲响应');
subplot(3,1,2);mesh(time,InitKesai1,VIRFt(:,:,2));title('上证综指与深证成指条件协波动的脉冲响应');
subplot(3,1,3);mesh(time,InitKesai1,VIRFt(:,:,3));title('深证成指条件波动的脉冲响应');
