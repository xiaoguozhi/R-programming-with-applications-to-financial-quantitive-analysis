function [B0, B1, B2, V, D] = solEngi(data, parameters)

N=size(data,2); %返回矩阵的列数
Nstar=N*(N+1)/2;
B0_star=ivech(parameters(1:Nstar));                              
B1_star=reshape(parameters(Nstar+1:Nstar+N*N),N,N);
B2_star=reshape(parameters(Nstar+N*N+1:Nstar+2*N*N),N,N);
 
B0=zeros(3,1);
B0(1)=B0_star(1,1)^2;
B0(2)=B0_star(1,1)*B0_star(2,1);
B0(3)=B0_star(2,1)^2+B0_star(2,2)^2;
B1=BEKK2VGARCH(B1_star);
B2=BEKK2VGARCH(B2_star);
[V,D]=eig(B1+B2);

% syms x;
% s = solve(V(1,1)+2*V(2,1)*x+V(3,1)*x^2);   %
% s = solve('0.9991+2*0.0420*x+0.0021*x^2=0');   %
