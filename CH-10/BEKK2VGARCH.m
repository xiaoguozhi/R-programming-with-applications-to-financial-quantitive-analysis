function out=BEKK2VGARCH(inp)
% 由BEKK模型的参数计算出VGARCH模型的参数值
% 输入矩阵为2*2矩阵,输出矩阵为3*3的矩阵
if length(inp)~=2
    warning('输入矩阵不符要求');
    return;
else
    out=zeros(3,3);
    out(1,1)=inp(1,1)^2;
    out(1,2)=2*inp(1,1)*inp(2,1);
    out(1,3)=inp(2,1)^2;
    out(2,1)=inp(1,1)*inp(1,2);
    out(2,2)=inp(2,1)*inp(1,2)+inp(1,1)*inp(2,2);
    out(2,3)=inp(2,1)*inp(2,2);
    out(3,1)=inp(1,2)^2;
    out(3,2)=2*inp(1,2)*inp(2,2);
    out(3,3)=inp(2,2)^2;
end
