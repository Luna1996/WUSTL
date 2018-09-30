function [g,E,bias,var] = bias_var_exp (N,D)
rng('shuffle');
d=rand(N,2)*2-1;
x=linspace(-1,1,D);
f=zeros(1,D);
for i=1:D
	f(i)=x(i)^2;
end
y=zeros(N,D);
E=zeros(1,N);
for i=1:N
	for j=1:D
		y(i,j)=(d(i,1)+d(i,2))*x(j)-d(i,1)*d(i,2);
	end
	E(i)=mean((y(i,:)-f).^2);
end
E=mean(E);
g=mean(y,1);
bias=mean((g-f).^2);
var=zeros(1,N);
for i=1:N
	var(i)=mean((y(i,:)-g).^2);
end
var=mean(var);
plot(x,g);
hold on;
plot(x,f);
end