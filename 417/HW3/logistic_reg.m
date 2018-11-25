function [ w, e_in ] = logistic_reg( X, y, w_init, max_its, eta )
%LOGISTIC_REG Learn logistic regression model using gradient descent
%   Inputs:
%       X : data matrix (without an initial column of 1s)
%       y : data labels (plus or minus 1)
%       w_init: initial value of the w vector (d+1 dimensional)
%       max_its: maximum number of iterations to run for
%       eta: learning rate
    
%   Outputs:
%       w : weight vector
%       e_in : in-sample error (as defined in LFD)
	w = w_init;
	d = size(w,2);
	N = size(y,1);
	X = [ones(N,1), X];
	n = 0;
	while true
		g = zeros(1,d);
		for i=1:N
			g = g + y(i)*X(i,:)/(1+exp(y(i)*dot(X(i,:),w)));
		end
		g = g/N;
		w = w + eta/N*g;
		n = n + 1;
		if n == max_its || max(-g)<0.001
			break
		end
	end
	e_in = 0;
	for i=1:N
		e_in = e_in + log(1 + exp(y(i)*dot(w,X(i,:))));
	end
	e_in = e_in/N;
end

