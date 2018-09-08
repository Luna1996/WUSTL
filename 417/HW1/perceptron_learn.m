function[w, iterations] = perceptron_learn(data_in)
%perceptron_learn Run PLA on the input data
%   Inputs: data_in: Assumed to be a matrix with each row representing an
%                    (x,y) pair, with the x vector augmented with an
%                    initial 1, and the label (y) in the last column
%   Outputs: w: A weight vector (should linearly separate the data if it is
%               linearly separable)
%            iterations: The number of iterations the algorithm ran for
[N,d] = size(data_in);
w = zeros(1,d-1);
iterations = 0;

X = data_in(:,1:d-1);
Y = data_in(:,d);

while(1)
	flag = 1;
	for i = 1:N
		if(sign(X(i,:)*w')~=Y(i))
			flag = 0;
			iterations = iterations + 1;
			w = w+Y(i)*X(i,:);
			break;
		end
	end
	if(flag)
		return;
	end
end
end

