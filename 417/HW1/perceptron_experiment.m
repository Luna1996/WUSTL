function [ num_iters, bounds] = perceptron_experiment ( N, d, num_samples )
%perceptron_experiment Code for running the perceptron experiment in HW1
%   Inputs: N is the number of training examples
%           d is the dimensionality of each example (before adding the 1)
%           num_samples is the number of times to repeat the experiment
%   Outputs: num_iters is the # of iterations PLA takes for each sample
%            bound_minus_ni is the difference between the theoretical bound
%               and the actual number of iterations
%      (both the outputs should be num_samples long)
rng('shuffle');
num_iters = zeros(1,num_samples);
bounds = zeros(1,num_samples);
v1 = ones(d,1);
v2 = ones(N,1);
for i=1:num_samples
	W = rand(d,1);
	X = rand(N,d)*2-1;
	Y = X * W;
	[~,it] = perceptron_learn([v2,X,sign(Y)]);
	num_iters(1,i) = it;
	bounds(1,i) = (max(X.^2 * v1)+1) * (W' * W) / min(Y.^2);
end
end