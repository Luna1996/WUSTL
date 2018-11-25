function [ oobErr ] = BaggedTrees( X, Y, numBags )
%BAGGEDTREES Returns out-of-bag classification error of an ensemble of
%numBags CART decision trees on the input dataset, and also plots the error
%as a function of the number of bags from 1 to numBags
%   Inputs:
%       X : Matrix of training data
%       Y : Vector of classes of the training examples
%       numBags : Number of trees to learn in the ensemble
%
%   You may use "fitctree" but do not use "TreeBagger" or any other inbuilt
%   bagging function
  N=size(X,1);
  I=1:N;
  T=zeros(N,numBags);
  for b=1:numBags
    D=datasample(I,N);
    tree=fitctree(X(D,:),Y(D));
    Dsub=setdiff(I,D);
    T(Dsub,b)=predict(tree,X(Dsub,:));
  end
  oob=zeros(numBags,1);
  for b=1:numBags
    oob(b)=sum(sign(sum(T(:,1:b),2))~=Y)/N;
  end
  figure;
  plot(1:numBags,oob);
  xlabel('numBags');
  ylabel('out-of-bag error')
  oobErr=oob(end);
end
