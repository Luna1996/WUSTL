function [testErr] = TestBaggedTrees( X_tr, Y_tr, X_te, Y_te, numBags)
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
  N_tr=size(X_tr,1);
  I=1:N_tr;
  N_te=size(X_te,1);
  T=zeros(N_te,numBags);
  for b=1:numBags
    D=datasample(I,N_tr);
    f=fitctree(X_tr(D,:),Y_tr(D));
    T(:,b)=predict(f,X_te);
  end
  T=sign(sum(T,2));
  testErr=sum(T~=Y_te)/N_te;
end
