function [ train_err, test_err ] = AdaBoost( X_tr, y_tr, X_te, y_te, n_trees )
%AdaBoost: Implement AdaBoost using decision stumps learned
%   using information gain as the weak learners.
%   X_tr: Training set
%   y_tr: Training set labels
%   X_te: Testing set
%   y_te: Testing set labels
%   n_trees: The number of trees to use
  N_tr=size(X_tr,1);
  I=1:N_tr;
  Tree=cell(n_trees,1);
  for b=1:n_trees
    D=datasample(I,N_tr);
    Tree{b}=fitctree(X_tr(D,:),Y_tr(D));
  end
end