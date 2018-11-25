% Script to load data from zip.train, filter it into datasets with only one
% and three or three and five, and compare the performance of plain
% decision trees (cross-validated) and bagged ensembles (OOB error)
train=load('zip.train');
test=load('zip.test');

fprintf('Working on the one-vs-three problem...\n\n');
subsample = train(find(train(:,1)==1 | train(:,1) == 3),:);
Y_tr = subsample(:,1);
Y_tr = sign(Y_tr-2);
X_tr = subsample(:,2:257);
f = fitctree(X_tr,Y_tr);
Test = test(find(test(:,1)==1 | test(:,1) == 3),:);
Y_te=Test(:,1);
Y_te=sign(Y_te-2);
X_te=Test(:,2:257);
T = predict(f,X_te);
fprintf('The test error of decision trees is %.4f\n', sum(T~=Y_te)/size(X_te,1));
fprintf('The test error of 200 bagged decision trees is %.4f\n', TestBaggedTrees(X_tr, Y_tr, X_te, Y_te, 200));


fprintf('Working on the three-vs-five problem...\n\n');
subsample = train(find(train(:,1)==3 | train(:,1) == 5),:);
Y_tr = subsample(:,1);
Y_tr = sign(Y_tr-4);
X_tr = subsample(:,2:257);
f = fitctree(X_tr,Y_tr);
Test = test(find(test(:,1)==3 | test(:,1) == 5),:);
Y_te=Test(:,1);
Y_te=sign(Y_te-4);
X_te=Test(:,2:257);
T = predict(f,X_te);
fprintf('The test error of decision trees is %.4f\n', sum(T~=Y_te)/size(X_te,1));
fprintf('The test error of 200 bagged decision trees is %.4f\n', TestBaggedTrees(X_tr, Y_tr, X_te, Y_te, 200));