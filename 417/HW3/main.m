[y1, X1] = get_data('clevelandtrain.csv');
[y2, X2] = get_data('clevelandtest.csv');
w_init = zeros(1, size(X1,2)+1);

[w1,e_in1] = logistic_reg(X1, y1, w_init, 10000, 0.00001);
test_error1 = find_test_error(w1, X2, y2);
disp([e_in1, test_error1]);

[w2,e_in2] = logistic_reg(X1, y1, w_init, 100000, 0.00001);
test_error2 = find_test_error(w2, X2, y2);
disp([e_in2, test_error2]);

[w3,e_in3] = logistic_reg(X1, y1, w_init, 1000000, 0.00001);
test_error3 = find_test_error(w3, X2, y2);
disp([e_in3, test_error3]);

[w4,e_in4] = glmfit(X1, de2bi(y1+1), 'binomial','link','logit');
test_error4 = find_test_error(w4', X2, y2);
disp([e_in4, test_error4]);