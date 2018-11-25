function [y, X] = get_data(file)
	data = csvread(file,1,0);
	X = data(:,1:end-1);
	y = data(:,end)*2-1;
end