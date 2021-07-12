function [] = myautocorr(y)
% Plots the sample autocorrelation function of y.
% Intended for use if Econometrics Toolbox is not available.
acf = zeros(21,1);
for lag = 0:20
    ybar = mean(y);
    acf(lag+1) = 1/length(y)*sum((y(1:end-lag,1)-ybar).*(y((lag+1):end,1)-ybar));
end
acf = acf/acf(1);
seacf = 1/sqrt(length(y));

figure
stem(0:20,acf,'color','r')
hold on
grid on
line([0.5,20],[2*seacf,2*seacf],'color','b')
line([0.5,20],[-2*seacf,-2*seacf],'color','b')
hold off
end

