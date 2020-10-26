%% Main script for Functional Time Series Analysis for the Data Expo 2018 project
% Section 1 fits 2-D tensor splines to the weather data
% Section 2 constructs spatial basis functions and reduces dimension
% Section 3 fits AR-GARCH models to the random coefficient time series
% Section 4 provides visualizations of the results of the AR-GARCH fits
% Section 5 predicts next-day coefficients to improve weather forecasts

%% Section 1: Fit tensor product splines to weather data
% Load Data - If need to re-import from Excel, use MaxMinTempImportScript.m.
% Otherwise .mat file contains data for quick import.
load('MaxTempFormatted.mat')
load('TempLocationsFormatted.mat')

% Visualize Temperature Forecast Errors
slider_plot_temp(MaxTemp(:,end))

% Set Domain for Longitude and Latitude
LonMin = floor(min(cat(1,MaxTemp(:).Lon)));
LonMax = ceil(max(cat(1,MaxTemp(:).Lon)));
LatMin = floor(min(cat(1,MaxTemp(:).Lat)));
LatMax = ceil(max(cat(1,MaxTemp(:).Lat)));

% Set Basis Functions
addpath bspline
order = 4; % 4 = piecewise cubic
knotseqLon = [repmat(LonMin,[1,order-1]),linspace(LonMin,LonMax,15),repmat(LonMax,[1,order-1])];
knotseqLat = [repmat(LatMin,[1,order-1]),linspace(LatMin,LatMax,15),repmat(LatMax,[1,order-1])];

numBFLon = length(knotseqLon)-order;
numBFLat = length(knotseqLat)-order;
phiLon = @(LonVec)bspline_basismatrix(order,knotseqLon,LonVec);
phiLat = @(LatVec)bspline_basismatrix(order,knotseqLat,LatVec);
numBF = numBFLon*numBFLat;

% Plot Forecast Locations and B-Spline Knots
figure;
plot(MaxTemp(974,7).Lon,MaxTemp(974,7).Lat,'o')
xlabel('Longitude')
xlim([floor(min(MaxTemp(974,7).Lon))-1,ceil(max(MaxTemp(974,7).Lon))+1])
ylim([floor(min(MaxTemp(974,7).Lat))-1,ceil(max(MaxTemp(974,7).Lat))+1])
ylabel('Latitude')
ax1 = gca; % the first axes
ax2 = axes('Position',ax1.Position,...
  'XAxisLocation','bottom',...
  'YAxisLocation','left',...
  'Color','none',... 
  'Ylim',ax1.YLim,...
  'XLim',ax1.XLim,...
  'TickLength',[0 0],...
  'YTick', knotseqLat(order:end-order+1), ...
  'XTick', knotseqLon(order:end-order+1),  ...
  'YTickLabel', [],  ...
  'XTickLabel', []  );
linkaxes([ax1 ax2],'xy')
grid on
set(gcf,'CurrentAxes',ax1);

% Visualize B-splines
% LonVec = linspace(LonMin,LonMax,1000)';
% plot(LonVec,phiLon(LonVec))
% LatVec = linspace(LatMin,LatMax,1000)';
% plot(LatVec,phiLat(LatVec))

% Construct 2-D Tensor Product Basis
phi = @(LonVec,LatVec)sparse(kron(bspline_basismatrix(order,knotseqLon,LonVec),ones(1,numBFLat)).*kron(ones(1,numBFLon),bspline_basismatrix(order,knotseqLat,LatVec)));

% Compute coefficients for B-spline basis
timeTotal = 0;
n = size(MaxTemp,1);
minDate = min(datenum(cat(1,MaxTemp(:).DateBeingForecasted)));
maxDate = max(datenum(cat(1,MaxTemp(:).DateBeingForecasted)));
for colIdx = 1:7
    ModelFit(colIdx).NumDaysAhead = colIdx-1;
    ModelFit(colIdx).DatesBeingForecasted = datenum(cat(1,MaxTemp(:,colIdx).DateBeingForecasted));
    ModelFit(colIdx).NumForecasts = length(ModelFit(colIdx).DatesBeingForecasted);
    ModelFit(colIdx).MeanError = mean(cat(1,MaxTemp(:,colIdx).Error));
    ModelFit(colIdx).BSplineCoeff = zeros(ModelFit(colIdx).NumForecasts,numBF);
    for i = 1:ModelFit(colIdx).NumForecasts
        tic
        fprintf('Spline Fit %d for %d Days Ahead\n',i,colIdx-1)

        % Least squares using truncated SVD as pseudoinverse
        [U,S,V] = svds(phi(MaxTemp(i,colIdx).Lon,MaxTemp(i,colIdx).Lat),numBF);
        keepidx = cumsum(diag(S).^2)/sum(diag(S).^2)<=0.99;
        ModelFit(colIdx).BSplineCoeff(i,:) = V(:,keepidx)/S(keepidx,keepidx)*U(:,keepidx)'*sparse(MaxTemp(i,colIdx).Error - ModelFit(colIdx).MeanError);
        SplineApprox(i).InterpVal = phi(MaxTemp(i,colIdx).Lon,MaxTemp(i,colIdx).Lat)*ModelFit(colIdx).BSplineCoeff(i,:)';
        SplineApprox(i).ActualVal = MaxTemp(i,colIdx).Error;
        SplineApprox(i).InterpErr = SplineApprox(i).InterpVal-SplineApprox(i).ActualVal;
        SplineApprox(i).BSplinesUsed = sum(keepidx);
        timeTotal = timeTotal + toc;
    end
end

%% Section 2: Identify number of basis functions to reduce dimension to, and refit weather data to reduced basis

% Reduce dimension using principal components of B-spline coefficients
sigma2 = zeros(numBF,7);
for colIdx = 1:7
    ModelFit(colIdx).meanBSplineCoeff = mean(ModelFit(colIdx).BSplineCoeff)';
    [ModelFit(colIdx).BSplineU,ModelFit(colIdx).BSplineS,ModelFit(colIdx).BSplineV] = svd(ModelFit(colIdx).BSplineCoeff-repmat(ModelFit(colIdx).meanBSplineCoeff',[ModelFit(colIdx).NumForecasts,1]));
    sigma2(:,colIdx) = diag(ModelFit(colIdx).BSplineS).^2;
    ModelFit(colIdx).PctExplained = cumsum(sigma2(:,colIdx))./sum(sigma2(:,colIdx));
end

% Explained variance plot
plot(cumsum(sigma2)./repmat(sum(sigma2,1),numBF,1))

% Set number of basis functions to reduce to
numBFred = 20;
for colIdx = 1:7
    ModelFit(colIdx).meanfcn = @(LonVec,LatVec)ModelFit(colIdx).MeanError+phi(LonVec,LatVec)*ModelFit(colIdx).meanBSplineCoeff;
    ModelFit(colIdx).phi_reduced = @(LonVec,LatVec)phi(LonVec,LatVec)*ModelFit(colIdx).BSplineV(:,1:numBFred);
end
LonVec = linspace(LonMin,LonMax,250)';
LatVec = linspace(LatMin,LatMax,250)';
LonVecExt = kron(LonVec,ones(length(LatVec),1));
LatVecExt = kron(ones(length(LonVec),1),LatVec);
phi_reduced_values = ModelFit(7).phi_reduced(LonVecExt,LatVecExt);
slider_plot_tempBF(LonVec,LatVec,phi_reduced_values)

% Compute coefficients on restricted basis
timeTotal = 0;
colIdx = 7;
n = ModelFit(colIdx).NumForecasts;
ModelFit(colIdx).reducedCoeff = zeros(n,numBFred);
ModelFit(colIdx).residMatrix = nan(n,111);
ModelFit(colIdx).errorMatrix = nan(n,111);
ModelFit(colIdx).meanResid = zeros(n,1);
for i = 1:n
    fprintf('Reduced Fit %d for %d Days Ahead\n',i,colIdx-1)
    Lon = MaxTemp(i,colIdx).Lon;
    Lat = MaxTemp(i,colIdx).Lat;
    ModelFit(colIdx).reducedCoeff(i,:) = ModelFit(colIdx).phi_reduced(Lon,Lat)\(MaxTemp(i,colIdx).Error-ModelFit(colIdx).meanfcn(Lon,Lat));
    ModelFit(colIdx).residMatrix(i,MaxTemp(i,colIdx).CityNum) = (MaxTemp(i,colIdx).Error-ModelFit(colIdx).meanfcn(Lon,Lat)) - ModelFit(colIdx).phi_reduced(Lon,Lat)*ModelFit(colIdx).reducedCoeff(i,:)';
    ModelFit(colIdx).errorMatrix(i,MaxTemp(i,colIdx).CityNum) = MaxTemp(i,colIdx).Error;
    ModelFit(colIdx).meanResid(i) = mean(ModelFit(colIdx).residMatrix(i,MaxTemp(i,colIdx).CityNum));
end
ModelFit(colIdx).totalVar = zeros(111,1);
ModelFit(colIdx).residVar = zeros(111,1);
for i = 1:111
    ModelFit(colIdx).residVar(i) = var(ModelFit(colIdx).residMatrix(~isnan(ModelFit(colIdx).residMatrix(:,i)),i));
    ModelFit(colIdx).totalVar(i) = var(ModelFit(colIdx).errorMatrix(~isnan(ModelFit(colIdx).errorMatrix(:,i)),i));
end

%% Section 3: Fit AR(1)-GARCH(1,1) model on coefficients of reduced basis

% Fit AR-GARCH model on coefficients
model = arima('Constant',0,'ARLags',1,'Distribution','t','Variance',garch(1,1));
colIdx = 7;
ModelFit(colIdx).EstMdl = cell(numBFred,1);
for k = 1:numBFred
    [ModelFit(colIdx).EstMdl{k},ModelFit(colIdx).EstParamCov{k}] = estimate(model,ModelFit(colIdx).reducedCoeff(:,k));
end

% Compute results for plotting
n = ModelFit(colIdx).NumForecasts;
ModelFit(colIdx).arcoeff = zeros(numBFred,1);
ModelFit(colIdx).uk = zeros(n,numBFred);
ModelFit(colIdx).condVk = zeros(n,numBFred);
ModelFit(colIdx).DF = zeros(numBFred,1);
ModelFit(colIdx).logLk = zeros(numBFred,1);
for k = 1:numBFred
    ModelFit(colIdx).arcoeff(k,1) = ModelFit(colIdx).EstMdl{k}.AR{1};
    [ModelFit(colIdx).uk(:,k),~,ModelFit(colIdx).logLk] = infer(ModelFit(colIdx).EstMdl{k},ModelFit(colIdx).reducedCoeff(:,k));
    ModelFit(colIdx).DF(k) = ModelFit(colIdx).EstMdl{k}.Distribution.DoF;
    ModelFit(colIdx).condVk(:,k) = infer(ModelFit(colIdx).EstMdl{k}.Variance,ModelFit(colIdx).reducedCoeff(:,k));
end
ModelFit(colIdx).ukstd = ModelFit(colIdx).uk./sqrt(ModelFit(colIdx).condVk);

%% Section 4: Visualizations of results of AR-GARCH fit

% Visualize Variance Before and After
colIdx = 7;
plot([ModelFit(colIdx).totalVar,ModelFit(colIdx).residVar])
title(sprintf('Variance by City for %d-Day Ahead Forecasts',colIdx-1))
legend('Total Error','Residual')

% Visualize Variance Before and After Geographically
colIdx = 7;
figure(1)
scatter3(MaxTemp(1,1).Lon,MaxTemp(1,1).Lat,ModelFit(colIdx).residVar,[],ModelFit(colIdx).residVar);
title(sprintf('Geographic Plot of Residual Variance for %d-Day Ahead Forecasts',colIdx-1))
figure(2)
scatter3(MaxTemp(1,1).Lon,MaxTemp(1,1).Lat,ModelFit(colIdx).totalVar,[],ModelFit(colIdx).totalVar);
title(sprintf('Geographic Plot of Total Error Variance for %d-Day Ahead Forecasts',colIdx-1))

% Visualize Correlation Before and After
colIdx = 7;
figure(1)
surf(corr(ModelFit(colIdx).errorMatrix(~isnan(sum(ModelFit(colIdx).errorMatrix,2)),:)))
title(sprintf('Correlation of Forecast Errors for %d-Day Ahead Forecasts',colIdx-1))
xlabel('City')
ylabel('City')
figure(2)
surf(corr(ModelFit(colIdx).residMatrix(~isnan(sum(ModelFit(colIdx).residMatrix,2)),:)))
title(sprintf('Correlation of Residuals for %d-Day Ahead Forecasts',colIdx-1))
zlim([-0.5,1])
xlabel('City')
ylabel('City')

% Plot coefficients for restricted basis
colIdx = 7;
slider_plot_generic(ModelFit(colIdx).DatesBeingForecasted,ModelFit(colIdx).reducedCoeff,sprintf('%d-Day Ahead Coefficient',colIdx-1))

% Plot residuals not accounted for by basis functions
colIdx = 7;
slider_plot_tempresid(MaxTemp,colIdx,ModelFit(colIdx).residMatrix,locationsFormatted)

% Plot Residual for AR(1)-GARCH(1,1) Fit on scores
colIdx = 7;
slider_plot_generic(ModelFit(colIdx).DatesBeingForecasted,ModelFit(colIdx).uk,sprintf('u_{1t}',colIdx-1))

% Plot Standardized Residual for AR(1)-GARCH(1,1) Fit on scores
colIdx = 7;
slider_plot_generic(ModelFit(colIdx).DatesBeingForecasted,ModelFit(colIdx).ukstd,sprintf('u_{1t}/n_{1t}',colIdx-1))

% Plot ACF of Squared Innovations
colIdx = 7;
autocorr(ModelFit(colIdx).uk(:,1).^2)
title('ACF for Squared Innovations')
ylabel('Sample Autocorrelation of u_{1t}^2')

% Plot ACF of Squared Standardized Innovations
colIdx = 7;
autocorr(ModelFit(colIdx).ukstd(:,1).^2)
title('ACF for Squared Standardized Innovations')
ylabel('Sample Autocorrelation of  [u_{1t}/n_{1t}]^2')

% Correlogram before and after
AllLon = MaxTemp(975,7).Lon;
AllLat = MaxTemp(975,7).Lat;
distmat = sqrt((repmat(AllLon,[1,111])-repmat(AllLon',[111,1])).^2 + (repmat(AllLat,[1,111])-repmat(AllLat',[111,1])).^2);
errorMatrixFull = ModelFit(colIdx).errorMatrix(~isnan(sum(ModelFit(colIdx).errorMatrix,2)),:);
residMatrixFull = ModelFit(colIdx).residMatrix(~isnan(sum(ModelFit(colIdx).residMatrix,2)),:);
corrmatbefore = corr(errorMatrixFull);
corrmatafter = corr(residMatrixFull);
figure;
plot(distmat(:),corrmatbefore(:),'bo')
ylim([-0.6,1])
ylabel('Correlation')
xlabel('Distance')
figure;
plot(distmat(:),corrmatafter(:),'ro')
ylabel('Correlation')
xlabel('Distance')

% Compute sum of squared correlations for different numbers of basis
% functions to assess spatial correlation in residuals.

% This .mat file contains corr(residMatrixFull) for different numbers of basis
% functions, and can be reproduced by setting numBFred to 5,10,15,...,45 
% and re-running Section 2.
load('MaxTempCorrResidDifferentBFs.mat')
sumsqcorr = [sum(CorrResid0(:).^2); sum(CorrResid5(:).^2); ...
              sum(CorrResid10(:).^2); sum(CorrResid15(:).^2); ...
              sum(CorrResid20(:).^2); sum(CorrResid25(:).^2); ...
              sum(CorrResid30(:).^2); sum(CorrResid35(:).^2); ...
              sum(CorrResid40(:).^2); sum(CorrResid45(:).^2)];

figure;
xlabel('Number of Basis Functions (K)')
yyaxis left
plot([0;5;10;15;20;25;30;35;40;45],sumsqcorr)
hold on;
plot(20,sumsqcorr(5),'bs','MarkerSize',10,'MarkerFaceColor','b')
ylabel('Sum of Squared Residual Correlogram')
hold on;
yyaxis right
plot(0:45,[0;100*cumsum(sigma2(1:45,7))./repmat(sum(sigma2(:,7),1),45,1)],'r')
ylabel('Percentage of Explained Variance')
ytickformat('percentage')


%% Section 5: Predict next-day coefficients to improve weather forecasts
colIdx = 7;
predCoeff = ModelFit(colIdx).reducedCoeff(1:end-1,:).*repmat(ModelFit(colIdx).arcoeff',n-1,1);
predForecastAdjFcn = @(LonVec,LatVec,i)ModelFit(colIdx).meanfcn(LonVec,LatVec) + ModelFit(colIdx).phi_reduced(LonVec,LatVec)*predCoeff(i,:)';
for i = 1:n-1
    predTemp(i).DateBeingForecasted = MaxTemp(i+1,colIdx).DateBeingForecasted;
    predTemp(i).AdjustedForecast = MaxTemp(i+1,colIdx).ForecastedMax - predForecastAdjFcn(MaxTemp(i+1,colIdx).Lon,MaxTemp(i+1,colIdx).Lat,i);
    predTemp(i).Error = MaxTemp(i+1,colIdx).Error;
    predTemp(i).AdjustedError = predTemp(i).AdjustedForecast - MaxTemp(i+1,colIdx).ActualMax;
    predTemp(i).ErrorMean = mean(predTemp(i).Error);
    predTemp(i).AdjustedErrorMean = mean(predTemp(i).AdjustedError);
    predTemp(i).ErrorVar = var(predTemp(i).Error);
    predTemp(i).AdjustedErrorVar = var(predTemp(i).AdjustedError);
end

% Compare forecast error distribution before and after
errorbefore = cat(1,predTemp(:).Error);
errorafter = cat(1,predTemp(:).AdjustedError);
mean(errorbefore)
mean(errorafter)
std(errorbefore)
std(errorafter)

% Visualize distributions
minerr = floor(min([errorbefore;errorafter]));
maxerr = ceil(max([errorbefore;errorafter]));
figure
hist1 = histogram(errorbefore,minerr:maxerr,'EdgeColor', 'blue', 'FaceColor',  'blue');
hold on;
hist2 = histogram(errorafter,minerr:maxerr,'EdgeColor', 'green', 'FaceColor',  'green', 'FaceAlpha', 0.2);
hold off;
legend('Unadjusted (Y_t(\tau))','Adjusted (Z_t(\tau))')
title('Distribution of 6-Day Ahead Weather Forecast Errors')
ylabel('Count')
xlabel('Forecast Error')
xlim([-30,30])
grid on
