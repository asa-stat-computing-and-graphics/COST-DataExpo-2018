%% Import Script
% This script converts table data of forecast and actual temperatures into
% MATLAB structs suitable for analysis in the main.m script.

%% Max Temperature 
% The file MaxTempRaw.mat contains the table of maximum temperature
% forecasts paired with its respective actual temperature.

% Step 1: Import
load('MaxTempRaw.mat')

% Step 1a: Exclude Alaska and Hawaii
ALHI = MaxTempRaw.CityNum >= 112;
MaxTempRaw(ALHI,:) = [];

% Step 2: Extract index ranges for curves
CurIdx = [1; union(find(diff(datenum(MaxTempRaw.DateOfForecast)))+1, find(diff(datenum(MaxTempRaw.DateBeingForecasted)))+1)];
CurIdx(:,2) = [CurIdx(2:end,1)-1; size(MaxTempRaw,1)];

% Step 3: Construct struct for curves
ForecastDates = unique(MaxTempRaw.DateOfForecast);
numSurf = size(CurIdx,1);

for i = numSurf:-1:1
    DateOfForecast = MaxTempRaw.DateOfForecast(CurIdx(i,1));
    DateBeingForecasted = MaxTempRaw.DateBeingForecasted(CurIdx(i,1));
    ridx = find(datenum(ForecastDates)==datenum(DateOfForecast));
    cidx = datenum(DateBeingForecasted) - datenum(DateOfForecast) + 1;
    MaxTemp(ridx,cidx).DateOfForecast = datestr(DateOfForecast);
    MaxTemp(ridx,cidx).DateBeingForecasted = datestr(DateBeingForecasted);
    MaxTemp(ridx,cidx).DaysAhead = datenum(DateBeingForecasted) - datenum(DateOfForecast);
    MaxTemp(ridx,cidx).CityNum = MaxTempRaw.CityNum(CurIdx(i,1):CurIdx(i,2));
    MaxTemp(ridx,cidx).MissingCities = setdiff(1:113,MaxTemp(ridx,cidx).CityNum);
    MaxTemp(ridx,cidx).Lon = MaxTempRaw.Lon(CurIdx(i,1):CurIdx(i,2));
    MaxTemp(ridx,cidx).Lat = MaxTempRaw.Lat(CurIdx(i,1):CurIdx(i,2));
    MaxTemp(ridx,cidx).ForecastedMax = MaxTempRaw.ForecastedMax(CurIdx(i,1):CurIdx(i,2));
    MaxTemp(ridx,cidx).ActualMax = MaxTempRaw.ActualMax(CurIdx(i,1):CurIdx(i,2));
    MaxTemp(ridx,cidx).Error = MaxTempRaw.ForecastedMax(CurIdx(i,1):CurIdx(i,2)) - MaxTempRaw.ActualMax(CurIdx(i,1):CurIdx(i,2));
end
MaxTemp(:,8) = [];
clear ALHI MaxTempRaw CurIdx i numSurf ridx cidx DateOfForecast DateBeingForecasted ForecastDates

%% Min Temperature
% The file MinTempRaw.mat contains the table of minimum temperature
% forecasts paired with its respective actual temperature.

% Step 1: Import
load('MinTempRaw.mat')

% Step 1b: Exclude Alaska and Hawaii
ALHI = MinTempRaw.CityNum >= 112;
MinTempRaw(ALHI,:) = [];

% Step 2: Extract index ranges for curves
CurIdx = [1; union(find(diff(datenum(MinTempRaw.DateOfForecast)))+1, find(diff(datenum(MinTempRaw.DateBeingForecasted)))+1)];
CurIdx(:,2) = [CurIdx(2:end,1)-1; size(MinTempRaw,1)];

% Step 3: Construct struct for curves
ForecastDates = unique(MinTempRaw.DateOfForecast);
numSurf = size(CurIdx,1);

for i = numSurf:-1:1
    DateOfForecast = MinTempRaw.DateOfForecast(CurIdx(i,1));
    DateBeingForecasted = MinTempRaw.DateBeingForecasted(CurIdx(i,1));
    ridx = find(datenum(ForecastDates)==datenum(DateOfForecast));
    cidx = datenum(DateBeingForecasted) - datenum(DateOfForecast) + 1;
    MinTemp(ridx,cidx).DateOfForecast = datestr(DateOfForecast);
    MinTemp(ridx,cidx).DateBeingForecasted = datestr(DateBeingForecasted);
    MinTemp(ridx,cidx).DaysAhead = datenum(DateBeingForecasted) - datenum(DateOfForecast);
    MinTemp(ridx,cidx).CityNum = MinTempRaw.CityNum(CurIdx(i,1):CurIdx(i,2));
    MinTemp(ridx,cidx).MissingCities = setdiff(1:113,MinTemp(ridx,cidx).CityNum);
    MinTemp(ridx,cidx).Lon = MinTempRaw.Lon(CurIdx(i,1):CurIdx(i,2));
    MinTemp(ridx,cidx).Lat = MinTempRaw.Lat(CurIdx(i,1):CurIdx(i,2));
    MinTemp(ridx,cidx).ForecastedMin = MinTempRaw.ForecastedMin(CurIdx(i,1):CurIdx(i,2));
    MinTemp(ridx,cidx).ActualMin = MinTempRaw.ActualMin(CurIdx(i,1):CurIdx(i,2));
    MinTemp(ridx,cidx).Error = MinTempRaw.ForecastedMin(CurIdx(i,1):CurIdx(i,2)) - MinTempRaw.ActualMin(CurIdx(i,1):CurIdx(i,2));
end
clear ALHI MinTempRaw CurIdx i numSurf ridx cidx DateOfForecast DateBeingForecasted ForecastDates
