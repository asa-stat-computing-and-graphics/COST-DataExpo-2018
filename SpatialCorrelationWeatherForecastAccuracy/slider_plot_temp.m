function [] = slider_plot_temp(TempStruct)
% Plot different plots according to slider location.

lonmin = floor(min(cat(1,TempStruct(:).Lon)));
lonmax = ceil(max(cat(1,TempStruct(:).Lon)));
latmin = floor(min(cat(1,TempStruct(:).Lat)));
latmax = ceil(max(cat(1,TempStruct(:).Lat)));
errmin = floor(min(cat(1,TempStruct(:).Error)));
errmax = ceil(max(cat(1,TempStruct(:).Error)));

figure('units','pixels',...
    'position',[100 100 900 900],...
    'menubar','none',...
    'name','slider_plot',...
    'numbertitle','off',...
    'resize','off');
axes('unit','pix',...
    'position',[50 80 800 775]);
LN = scatter3(TempStruct(1).Lon,TempStruct(1).Lat,TempStruct(1).Error,'o');
xlim([lonmin,lonmax])
ylim([latmin,latmax])
zlim([errmin,errmax])
title(['Date of Forecast: ',datestr(TempStruct(1).DateOfForecast), ', Date Being Forecasted: ',datestr(TempStruct(1).DateBeingForecasted)]);
uicontrol('style','slide',...
    'unit','pix',...
    'position',[50 10 800 30],...
    'min',1,'max',length(TempStruct),'val',1,...
    'sliderstep',[1/(length(TempStruct)-1) 10/(length(TempStruct)-1)],...
    'callback',{@sl_call,TempStruct,LN});
rotate3d on

function [] = sl_call(varargin)
% Callback for the slider.
[h,S,LN] = varargin{[1,3,4]};  % calling handle and data structure.
set(LN,'xdata',S(round(get(h,'value'))).Lon)
set(LN,'ydata',S(round(get(h,'value'))).Lat)
set(LN,'zdata',S(round(get(h,'value'))).Error)
title(['Date of Forecast: ',datestr(S(round(get(h,'value'))).DateOfForecast), ', Date Being Forecasted: ',datestr(S(round(get(h,'value'))).DateBeingForecasted)]);
lonmin = floor(min(cat(1,S(:).Lon)));
lonmax = ceil(max(cat(1,S(:).Lon)));
latmin = floor(min(cat(1,S(:).Lat)));
latmax = ceil(max(cat(1,S(:).Lat)));
errmin = floor(min(cat(1,S(:).Error)));
errmax = ceil(max(cat(1,S(:).Error)));
xlim([lonmin,lonmax])
ylim([latmin,latmax])
zlim([errmin,errmax])