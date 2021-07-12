function [] = slider_plot_generic(DatesBeingForecasted,MatrixToPlot,Identifier)
% Plot different plots according to slider location.

datemin = min(DatesBeingForecasted);
datemax = max(DatesBeingForecasted);
numBFred = size(MatrixToPlot,2);
ymin = ceil(min(cat(1,MatrixToPlot(:))));
ymax = ceil(max(cat(1,MatrixToPlot(:))));

figure('units','normalized',...
    'position',[0.1 0.1 0.3 0.6],...
    'menubar','none',...
    'name','slider_plot',...
    'numbertitle','off',...
    'resize','on');
axes('unit','normalized',...
    'position',[0.1 0.2 0.8 0.7]);
LN = plot(DatesBeingForecasted,MatrixToPlot(:,1));
title([Identifier,' for Basis Function 1']);
uicontrol('style','slide',...
    'unit','normalized',...
    'position',[0.1 0.05 0.8 0.05],...
    'min',1,'max',numBFred,'val',1,...
    'sliderstep',[1/(numBFred-1) 10/(numBFred-1)],...
    'callback',{@sl_call,MatrixToPlot,Identifier,LN});
datetick('x','mmm-yyyy')
xlim([datemin,datemax])
xlabel('Date Being Forecasted')
ylim([ymin,ymax])
ylabel(Identifier)

function [] = sl_call(varargin)
% Callback for the slider.
[h,S,Identifier,LN] = varargin{[1,3,4,5]};  % calling handle and data structure.
set(LN,'ydata',S(:,round(get(h,'value'))))
title([Identifier,sprintf(' for Basis Function %d',round(get(h,'value')))])